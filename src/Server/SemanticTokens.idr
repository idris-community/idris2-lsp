module Server.SemanticTokens

import Core.Metadata
import Data.List
import Language.LSP.Message
import Libraries.Data.PosMap
import Data.String
import Core.Context
import Server.Configuration
import Server.Capabilities
import Server.Log

||| encode using relative tokens according the to LSP spec
encode : FilePos -> List ASemanticDecoration -> List Int
encode _ [] = []
encode (relLine, relStartChar) (((_, (sl, sc), (el, ec)), decor, _) :: xs) =
  encoding ++ encode (sl, sc) xs
 where
  ||| Line, StartChar, Length, TokenType, TokenModifiers
  encoding : List Int
  encoding = [ sl - relLine
             , if sl == relLine then sc - relStartChar else sc
             , ec - sc
             , encodeDecorAsNum decor
             , 0]

||| Remove zero width tokens and split multiline tokens
processToken : (Int -> Int) -> ASemanticDecoration -> List ASemanticDecoration
processToken getLineLength orig@((fileName, (startLine, startChar), (endLine, endChar)), decoration, name) =
  if startLine >= endLine
     then if startChar >= endChar
             then []
             else [orig]
     else let
            lineLength = getLineLength startLine
            rest = processToken getLineLength ((fileName, (startLine+1, 0), (endLine, endChar)), decoration, name)
           in if lineLength == startChar
                 then rest
                 else ((fileName, (startLine, startChar), (startLine, lineLength)), decoration, name) :: rest

||| Write from last to current, poping of the stack when the end of a token is reached
||| current = Nothing means there a no remaining tokens except those on the stack
processStack : (last : FilePos) -> (current : Maybe FilePos) -> (stack : List ASemanticDecoration) -> (List ASemanticDecoration, List ASemanticDecoration)
processStack last _ [] = ([], [])
processStack last mcurrent orig@(((fileName, _, end), decoration, name)::rest) = case (mcurrent, maybe False (end >) mcurrent) of
  (Just current, True) => ([((fileName, last, current), decoration, name)], orig)
  _ => let (output, stack) = processStack end mcurrent rest in (((fileName, last, end), decoration, name) :: output, stack)

||| Remove all overlaping tokens
removeOverlap : (last : FilePos) -> (stack : List ASemanticDecoration) -> (next : List ASemanticDecoration) -> List ASemanticDecoration
removeOverlap last stack [] = fst $ processStack last Nothing stack
removeOverlap last stack (current@((fileName, start, end), decoration, name)::rest) =
  let (output, newStack) = processStack last (Just start) stack
  in output ++ removeOverlap start (current :: newStack) rest

||| Get the semantic tokens from the Metadata
export
getSemanticTokens : Ref LSPConf LSPConfiguration => Ref Ctxt Defs => Metadata -> (getLineLength : Int -> Int) -> Core SemanticTokens
getSemanticTokens md getLineLength = do
  logD SemanticTokens "Fetching semantic highlightning metadata"
  allSemHigh <- allSemanticHighlighting md
  let overlappingHighlightingList = toList allSemHigh
  logD SemanticTokens "Removing overlapping tokens"
  let multilineSemanticHighlightingList = removeOverlap (0, 0) [] $ overlappingHighlightingList
  logD SemanticTokens "Splitting multiline tokens"
  let singlelineSemanticHighlightingList = foldMap (processToken getLineLength) multilineSemanticHighlightingList
  logD SemanticTokens "Encoding semantic tokens"
  let encodedTokens = encode (0, 0) singlelineSemanticHighlightingList
  pure $ MkSemanticTokens Nothing encodedTokens

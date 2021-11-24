||| Common types and utility funtions for the LSP server implementation.
|||
||| (C) The Idris Community, 2021
module Server.Utils

import Core.Context
import Core.Core
import Core.FC
import Core.Name
import Core.Metadata
import Data.Bits
import Data.List
import Data.String
import Idris.Pretty
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Configuration
import System.File
import System

||| Gets a specific component of a reference, using the supplied projection.
export
gets : (l : label) -> Ref l a => (a -> b) -> Core b
gets l f = f <$> get l

export
uncons' : List a -> Maybe (a, List a)
uncons' [] = Nothing
uncons' (x :: xs) = Just (x, xs)

||| Reads a single header from an LSP message on the supplied file handle.
||| Headers end with the string "\r\n".
export
fGetHeader : (handle : File) -> Core (Either FileError String)
fGetHeader handle = do
  False <- coreLift $ fEOF handle
    | True => coreLift $ exitWith (ExitFailure 1)
  Right l <- coreLift $ fGetLine handle
    | Left err => pure $ Left err
  -- TODO: reading up to a string should probably be handled directly by the FFI primitive
  --       or at least in a more efficient way in Idris2
  if isSuffixOf "\r\n" l
     then pure $ Right l
     else (map (l ++)) <$> fGetHeader handle

-- From Language.JSON.Data
private
b16ToHexString : Bits16 -> String
b16ToHexString n =
  case n of
    0 => "0"
    1 => "1"
    2 => "2"
    3 => "3"
    4 => "4"
    5 => "5"
    6 => "6"
    7 => "7"
    8 => "8"
    9 => "9"
    10 => "A"
    11 => "B"
    12 => "C"
    13 => "D"
    14 => "E"
    15 => "F"
    other => assert_total $
               b16ToHexString (n `shiftR` fromNat 4) ++
               b16ToHexString (n .&. 15)

||| Pad a string with leading zeros, if
||| its length is less than 4, up to 4 symbols.
pad4 : String -> String
pad4 str =
  case length str of
    0 => "0000"
    1 => "000" ++ str
    2 => "00" ++ str
    3 => "0" ++ str
    _ => str

||| See https://en.wikipedia.org/wiki/UTF-16 for the algorithm.
||| Returns the codepoint value represented as a hex string,
||| if it encodes a symbol from the Basic Multilingual Plane.
||| Otherwise, returns the 16-bit surrogate pair, every element of which
||| is, in turn, represented as a hex string.
encodeCodepointH : Bits32 -> Either String (String, String)
encodeCodepointH x =
  case x <= 0xFFFF of
    -- Basic Multilingual Plane
    True => Left $ pad4 (b16ToHexString (cast x))
    --  One of the Supplementary Planes
    False =>
      let x' = x - 0x10000 in
      Right $
        ( pad4 (b16ToHexString (cast $ 0xD800 + (x' `shiftR` fromNat 10)))
        , pad4 (b16ToHexString (cast $ 0xDC00 + (x' .&. 0b1111111111))))

||| Encode an arbitrary unicode codepointby escaping it
||| as defined in
||| https://tools.ietf.org/id/draft-ietf-json-rfc4627bis-09.html#rfc.section.7
encodeCodepoint : Bits32 -> String
encodeCodepoint x =
  case encodeCodepointH x of
    Left w => "\\u" ++ w
    Right (w1, w2) => "\\u" ++ w1 ++ "\\u" ++ w2

||| Here we escape all wide characters (exceeding 8 bit width).
||| JSON spec doesn't seem to require that,
||| but at least some of the editors (e.g. Neovim) expect
||| wide characters escaped, otherwise refusing to work.
private
showChar : Char -> String
showChar c =
  case c of
       '\b' => "\\b"
       '\f' => "\\f"
       '\n' => "\\n"
       '\r' => "\\r"
       '\t' => "\\t"
       '\\' => "\\\\"
       '"'  => "\\\""
       c => if isControl c || c >= '\127'
               then encodeCodepoint (cast $ ord c)
               else singleton c

private
showString : String -> String
showString x = "\"" ++ concatMap showChar (unpack x) ++ "\""

export
stringify : JSON -> String
stringify JNull = "null"
stringify (JBoolean x) = if x then "true" else "false"
stringify (JNumber x) =
  let s = show x
   in if isSuffixOf ".0" s then substr 0 (length s `minus` 2) s else s
stringify (JString x) = showString x
stringify (JArray xs) = "[" ++ stringifyValues xs ++ "]"
  where
    stringifyValues : List JSON -> String
    stringifyValues [] = ""
    stringifyValues (x :: xs) =
      stringify x ++ if isNil xs then "" else "," ++ stringifyValues xs
stringify (JObject xs) = "{" ++ stringifyProps xs ++ "}"
  where
    stringifyProp : (String, JSON) -> String
    stringifyProp (key, value) = showString key ++ ":" ++ stringify value

    stringifyProps : List (String, JSON) -> String
    stringifyProps [] = ""
    stringifyProps (x :: xs) =
      stringifyProp x ++ if isNil xs then "" else "," ++ stringifyProps xs

export
findInTreeLoc' : FilePos -> FilePos -> PosMap (NonEmptyFC, a) -> List (NonEmptyFC, a)
findInTreeLoc' sp ep m = sortBy (\x, y => cmp (measure x) (measure y)) $ dominators (sp, ep) m
  where
    cmp : FileRange -> FileRange -> Ordering
    cmp ((sr1, sc1), (er1, ec1)) ((sr2, sc2), (er2, ec2)) =
      compare (er1 - sr1, ec1 - sc1) (er2 - sr2, ec2 - sc2)

export
findPointInTreeLoc' : FilePos -> PosMap (NonEmptyFC, a) -> List (NonEmptyFC, a)
findPointInTreeLoc' p = findInTreeLoc' p p

export
findInTreeLoc : FilePos -> FilePos -> PosMap (NonEmptyFC, a) -> Maybe (NonEmptyFC, a)
findInTreeLoc sp ep m = head' $ findInTreeLoc' sp ep m

export
findPointInTreeLoc : FilePos -> PosMap (NonEmptyFC, a) -> Maybe (NonEmptyFC, a)
findPointInTreeLoc p = findInTreeLoc p p

export
findInTree : FilePos -> FilePos -> PosMap (NonEmptyFC, a) -> Maybe a
findInTree sp ep m = snd <$> findInTreeLoc sp ep m

export
findPointInTree : FilePos -> PosMap (NonEmptyFC, a) -> Maybe a
findPointInTree p = findInTree p p

export
anyAt : (a -> Bool) -> a -> b -> Bool
anyAt p loc _ = p loc

export
anyWithName : Name -> (NonEmptyFC -> Bool) -> NonEmptyFC -> (Name, b) -> Bool
anyWithName name p loc (n, _) = p loc && name == n

export
Cast FilePos Position where
  cast (line, col) = MkPosition line col

export
Cast Position FilePos where
  cast (MkPosition line col) = (line, col)

export
Cast FileRange Range where
  cast (start, end) = MkRange (cast start) (cast end)

export
Cast Range FileRange where
  cast (MkRange start end) = (cast start, cast end)

export
Cast FC Range where
  cast (MkFC _ start end) = MkRange { start = cast start, end = cast end }
  cast (MkVirtualFC _ start end) = MkRange { start = cast start, end = cast end }
  cast EmptyFC = MkRange { start = MkPosition 0 0, end = MkPosition 0 0 }

export
Cast NonEmptyFC Range where
  cast (_, start, end) = MkRange { start = cast start, end = cast end }

public export
Measure (Range, a) where
  measure (r, _) = (cast r.start, cast r.end)

export
prettyTerm : IPTerm -> Doc IdrisAnn
prettyTerm = reAnnotate Syntax . Pretty.prettyTerm

export
searchCache : Ref LSPConf LSPConfiguration => Range -> IdrisAction -> Core (List CodeAction)
searchCache r type = do
  cache <- gets LSPConf cachedActions
  let inRange = dominators (cast r) cache
  pure $ concatMap (snd . snd) $ filter (\(_, t, _) => type == t) inRange

export
pathToURI : String -> URI
pathToURI path =
  MkURI { scheme    = "file"
        , authority = Just (MkURIAuthority Nothing "" Nothing)
        , path      = path
        , query     = ""
        , fragment  = ""
        }

export
toStart : FC -> FC
toStart (MkFC f _ _) = MkFC f (0, 0) (0, 0)
toStart (MkVirtualFC f _ _) = MkVirtualFC f (0, 0) (0, 0)
toStart EmptyFC = EmptyFC

export
Show Position where
  show (MkPosition line character) = "\{show line}:\{show character}"

export
Show Range where
  show (MkRange start end) = "\{show start} -- \{show end}"

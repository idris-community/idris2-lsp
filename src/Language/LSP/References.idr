module Language.LSP.References

import Core.Context
import Core.Core
import Core.Metadata
import Core.Name
import Core.Options
import Data.List
import Data.String
import Data.URI
import Language.LSP.Message
import Language.LSP.Utils
import Server.Configuration
import Server.Log
import Server.Utils
import System.Directory
import System.File
import System.Path

-- Characters that can appear inside an Idris2 identifier
isIdentChar : Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\'' || c == '!'

-- Find all column positions (0-indexed) where `needle` occurs in `lineChars`
-- with word boundaries on both sides.
findWordOccurrences : List Char -> List Char -> List Int
findWordOccurrences needle lineChars = go lineChars 0 Nothing
  where
    nlen : Nat
    nlen = length needle

    go : List Char -> Int -> Maybe Char -> List Int
    go []        _   _    = []
    go (c :: cs) col prev =
      let beforeOk  = maybe True (not . isIdentChar) prev
          matchHere = isPrefixOf needle (c :: cs)
          afterChar = case List.drop nlen (c :: cs) of
                        []       => Nothing
                        (x :: _) => Just x
          afterOk   = maybe True (not . isIdentChar) afterChar
          rest      = go cs (col + 1) (Just c)
      in if beforeOk && matchHere && afterOk
         then col :: rest
         else rest

-- Extract the unqualified name string from a Name
shortName : Name -> Maybe String
shortName (UN (Basic n)) = Just n
shortName (UN (Field n)) = Just n
shortName (MN n _)       = Just n
shortName (NS _ n)       = shortName n
shortName _              = Nothing

-- Collect all .idr and .lidr files recursively under `dir`
collectIdrFiles : String -> IO (List String)
collectIdrFiles dir = do
  Right entries <- listDir dir | Left _ => pure []
  concat <$> traverse (checkEntry dir) entries
  where
    isIdrFile : String -> Bool
    isIdrFile p = isSuffixOf ".idr" p || isSuffixOf ".lidr" p

    checkEntry : String -> String -> IO (List String)
    checkEntry base entry =
      let fullPath = base </> entry
      in do
        Right subEntries <- listDir fullPath
          | Left _ => pure (if isIdrFile fullPath then [fullPath] else [])
        -- listDir succeeded → it's a directory; recurse
        subFiles <- collectIdrFiles fullPath
        pure subFiles

-- Search a file's content for word-boundary occurrences of `needle`
-- Returns Location list (each occurrence = one Location)
-- Pair each line with its 0-based line number
withLineNums : List String -> List (Int, String)
withLineNums = go 0
  where
    go : Int -> List String -> List (Int, String)
    go _ []        = []
    go i (l :: ls) = (i, l) :: go (i + 1) ls

searchFileContent : DocumentURI -> String -> String -> List Location
searchFileContent fileUri needle content =
  let needleChars = unpack needle
      fileLines   = withLineNums (lines content)
      nlen        = cast (length needleChars)
  in concatMap (searchLine needleChars nlen) fileLines
  where
    searchLine : List Char -> Int -> (Int, String) -> List Location
    searchLine needleChars nlen (lineNum, lineStr) =
      map (\col => MkLocation fileUri
                    (MkRange (MkPosition lineNum col)
                             (MkPosition lineNum (col + nlen))))
          (findWordOccurrences needleChars (unpack lineStr))

-- Collect unique search roots from the LSP workspace configuration.
-- Prefers rootUri / workspaceFolders sent by the client; falls back to
-- the Idris2 working directory.
getSearchRoots : Ref LSPConf LSPConfiguration
              => Ref Ctxt Defs
              => Core (List String)
getSearchRoots = do
  defs <- get Ctxt
  let wdir = defs.options.dirs.working_dir
  conf <- gets LSPConf initialized
  case conf of
    Nothing => pure [wdir]
    Just params =>
      let rootPath  = case params.rootUri of
                        Here uri => Just (uriPathToSystemPath uri.path)
                        There _  => Nothing
          folders   = case params.workspaceFolders of
                        Just (Here fs) => map (\f => uriPathToSystemPath f.uri.path) fs
                        _              => []
          explicit  = mapMaybe id [rootPath] ++ folders
       in pure $ if null explicit then [wdir] else nub explicit

-- Search all .idr files under each of `rootDirs` for `needle`, skipping the
-- current file.  Files are deduplicated across roots before searching so that
-- overlapping workspace folders do not produce duplicate results.
findCrossFileRefs : List String -> String -> String -> Core (List Location)
findCrossFileRefs rootDirs needle skipUriStr = do
  allFiles <- coreLift $ do
    fileLists <- traverse collectIdrFiles rootDirs
    pure $ nub (concat fileLists)
  coreLift $ concat <$> traverse (searchOne needle skipUriStr) allFiles
  where
    searchOne : String -> String -> String -> IO (List Location)
    searchOne n skipStr path = do
      let rawUri = "file://\{systemPathToURIPath path}"
      let Right fileUri = escapeAndParseURI rawUri | Left _ => pure []
      if show fileUri == skipStr
        then pure []   -- current file already covered by metadata
        else do
          Right content <- readFile path | Left _ => pure []
          pure $ searchFileContent fileUri n content

export
findReferences : Ref Ctxt Defs
              => Ref MD Metadata
              => Ref LSPConf LSPConfiguration
              => ReferenceParams -> Core (List Location)
findReferences params = do
  logI FindReferences "Searching for \{show params.textDocument.uri}"
  Just (uri, _) <- gets LSPConf openFile
    | Nothing => logE FindReferences "No open file" >> pure []
  let True = uri == params.textDocument.uri
    | False => do
        logD FindReferences "Expected request for the currently opened file \{show uri}, instead received \{show params.textDocument.uri}"
        pure []

  let line = params.position.line
  let col  = params.position.character
  nameLocs <- gets MD nameLocMap
  let Just ((origin, _, _), name) = findPointInTreeLoc (line, col) nameLocs
    | Nothing => logD FindReferences "No name found at \{show line}:\{show col}}" >> pure []
  logI FindReferences "Found name \{show name}"

  -- Within-file results (accurate, from type-checker metadata)
  let inFile : List NonEmptyFC
      inFile = map fst $ filter (\((o, _, _), n) => o == origin && n == name) $ toList nameLocs
  let inFileRefs = map (\fc => MkLocation uri (cast {from = NonEmptyFC, to = Range} fc)) inFile
  logI FindReferences "Found \{show $ length inFileRefs} in-file references"

  -- Cross-file text search (approximate, word-boundary grep)
  case shortName name of
    Nothing  => pure inFileRefs
    Just sn  => do
      roots <- getSearchRoots
      logI FindReferences "Searching \{show (length roots)} root(s): \{show roots}"
      let skipStr = show uri
      crossRefs <- findCrossFileRefs roots sn skipStr
      logI FindReferences "Found \{show $ length crossRefs} cross-file references"
      pure (inFileRefs ++ crossRefs)

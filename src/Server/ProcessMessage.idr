||| Functions for the processing of received request and
||| notifications.
|||
||| (C) The Idris Community, 2021
module Server.ProcessMessage

import Core.Context
import Core.Core
import Core.Directory
import Core.InitPrimitives
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.List
import Data.List1
import Data.OneOf
import Data.SortedMap
import Data.SortedSet
import Data.String
import Idris.Doc.String
import Idris.Error
import Idris.IDEMode.Holes
import Idris.ModTree
import Idris.Package
import Idris.Pretty
import Idris.ProcessIdr
import Idris.REPL
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Language.JSON
import Language.LSP.BrowseNamespace
import Language.LSP.CodeAction
import Language.LSP.CodeAction.AddClause
import Language.LSP.CodeAction.CaseSplit
import Language.LSP.CodeAction.ExprSearch
import Language.LSP.CodeAction.GenerateDef
import Language.LSP.CodeAction.GenerateDefNext
import Language.LSP.CodeAction.Intro
import Language.LSP.CodeAction.MakeCase
import Language.LSP.CodeAction.MakeLemma
import Language.LSP.CodeAction.MakeWith
import Language.LSP.CodeAction.RefineHole
import Language.LSP.Completion.Handler
import Language.LSP.Completion.Info
import Language.LSP.Definition
import Language.LSP.DocumentHighlight
import Language.LSP.DocumentSymbol
import Language.LSP.Message
import Language.LSP.Message.DocumentFormatting
import Language.LSP.Metavars
import Language.LSP.SignatureHelp
import Language.LSP.VirtualDocument
import Language.LSP.Utils
import Language.LSP.Severity
import Libraries.Data.List.Extra
import Libraries.Data.PosMap
import Libraries.Data.String.Extra
import Libraries.Data.Version
import Libraries.Utils.Path
import Parser.Unlit
import Server.Capabilities
import Server.Configuration
import Server.Diagnostics
import Server.Log
import Server.QuickFix
import Server.Response
import Server.SemanticTokens
import Server.Utils
import System
import System.Clock
import System.Directory
import System.File

||| Mostly copied from Idris.REPL.displayResult.
partial
replResultToDoc : Ref Ctxt Defs
               => Ref UST UState
               => Ref Syn SyntaxInfo
               => Ref MD Metadata
               => Ref ROpts REPLOpts
               => REPLResult -> Core (Doc IdrisAnn)
replResultToDoc (REPLError err) = pure err
replResultToDoc (Evaluated x Nothing) = pure (prettyTerm x)
replResultToDoc (Evaluated x (Just y)) = pure (prettyTerm x <++> colon <++> code (prettyTerm y))
replResultToDoc (Printed xs) = pure xs
replResultToDoc (TermChecked x y) = pure (prettyTerm x <++> colon <++> code (prettyTerm y))
replResultToDoc (FileLoaded x) = pure (reflow "Loaded file" <++> pretty0 x)
replResultToDoc (ModuleLoaded x) = pure (reflow "Imported module" <++> pretty0 x)
replResultToDoc (ErrorLoadingModule x err) = pure $ reflow "Error loading module" <++> pretty0 x <+> colon <++> !(perror err)
replResultToDoc (ErrorLoadingFile x err) = pure (reflow "Error loading file" <++> pretty0 x <+> colon <++> pretty0 (show err))
replResultToDoc (ErrorsBuildingFile x errs) = pure (reflow "Error(s) building file" <++> pretty0 x) -- messages already displayed while building
replResultToDoc NoFileLoaded = pure (reflow "No file can be reloaded")
replResultToDoc (CurrentDirectory dir) = pure (reflow "Current working directory is" <++> dquotes (pretty0 dir))
replResultToDoc CompilationFailed = pure (reflow "Compilation failed")
replResultToDoc (Compiled f) = pure (pretty0 "File" <++> pretty0 f <++> pretty0 "written")
replResultToDoc (ProofFound x) = pure (prettyTerm x)
replResultToDoc (Missed cases) = pure $ vsep (handleMissing <$> cases)
replResultToDoc (CheckedTotal xs) = pure (vsep (map (\(fn, tot) => pretty0 fn <++> pretty0 "is" <++> pretty0 tot) xs))
replResultToDoc (LogLevelSet Nothing) = pure (reflow "Logging turned off")
replResultToDoc (LogLevelSet (Just k)) = pure (reflow "Set log level to \{show k}")
replResultToDoc (ConsoleWidthSet (Just k)) = pure (reflow "Set consolewidth to \{show k}")
replResultToDoc (ConsoleWidthSet Nothing) = pure (reflow "Set consolewidth to auto")
replResultToDoc (ColorSet b) = pure (reflow (if b then "Set color on" else "Set color off"))
replResultToDoc (VersionIs x) = pure (pretty0 (showVersion True x))
replResultToDoc (RequestedHelp) = pure (pretty0 displayHelp)
replResultToDoc (RequestedDetails details) = pure (pretty0 details)
replResultToDoc (Edited (DisplayEdit Empty)) = pure (pretty0 "")
replResultToDoc (Edited (DisplayEdit xs)) = pure xs
replResultToDoc (Edited (EditError x)) = pure x
replResultToDoc (Edited (MadeLemma lit name pty pappstr)) = pure $ pretty0 (relit lit (show name ++ " : " ++ show pty ++ "\n") ++ pappstr)
replResultToDoc (Edited (MadeWith lit wapp)) = pure $ pretty0 $ showSep "\n" (map (relit lit) wapp)
replResultToDoc (Edited (MadeCase lit cstr)) = pure $ pretty0 $ showSep "\n" (map (relit lit) cstr)
replResultToDoc (Edited (MadeIntro is)) = pure $ pretty0 $ showSep "\n" (toList is)
replResultToDoc (OptionsSet opts) = pure (vsep (pretty0 <$> opts))
replResultToDoc Done = pure ""
replResultToDoc (Executed _) = pure ""
replResultToDoc DefDeclared = pure ""
replResultToDoc Exited = pure ""
-- Would be nice to preserve colours in the future, but
-- sending ansi codes over the socket is a probably a no-go.
replResultToDoc (PrintedDoc doc) = pure (unAnnotate doc)

displayType : Ref Ctxt Defs
           => Ref Syn SyntaxInfo
           => Defs -> (Name, Int, GlobalDef) -> Core (Doc IdrisAnn)
displayType defs (n, i, gdef) =
  maybe (do tm <- resugar [] =<< normaliseHoles defs [] (type gdef)
            pure (pretty0 !(aliasName (fullname gdef)) <++> colon <++> prettyTerm tm))
        (\num => reAnnotate Syntax <$> prettyHole defs [] n num (type gdef))
        (isHole gdef)

displayTerm : Ref Ctxt Defs
           => Ref Syn SyntaxInfo
           => Defs -> ClosedTerm -> Core (Doc IdrisAnn)
displayTerm defs = map prettyTerm . (resugar [] <=< normaliseHoles defs [])

processSettings : Ref Ctxt Defs => Ref LSPConf LSPConfiguration => JSON -> Core ()
processSettings (JObject xs) = do
  case lookup "logFile" xs of
       Just (JString fname) => changeLogFile fname
       Just _ => logE Configuration "Incorrect type for log file location, expected string"
       Nothing => pure ()
  case lookup "longActionTimeout" xs of
       Just (JNumber v) => do
         setSearchTimeout (cast v)
         logI Configuration "Long action timeout set to \{show $ cast {to = Int} v}"
       Just _ => logE Configuration "Incorrect type for long action timeout, expected number"
       Nothing => pure ()
  case lookup "maxCodeActionResults" xs of
       Just (JNumber v) => do
         update LSPConf ({ searchLimit := integerToNat (cast v) })
         logI Configuration "Max code action results set to \{show $ cast {to = Int} v}"
       Just _ => logE Configuration "Incorrect type for max code action results, expected number"
       Nothing => pure ()
  case lookup "showImplicits" xs of
       Just (JBoolean b) => do
         pp <- getPPrint
         when (pp.showImplicits /= b) $ do
           setPPrint ({ showImplicits := b } pp)
           update LSPConf ({ cachedHovers := empty })
         logI Configuration "Show implicits set to \{show b}"
       Just _ => logE Configuration "Incorrect type for show implicits, expected boolean"
       Nothing => pure ()
  case lookup "fullNamespace" xs of
       Just (JBoolean b) => do
         pp <- getPPrint
         when (pp.fullNamespace /= b) $ do
           setPPrint ({ fullNamespace := b } pp)
           update LSPConf ({ cachedHovers := empty })
         logI Configuration "Full namespace set to \{show b}"
       Just _ => logE Configuration "Incorrect type for full namespace, expected boolean"
       Nothing => pure ()
  case lookup "showMachineNames" xs of
       Just (JBoolean b) => do
         pp <- getPPrint
         when (pp.showMachineNames /= b) $ do
           setPPrint ({ showMachineNames := b } pp)
           update LSPConf ({ cachedHovers := empty })
       Just _ => logE Configuration "Incorrect type for show machine names, expected boolean"
       Nothing => pure ()
  case lookup "logSeverity" xs of
       Just (JString ll) =>
         whenJust (parseSeverity ll) $ \l => update LSPConf ({ logSeverity := l})
       Just _ => logE Configuration "Incorrect type for log severity, expected string"
       Nothing => pure ()
  case lookup "briefCompletions" xs of
      Just (JBoolean b) => update LSPConf ({ briefCompletions := b})
      _ => pure ()
processSettings _ = logE Configuration "Incorrect type for options"

isDirty : Ref LSPConf LSPConfiguration => DocumentURI -> Core Bool
isDirty uri = gets LSPConf (contains uri . dirtyFiles)

isError : Ref LSPConf LSPConfiguration => DocumentURI -> Core Bool
isError uri = gets LSPConf (contains uri . errorFiles)

loadURI : Ref LSPConf LSPConfiguration
       => Ref Ctxt Defs
       => Ref UST UState
       => Ref Syn SyntaxInfo
       => Ref MD Metadata
       => Ref ROpts REPLOpts
       => InitializeParams -> URI -> Maybe Int -> Core (Either String ())
loadURI conf uri version = do
  logI Server "Loading file \{show uri}"
  defs <- get Ctxt
  let extraDirs = defs.options.dirs.extra_dirs
  update ROpts { evalResultName := Nothing }
  resetContext (Virtual Interactive)
  let fpath = uriPathToSystemPath uri.path
  let Just (startFolder, startFile) = splitParent fpath
    | Nothing => do let msg = "Cannot find the parent folder for \{show uri}"
                    logE Server msg
                    pure $ Left msg
  True <- coreLift $ changeDir startFolder
    | False => do let msg = "Cannot change current directory to \{show startFolder}, folder of \{show startFile}"
                  logE Server msg
                  pure $ Left msg
  Right fname <- catch (maybe (Left "Cannot find the ipkg file") Right <$> findIpkg (Just fpath))
                       (pure . Left . show)
    | Left err => do let msg = "Cannot load ipkg file for \{show uri}: \{show err}"
                     logE Server msg
                     pure $ Left msg
  Right res <- coreLift $ File.ReadWrite.readFile fname
    | Left err => do let msg = "Cannot read file at \{show uri}"
                     logE Server msg
                     pure $ Left msg
  update LSPConf ({ virtualDocuments $= insert uri (fromMaybe 0 version, res ++ "\n") })
  --     A hack to solve some interesting edge-cases around missing newlines ^^^^^^^
  setSource res
  errs <- catch
            (buildDeps fname)
            (\err => do
              logE Server "Caught error which shouldn't be leaked while loading file: \{show err}"
              pure [err])
  -- FIXME: the compiler always dumps the errors on stdout, requires
  --        a compiler change.
  -- NOTE on catch: It seems the compiler sometimes throws errors instead
  -- of accumulating them in the buildDeps.
  unless (null errs) $ do
    update LSPConf ({ errorFiles $= insert uri })
    -- ModTree 397--308 loads data into context from ttf/ttm if no errors
    -- In case of error, we reprocess fname to populate metadata and syntax
    logI Channel "Rebuild \{fname} due to errors"
    modIdent <- ctxtPathToNS fname
    let msgPrefix : Doc IdrisAnn = pretty0 ""
    let buildMsg : Doc IdrisAnn = pretty0 modIdent
    clearCtxt; addPrimitives
    put MD (initMetadata (PhysicalIdrSrc modIdent))
    ignore $ ProcessIdr.process msgPrefix buildMsg fname modIdent

  let caps = (publishDiagnostics <=< textDocument) . capabilities $ conf
  update LSPConf ({ quickfixes := [], cachedActions := empty, cachedHovers := empty })
  traverse_ (findQuickfix caps uri) errs
  defs <- get Ctxt
  session <- getSession
  let warnings = if session.warningsAsErrors then [] else reverse (warnings defs)
  sendDiagnostics caps uri version warnings errs
  defs <- get Ctxt
  put Ctxt ({ options->dirs->extra_dirs := extraDirs } defs)
  cNames <- completionNames
  update LSPConf ({completionCache $= insert uri cNames})
  update LSPConf ({ openFile := Just (uri, fromMaybe 0 version) })
  pure $ Right ()

loadIfNeeded : Ref LSPConf LSPConfiguration
            => Ref Ctxt Defs
            => Ref UST UState
            => Ref Syn SyntaxInfo
            => Ref MD Metadata
            => Ref ROpts REPLOpts
            => InitializeParams -> URI -> Maybe Int -> Core (Either String ())
loadIfNeeded conf uri version = do
  Just (oldUri, oldVersion) <- gets LSPConf openFile
    | Nothing => loadURI conf uri version
  if (oldUri == uri && (isNothing version || (Just oldVersion) == version))
     then pure $ Right ()
     else loadURI conf uri version

withURI : Ref LSPConf LSPConfiguration
       => Ref Ctxt Defs
       => Ref UST UState
       => Ref Syn SyntaxInfo
       => Ref MD Metadata
       => Ref ROpts REPLOpts
       => InitializeParams
       -> URI -> Maybe Int -> Core (Either ResponseError a) -> Core (Either ResponseError a) -> Core (Either ResponseError a)
withURI conf uri version d k = do
  when !(isError uri) $ ignore $ logW Server "Trying to load \{show uri} which has errors" >> d
  case !(loadIfNeeded conf uri version) of
       Right () => k
       Left err => do
         logE Server "Error while loading \{show uri}: \{show err}"
         pure $ Left (MkResponseError (Custom 3) err JNull)

||| Guard for requests that requires a successful initialization before being allowed.
whenInitializedRequest : Ref LSPConf LSPConfiguration => (InitializeParams -> Core (Either ResponseError a)) -> Core (Either ResponseError a)
whenInitializedRequest k =
  case !(gets LSPConf initialized) of
       Just conf => k conf
       Nothing => do logE Server "Cannot process requests before initalization"
                     pure $ Left $ serverNotInitialized

||| Guard for requests that cannot be sent after the shutdown protocol.
whenNotShutdownRequest : Ref LSPConf LSPConfiguration => Core (Either ResponseError a) -> Core (Either ResponseError a)
whenNotShutdownRequest k =
  if !(gets LSPConf isShutdown)
     then do logE Server "Cannot process requests after shutdown"
             pure $ Left $ invalidRequest "Server has been shutdown"
     else k

||| whenInitializedRequest + whenNotShutdownRequest
whenActiveRequest : Ref LSPConf LSPConfiguration => (InitializeParams -> Core (Either ResponseError a)) -> Core (Either ResponseError a)
whenActiveRequest = whenNotShutdownRequest . whenInitializedRequest

||| Guard for notifications that requires a successful initialization before being allowed.
whenInitializedNotification : Ref LSPConf LSPConfiguration => (InitializeParams -> Core ()) -> Core ()
whenInitializedNotification k =
  case !(gets LSPConf initialized) of
       Just conf => k conf
       Nothing => do logE Server "Cannot process notification before initialization"
                     sendUnknownResponseMessage $ serverNotInitialized

||| Guard for notifications that cannot be sent after the shutdown protocol.
whenNotShutdownNotification : Ref LSPConf LSPConfiguration => Core () -> Core ()
whenNotShutdownNotification k =
  if !(gets LSPConf isShutdown)
     then do logE Server "Cannot process notifications after shutdown"
             sendUnknownResponseMessage $ invalidRequest "Server has been shutdown"
     else k

||| whenInitializedNotification + whenNotShutdownNotification
whenActiveNotification : Ref LSPConf LSPConfiguration => (InitializeParams -> Core ()) -> Core ()
whenActiveNotification = whenNotShutdownNotification . whenInitializedNotification

||| Format Idris2 source code.
formatIdrisSourceImpl : (doStructural : Bool) -> FormattingOptions -> String -> Bool -> String
formatIdrisSourceImpl doStructural options src isLiterate =
  -- Helper: Normalize line endings (Windows \r\n and old Mac \r to Unix \n)
  let goLine : List Char -> List Char
      goLine [] = []
      goLine ('\r' :: '\n' :: xs) = '\n' :: goLine xs  -- CRLF -> LF
      goLine ('\r' :: xs) = '\n' :: goLine xs  -- CR -> LF
      goLine (c :: xs) = c :: goLine xs
  
      normalizedSrc = pack $ goLine (unpack src)
      lineList : List String
      lineList = lines normalizedSrc
      
      -- Step 2: Convert tabs to spaces if insertSpaces is true
      tabSize : Nat
      tabSize = cast options.tabSize
      
      convertIndent : String -> String
      convertIndent line = 
        if options.insertSpaces
           then pack $ goTab (unpack line)
           else line
        where
          goTab : List Char -> List Char
          goTab [] = []
          goTab ('\t' :: xs) = replicate tabSize ' ' ++ goTab xs
          goTab (c :: xs) = c :: goTab xs
      
      -- Literate Idris: Only process lines starting with '>'
      processLiterate : String -> String
      processLiterate line =
        if isPrefixOf ">" line
           then ">" ++ processLine (substr 1 (length line) line)
           else line
        where
          processLine : String -> String
          processLine l =
            let withIndent = convertIndent l
                withTrim = if options.trimTrailingWhitespace == Just True
                              then trimTrailing withIndent
                              else withIndent
             in withTrim
          where
            trimTrailing : String -> String
            trimTrailing = pack . reverse . dropWhile isSpace . reverse . unpack
      
      passedIndent : List String
      passedIndent = if isLiterate 
                      then map processLiterate lineList
                      else map convertIndent lineList
      
      -- Step 3: Trim trailing whitespace from each line if option is set
      trimTrailingWS : String -> String
      trimTrailingWS = pack . reverse . dropWhile isSpace . reverse . unpack

      -- Collapse multiple consecutive interior spaces to one space.
      -- Preserves leading indentation. Skips string literals and -- comments.
      collapseInteriorSpaces : String -> String
      collapseInteriorSpaces line =
        let chars   = unpack line
            leading = takeWhile isSpace chars
            rest    = dropWhile isSpace chars
         in pack (leading ++ collapseContent rest False)
        where
          collapseContent : List Char -> Bool -> List Char
          collapseContent [] _ = []
          collapseContent ('"' :: xs) inStr = '"' :: collapseContent xs (not inStr)
          collapseContent ('\\' :: c :: xs) True = '\\' :: c :: collapseContent xs True
          collapseContent ('-' :: '-' :: xs) False = '-' :: '-' :: xs
          collapseContent (c :: xs) True = c :: collapseContent xs True
          collapseContent (' ' :: ' ' :: xs) False = collapseContent (' ' :: xs) False
          collapseContent (c :: xs) False = c :: collapseContent xs False

      -- Ensure a space after commas (outside strings/char literals/comments).
      -- Skips `,)`, `,]`, `,}` and already-spaced `, `.
      -- Tracks both "..." strings and '...' char literals to avoid mangling them.
      spaceAfterCommas : String -> String
      spaceAfterCommas line = pack $ go (unpack line) False False
        where
          -- inStr: inside "..." | inChar: inside '...'
          go : List Char -> (inStr : Bool) -> (inChar : Bool) -> List Char
          go [] _ _ = []
          -- String literal toggle
          go ('"' :: xs) False inChar = '"' :: go xs True inChar
          go ('"' :: xs) True  inChar = '"' :: go xs False inChar
          go ('\\' :: c :: xs) True inChar = '\\' :: c :: go xs True inChar
          -- Char literal: open, escape inside, close
          go ('\'' :: xs) False False = '\'' :: go xs False True
          go ('\\' :: c :: xs) False True = '\\' :: c :: go xs False True
          go ('\'' :: xs) False True  = '\'' :: go xs False False
          -- Line comment: pass rest verbatim (normal mode only)
          go ('-' :: '-' :: xs) False False = '-' :: '-' :: xs
          -- Inside string or char: pass verbatim
          go (c :: xs) True  inChar = c :: go xs True inChar
          go (c :: xs) False True   = c :: go xs False True
          -- Comma in normal mode
          go (',' :: c :: xs) False False =
            if c == ' ' || c == ')' || c == ']' || c == '}'
               then ',' :: go (c :: xs) False False
               else ',' :: ' ' :: go (c :: xs) False False
          go (',' :: []) False False = [',']
          go (c :: xs) inStr inChar = c :: go xs inStr inChar

      -- Ensure a single space after `--` in line comments.
      -- Leaves `---` dividers and `--|` doc-style comments untouched.
      spaceAfterLineComment : String -> String
      spaceAfterLineComment line = pack $ go (unpack line) False
        where
          fixComment : List Char -> List Char
          fixComment [] = []
          fixComment (' ' :: xs) = ' ' :: xs   -- already spaced
          fixComment ('-' :: xs) = '-' :: xs   -- divider ---
          fixComment ('|' :: xs) = '|' :: xs   -- doc-style --|
          fixComment cs          = ' ' :: cs   -- add space

          go : List Char -> Bool -> List Char
          go [] _ = []
          go ('"' :: xs) inStr = '"' :: go xs (not inStr)
          go ('\\' :: c :: xs) True = '\\' :: c :: go xs True
          go (c :: xs) True = c :: go xs True
          go ('-' :: '-' :: rest) False = '-' :: '-' :: fixComment rest
          go (c :: xs) False = c :: go xs False

      -- Ensure a space before and after ':' in type annotations.
      -- Skips '::' (cons), ':=' (record update), already-spaced ': '.
      -- Skips inside strings, char literals, and line comments.
      spaceAroundColon : String -> String
      spaceAroundColon line = pack $ go (unpack line) False False Nothing
        where
          go : List Char -> (inStr : Bool) -> (inChar : Bool) -> (prev : Maybe Char) -> List Char
          go [] _ _ _ = []
          go ('"' :: xs) False inChar _ = '"' :: go xs True  inChar (Just '"')
          go ('"' :: xs) True  inChar _ = '"' :: go xs False inChar (Just '"')
          go ('\\' :: c :: xs) True inChar _ = '\\' :: c :: go xs True inChar (Just c)
          go ('\'' :: xs) False False _ = '\'' :: go xs False True  (Just '\'')
          go ('\\' :: c :: xs) False True _ = '\\' :: c :: go xs False True (Just c)
          go ('\'' :: xs) False True  _ = '\'' :: go xs False False (Just '\'')
          go ('-' :: '-' :: xs) False False _ = '-' :: '-' :: xs
          go (c :: xs) True  inChar _ = c :: go xs True  inChar (Just c)
          go (c :: xs) False True   _ = c :: go xs False True  (Just c)
          -- Skip :: (cons) and := (record update)
          go (':' :: ':' :: xs) False False _ = ':' :: ':' :: go xs False False (Just ':')
          go (':' :: '=' :: xs) False False _ = ':' :: '=' :: go xs False False (Just '=')
          -- Single colon: ensure space before (from prev) and space after
          go (':' :: rest) False False prev =
            let before = case prev of
                           Just c  => if c == ' ' then [] else [' ']
                           Nothing => []
                after  = case rest of
                           (' ' :: _) => []
                           []         => []
                           _          => [' ']
             in before ++ [':'] ++ after ++ go rest False False (Just ':')
          go (c :: xs) inStr inChar _ = c :: go xs inStr inChar (Just c)

      -- Ensure a space before and after '->' and '=>'.
      -- Skips inside strings, char literals, and line comments.
      -- Note: '--' is already caught as a line comment before '->' can match.
      spaceAroundArrows : String -> String
      spaceAroundArrows line = pack $ go (unpack line) False False Nothing
        where
          addSpaces : List Char -> Maybe Char -> List Char -> List Char -> List Char
          addSpaces token prev rest continue =
            let before = case prev of
                           Just c  => if c == ' ' then [] else [' ']
                           Nothing => []
                after  = case rest of
                           (' ' :: _) => []
                           []         => []
                           _          => [' ']
             in before ++ token ++ after ++ continue

          go : List Char -> (inStr : Bool) -> (inChar : Bool) -> (prev : Maybe Char) -> List Char
          go [] _ _ _ = []
          go ('"' :: xs) False inChar _ = '"' :: go xs True  inChar (Just '"')
          go ('"' :: xs) True  inChar _ = '"' :: go xs False inChar (Just '"')
          go ('\\' :: c :: xs) True inChar _ = '\\' :: c :: go xs True inChar (Just c)
          go ('\'' :: xs) False False _ = '\'' :: go xs False True  (Just '\'')
          go ('\\' :: c :: xs) False True _ = '\\' :: c :: go xs False True (Just c)
          go ('\'' :: xs) False True  _ = '\'' :: go xs False False (Just '\'')
          go ('-' :: '-' :: xs) False False _ = '-' :: '-' :: xs
          go (c :: xs) True  inChar _ = c :: go xs True  inChar (Just c)
          go (c :: xs) False True   _ = c :: go xs False True  (Just c)
          -- -> arrow
          go ('-' :: '>' :: rest) False False prev =
            addSpaces ['-', '>'] prev rest (go rest False False (Just '>'))
          -- => arrow
          go ('=' :: '>' :: rest) False False prev =
            addSpaces ['=', '>'] prev rest (go rest False False (Just '>'))
          go (c :: xs) inStr inChar _ = c :: go xs inStr inChar (Just c)

      -- Ensure spaces around '=' and '=='.
      -- Skips: =>, <=, >=, /=, := (multi-char operators using =).
      -- Skips inside strings, char literals, and line comments.
      spaceAroundEquals : String -> String
      spaceAroundEquals line = pack $ go (unpack line) False False Nothing
        where
          addSpaces : List Char -> Maybe Char -> List Char -> List Char -> List Char
          addSpaces token prev rest continue =
            let before = case prev of
                           Just c  => if c == ' ' then [] else [' ']
                           Nothing => []
                after  = case rest of
                           (' ' :: _) => []
                           []         => []
                           _          => [' ']
             in before ++ token ++ after ++ continue

          -- Is this char one that forms a multi-char operator with '='?
          isOpChar : Char -> Bool
          isOpChar c = c == '<' || c == '>' || c == '/' || c == ':' || c == '='

          go : List Char -> (inStr : Bool) -> (inChar : Bool) -> (prev : Maybe Char) -> List Char
          go [] _ _ _ = []
          go ('"' :: xs) False inChar _ = '"' :: go xs True  inChar (Just '"')
          go ('"' :: xs) True  inChar _ = '"' :: go xs False inChar (Just '"')
          go ('\\' :: c :: xs) True inChar _ = '\\' :: c :: go xs True inChar (Just c)
          go ('\'' :: xs) False False _ = '\'' :: go xs False True  (Just '\'')
          go ('\\' :: c :: xs) False True _ = '\\' :: c :: go xs False True (Just c)
          go ('\'' :: xs) False True  _ = '\'' :: go xs False False (Just '\'')
          go ('-' :: '-' :: xs) False False _ = '-' :: '-' :: xs
          go (c :: xs) True  inChar _ = c :: go xs True  inChar (Just c)
          go (c :: xs) False True   _ = c :: go xs False True  (Just c)
          -- == : handle as unit before single =
          go ('=' :: '=' :: rest) False False prev =
            addSpaces ['=', '='] prev rest (go rest False False (Just '='))
          -- = followed by > (=>) or preceded by op char: skip
          go ('=' :: '>' :: rest) False False prev =
            '=' :: '>' :: go rest False False (Just '>')
          go ('=' :: rest) False False (Just prev) =
            if isOpChar prev
               then '=' :: go rest False False (Just '=')  -- part of <=, >=, /=, :=
               else addSpaces ['='] (Just prev) rest (go rest False False (Just '='))
          go ('=' :: rest) False False Nothing =
            addSpaces ['='] Nothing rest (go rest False False (Just '='))
          go (c :: xs) inStr inChar _ = c :: go xs inStr inChar (Just c)

      -- Ensure spaces around '|', '||', and '<-'.
      -- '|||' doc comments are passed verbatim.
      -- Skips inside strings, char literals, and line comments.
      spaceAroundPipeAndBind : String -> String
      spaceAroundPipeAndBind line = pack $ go (unpack line) False False Nothing
        where
          addSpaces : List Char -> Maybe Char -> List Char -> List Char -> List Char
          addSpaces token prev rest continue =
            let before = case prev of
                           Just c  => if c == ' ' then [] else [' ']
                           Nothing => []
                after  = case rest of
                           (' ' :: _) => []
                           []         => []
                           _          => [' ']
             in before ++ token ++ after ++ continue

          go : List Char -> (inStr : Bool) -> (inChar : Bool) -> (prev : Maybe Char) -> List Char
          go [] _ _ _ = []
          go ('"' :: xs) False inChar _ = '"' :: go xs True  inChar (Just '"')
          go ('"' :: xs) True  inChar _ = '"' :: go xs False inChar (Just '"')
          go ('\\' :: c :: xs) True inChar _ = '\\' :: c :: go xs True inChar (Just c)
          go ('\'' :: xs) False False _ = '\'' :: go xs False True  (Just '\'')
          go ('\\' :: c :: xs) False True _ = '\\' :: c :: go xs False True (Just c)
          go ('\'' :: xs) False True  _ = '\'' :: go xs False False (Just '\'')
          go ('-' :: '-' :: xs) False False _ = '-' :: '-' :: xs
          go (c :: xs) True  inChar _ = c :: go xs True  inChar (Just c)
          go (c :: xs) False True   _ = c :: go xs False True  (Just c)
          -- ||| doc comment: pass rest verbatim
          go ('|' :: '|' :: '|' :: xs) False False _ = '|' :: '|' :: '|' :: xs
          -- || logical or
          go ('|' :: '|' :: rest) False False prev =
            addSpaces ['|', '|'] prev rest (go rest False False (Just '|'))
          -- single |
          go ('|' :: rest) False False prev =
            addSpaces ['|'] prev rest (go rest False False (Just '|'))
          -- <- do-notation bind
          go ('<' :: '-' :: rest) False False prev =
            addSpaces ['<', '-'] prev rest (go rest False False (Just '-'))
          -- && logical and
          go ('&' :: '&' :: rest) False False prev =
            addSpaces ['&', '&'] prev rest (go rest False False (Just '&'))
          -- ++ concatenation
          go ('+' :: '+' :: rest) False False prev =
            addSpaces ['+', '+'] prev rest (go rest False False (Just '+'))
          go (c :: xs) inStr inChar _ = c :: go xs inStr inChar (Just c)

      -- Ensure a space after '{' and before '}' in record literals/updates.
      -- Skips '{}' (empty), '{-' (block comment start), and '-}' (block comment end).
      -- Skips inside strings, char literals, and line comments.
      spaceInsideBraces : String -> String
      spaceInsideBraces line = pack $ go (unpack line) False False Nothing
        where
          go : List Char -> (inStr : Bool) -> (inChar : Bool) -> (prev : Maybe Char) -> List Char
          go [] _ _ _ = []
          go ('"' :: xs) False ic _ = '"' :: go xs True  ic (Just '"')
          go ('"' :: xs) True  ic _ = '"' :: go xs False ic (Just '"')
          go ('\\' :: c :: xs) True ic _ = '\\' :: c :: go xs True ic (Just c)
          go ('\'' :: xs) False False _ = '\'' :: go xs False True  (Just '\'')
          go ('\\' :: c :: xs) False True _ = '\\' :: c :: go xs False True (Just c)
          go ('\'' :: xs) False True  _ = '\'' :: go xs False False (Just '\'')
          go ('-' :: '-' :: xs) False False _ = '-' :: '-' :: xs
          go (c :: xs) True  ic _ = c :: go xs True  ic (Just c)
          go (c :: xs) False True  _ = c :: go xs False True  (Just c)
          -- '{-' block comment start: no space
          go ('{' :: '-' :: rest) False False _ =
            '{' :: '-' :: go rest False False (Just '-')
          -- '{}' empty braces: no space
          go ('{' :: '}' :: rest) False False _ =
            '{' :: '}' :: go rest False False (Just '}')
          -- '{' followed by space: already spaced, skip the space to avoid double
          go ('{' :: ' ' :: rest) False False _ =
            '{' :: ' ' :: go rest False False (Just ' ')
          -- '{' followed by other: add space
          go ('{' :: rest) False False _ =
            '{' :: ' ' :: go rest False False (Just '{')
          -- '}': add space before unless already spaced or end of block comment '-}'
          go ('}' :: rest) False False prev =
            let needSpace = case prev of
                              Just ' ' => False
                              Just '-' => False  -- '-}' block comment end
                              Nothing  => False
                              _        => True
             in (if needSpace then [' '] else []) ++ ('}' :: go rest False False (Just '}'))
          go (c :: xs) is ic _ = c :: go xs is ic (Just c)

      -- Ensure spaces around $ (function application operator).
      -- Skips inside strings, char literals, and line comments.
      spaceAroundDollar : String -> String
      spaceAroundDollar line = pack $ go (unpack line) False False Nothing
        where
          addSpaces : Maybe Char -> List Char -> List Char -> List Char
          addSpaces prev rest continue =
            let before = case prev of { Just ' ' => []; Nothing => []; _ => [' '] }
                after  = case rest  of { (' ' :: _) => []; [] => []; _ => [' '] }
             in before ++ ['$'] ++ after ++ continue

          go : List Char -> (inStr : Bool) -> (inChar : Bool) -> (prev : Maybe Char) -> List Char
          go [] _ _ _ = []
          go ('"' :: xs) False ic _ = '"' :: go xs True  ic (Just '"')
          go ('"' :: xs) True  ic _ = '"' :: go xs False ic (Just '"')
          go ('\\' :: c :: xs) True ic _ = '\\' :: c :: go xs True ic (Just c)
          go ('\'' :: xs) False False _ = '\'' :: go xs False True  (Just '\'')
          go ('\\' :: c :: xs) False True _ = '\\' :: c :: go xs False True (Just c)
          go ('\'' :: xs) False True  _ = '\'' :: go xs False False (Just '\'')
          go ('-' :: '-' :: xs) False False _ = '-' :: '-' :: xs
          go (c :: xs) True  ic _ = c :: go xs True  ic (Just c)
          go (c :: xs) False True  _ = c :: go xs False True  (Just c)
          go ('$' :: rest) False False prev =
            addSpaces prev rest (go rest False False (Just '$'))
          go (c :: xs) is ic _ = c :: go xs is ic (Just c)

      -- Ensure spaces around arithmetic and comparison operators: +, *, /, <, >.
      -- Multi-char sequences already handled upstream (++, ->, <-, =>, <=, >=) are
      -- passed through unchanged. - is skipped (unary/binary ambiguity).
      -- Skips inside strings, char literals, and line comments.
      spaceAroundArithmetic : String -> String
      spaceAroundArithmetic line = pack $ go (unpack line) False False Nothing
        where
          addSpaces : List Char -> Maybe Char -> List Char -> List Char -> List Char
          addSpaces token prev rest continue =
            let before = case prev of { Just ' ' => []; Nothing => []; _ => [' '] }
                after  = case rest  of { (' ' :: _) => []; [] => []; _ => [' '] }
             in before ++ token ++ after ++ continue

          go : List Char -> (inStr : Bool) -> (inChar : Bool) -> (prev : Maybe Char) -> List Char
          go [] _ _ _ = []
          go ('"' :: xs) False ic _ = '"' :: go xs True  ic (Just '"')
          go ('"' :: xs) True  ic _ = '"' :: go xs False ic (Just '"')
          go ('\\' :: c :: xs) True ic _ = '\\' :: c :: go xs True ic (Just c)
          go ('\'' :: xs) False False _ = '\'' :: go xs False True  (Just '\'')
          go ('\\' :: c :: xs) False True _ = '\\' :: c :: go xs False True (Just c)
          go ('\'' :: xs) False True  _ = '\'' :: go xs False False (Just '\'')
          go ('-' :: '-' :: xs) False False _ = '-' :: '-' :: xs
          go (c :: xs) True  ic _ = c :: go xs True  ic (Just c)
          go (c :: xs) False True  _ = c :: go xs False True  (Just c)
          -- ++ already handled upstream; pass through
          go ('+' :: '+' :: rest) False False _ = '+' :: '+' :: go rest False False (Just '+')
          -- + single
          go ('+' :: rest) False False prev =
            addSpaces ['+'] prev rest (go rest False False (Just '+'))
          -- - is skipped: unary vs binary is ambiguous (e.g. -1 vs x-y)
          -- * single
          go ('*' :: rest) False False prev =
            addSpaces ['*'] prev rest (go rest False False (Just '*'))
          -- / single; pass through /= unchanged
          go ('/' :: '=' :: rest) False False _ = '/' :: '=' :: go rest False False (Just '=')
          go ('/' :: rest) False False prev =
            addSpaces ['/'] prev rest (go rest False False (Just '/'))
          -- <- already spaced upstream; pass through so < is not re-spaced
          go ('<' :: '-' :: rest) False False _ = '<' :: '-' :: go rest False False (Just '-')
          -- <= comparison: add spaces around the pair
          go ('<' :: '=' :: rest) False False prev =
            addSpaces ['<', '='] prev rest (go rest False False (Just '='))
          -- << shift: pass through
          go ('<' :: '<' :: rest) False False _ = '<' :: '<' :: go rest False False (Just '<')
          -- < single comparison
          go ('<' :: rest) False False prev =
            addSpaces ['<'] prev rest (go rest False False (Just '<'))
          -- -> and => already spaced upstream; pass through so > is not re-spaced
          go ('-' :: '>' :: rest) False False _ = '-' :: '>' :: go rest False False (Just '>')
          go ('=' :: '>' :: rest) False False _ = '=' :: '>' :: go rest False False (Just '>')
          -- >= comparison: add spaces around the pair
          go ('>' :: '=' :: rest) False False prev =
            addSpaces ['>', '='] prev rest (go rest False False (Just '='))
          -- >> shift: pass through
          go ('>' :: '>' :: rest) False False _ = '>' :: '>' :: go rest False False (Just '>')
          -- > single comparison
          go ('>' :: rest) False False prev =
            addSpaces ['>'] prev rest (go rest False False (Just '>'))
          go (c :: xs) is ic _ = c :: go xs is ic (Just c)

      processLine : String -> String
      processLine line =
        let withTrim      = if options.trimTrailingWhitespace == Just True
                               then trimTrailingWS line
                               else line
            withCollapsed = collapseInteriorSpaces withTrim
            withCommas    = spaceAfterCommas withCollapsed
            withColon     = spaceAroundColon withCommas
            withArrows    = spaceAroundArrows withColon
            withEquals    = spaceAroundEquals withArrows
            withPipe      = spaceAroundPipeAndBind withEquals
            withBraces    = spaceInsideBraces withPipe
            withDollar    = spaceAroundDollar withBraces
            withArith     = spaceAroundArithmetic withDollar
            withComment   = spaceAfterLineComment withArith
         in withComment
      
      -- For literate idris, we've already handled the lines
      pass1 : List String
      pass1 = if isLiterate then passedIndent else map processLine passedIndent

      -- Step 4-8: Structural normalization (ONLY for non-literate files, and
      -- only when doStructural is True — range formatting skips this to avoid
      -- affecting lines outside the selected range).
      finalLines : List String
      finalLines = if isLiterate || not doStructural
                      then pass1
                      else normalizeStructure pass1
        where
          normalizeStructure : List String -> List String
          normalizeStructure linesInput =
            let collapseBlanks : List String -> List String
                collapseBlanks [] = []
                collapseBlanks (x :: xs) = x :: go False xs
                  where
                    go : Bool -> List String -> List String
                    go wasBlank [] = []
                    go wasBlank (y :: ys) =
                      let isBlank = trim y == ""
                       in if isBlank && wasBlank
                             then go True ys  -- skip consecutive blank
                             else y :: go isBlank ys

                dropWhileEnd : (a -> Bool) -> List a -> List a
                dropWhileEnd p = reverse . dropWhile p . reverse

                isModuleLine : String -> Bool
                isModuleLine line = isPrefixOf "module" (trim line)
                
                ensureSingleBlank : List String -> List String
                ensureSingleBlank [] = [""]
                ensureSingleBlank (y :: ys) =
                  if trim y == ""
                     then "" :: ys  -- already blank
                     else "" :: y :: ys  -- insert blank
                
                ensureModuleBlank : List String -> List String
                ensureModuleBlank [] = []
                ensureModuleBlank (x :: xs) =
                  if isModuleLine x
                     then x :: ensureSingleBlank xs
                     else x :: xs

                normalizeImports : List String -> List String
                normalizeImports linesIn = go False [] linesIn
                  where
                    needBlankBeforeImports : List String -> Bool
                    needBlankBeforeImports [] = False
                    needBlankBeforeImports (y :: _) =
                      not (isModuleLine y) && trim y /= ""
                    
                    go : Bool -> List String -> List String -> List String
                    go inImports acc [] = 
                      let acc' = if inImports then "" :: acc else acc
                       in reverse acc'
                    go inImports acc (x :: xs) =
                      let isImport = isPrefixOf "import" (trim x)
                          isBlank = trim x == ""
                       in if isImport
                             then
                               let acc' = if not inImports && needBlankBeforeImports acc
                                             then "" :: acc
                                             else acc
                                in go True (x :: acc') xs
                             else if isBlank && inImports
                                     then go True acc xs  -- skip blanks and STAY in import block
                                     else
                                       -- End of import block, add blank after
                                       let acc' = if inImports then "" :: acc else acc
                                        in go False (x :: acc') xs
                                        
                collapsed = collapseBlanks linesInput
                trimmedLeading = dropWhile (\s => trim s == "") collapsed
                trimmedTrailing = dropWhileEnd (\s => trim s == "") trimmedLeading
                withModuleBlank = ensureModuleBlank trimmedTrailing
                withImportSpacing = normalizeImports withModuleBlank
             in withImportSpacing

      -- Step 9: Check if we should add final newline
      hadContent : Bool
      hadContent = not (null finalLines)

      -- Step 10: unlines always adds a trailing newline
   in if hadContent then unlines finalLines else ""

||| Format an entire Idris source file, including structural normalization
||| (blank line collapsing, module/import spacing).
export
formatIdrisSource : FormattingOptions -> String -> Bool -> String
formatIdrisSource = formatIdrisSourceImpl True

||| Format a fragment of Idris source (e.g. a selected range), applying only
||| per-line transforms without structural normalization that could affect
||| lines outside the fragment.
export
formatIdrisSourceRange : FormattingOptions -> String -> Bool -> String
formatIdrisSourceRange = formatIdrisSourceImpl False

export
handleRequest :
       Ref LSPConf LSPConfiguration
    => Ref Ctxt Defs
    => Ref UST UState
    => Ref Syn SyntaxInfo
    => Ref MD Metadata
    => Ref ROpts REPLOpts
    => (method : Method Client Request)
    -> (params : MessageParams method)
    -> Core (Either ResponseError (ResponseResult method))

handleRequest Initialize params = do
  logI Channel "Received initialization request"
  whenJust params.initializationOptions processSettings

  update LSPConf ({ initialized := Just params })
  logI Server "Server initialized and configured"
  pure $ pure $ MkInitializeResult serverCapabilities (Just serverInfo)

handleRequest Shutdown params = do
  logI Channel "Received shutdown request"
  -- In a future multithreaded model, we must guarantee that all pending request are still executed.
  update LSPConf ({ isShutdown := True })
  logI Server "Server ready to be shutdown"
  pure $ pure $ (the (Maybe Null) Nothing)

handleRequest TextDocumentHover params = whenActiveRequest $ \conf => do
  logI Channel "Received hover request for \{show params.textDocument.uri}"
  False <- isDirty params.textDocument.uri
    | True => do logW Hover "\{show params.textDocument.uri} has unsaved changes, cannot complete the request"
                 pure $ pure $ make $ MkNull
  withURI conf params.textDocument.uri Nothing (pure $ pure $ make $ MkNull) $ do
    Nothing <- gets LSPConf (map snd . head' . searchPos (cast params.position) . cachedHovers)
      | Just hover => do logD Hover "Found cached action"
                         pure $ pure $ make hover

    defs <- get Ctxt
    nameLocs <- gets MD nameLocMap

    let Just (loc, name) = findPointInTreeLoc (cast params.position) nameLocs
      | Nothing => pure $ pure $ make $ MkNull
    logD Hover "Found name \{show name}"
    -- Lookup the name globally
    globals <- lookupCtxtName name (gamma defs)
    globalResult <- the (Core $ Maybe $ Doc IdrisAnn) $ case globals of
      [] => pure Nothing
      ts => do tys <- traverse (displayType defs) ts
               pure $ Just (vsep tys)
    localResult <- findTypeAt $ anyWithName name $ within (cast params.position)
    line <- case (globalResult, localResult) of
      -- Give precedence to the local name, as it shadows the others
      (_, Just (n, _, type)) => pure $ renderString $ unAnnotateS $ layoutUnbounded $
                                  pretty0 (nameRoot n) <++> colon <++> !(displayTerm defs type)
      (Just globalDoc, Nothing) => pure $ renderString $ unAnnotateS $ layoutUnbounded globalDoc
      (Nothing, Nothing) => pure ""
    let False = null line
      | True => pure $ pure $ make $ MkNull
    let supportsMarkup = maybe False (Markdown `elem`) $ conf.capabilities.textDocument >>= .hover >>= .contentFormat
    let markupContent = the MarkupContent $ if supportsMarkup
                                               then MkMarkupContent Markdown $ "```idris\n\{line}\n```"
                                               else MkMarkupContent PlainText line
    let hover = MkHover (make markupContent) Nothing
    -- TODO consider reenabling hover caching once locations are accurate
    -- update LSPConf ({ cachedHovers $= insert (cast loc, hover) })
    pure $ pure (make hover)

handleRequest TextDocumentDocumentHighlight params = whenActiveRequest $ \conf => do
  logI Channel "Received documentHighlight request for \{show params.textDocument.uri}"
  False <- isDirty params.textDocument.uri
    | True => do logW DocumentHighlight "\{show params.textDocument.uri} has unsaved changes, cannot complete the request"
                 pure $ pure $ make $ MkNull
  withURI conf params.textDocument.uri Nothing (pure $ pure $ make MkNull) $ do
    highlights <- documentHighlights params
    pure $ pure $ make highlights

handleRequest TextDocumentDefinition params = whenActiveRequest $ \conf => do
  logI Channel "Received gotoDefinition request for \{show params.textDocument.uri}"
  False <- isDirty params.textDocument.uri
    | True => do logW GotoDefinition "\{show params.textDocument.uri} has unsaved changes, cannot complete the request"
                 pure $ pure $ make $ MkNull
  withURI conf params.textDocument.uri Nothing (pure $ pure $ make $ MkNull) $ do
    Just link <- gotoDefinition params
      | Nothing => pure $ pure $ make MkNull
    pure $ pure $ make link

handleRequest TextDocumentCodeAction params = whenActiveRequest $ \conf => do
  logI Channel "Received codeAction request for \{show params.textDocument.uri}"
  False <- isDirty params.textDocument.uri
    | True => do logW CodeAction "\{show params.textDocument.uri} has unsaved changes, cannot complete the request"
                 pure $ pure $ make $ MkNull
  withURI conf params.textDocument.uri Nothing
    (do quickfixActions <- if maybe True (CodeActionKind.QuickFix `elem`) params.context.only
                              then map Just <$> gets LSPConf quickfixes
                              else pure []
        pure $ pure $ make $ flatten quickfixActions)
    (do quickfixActions <- if maybe True (CodeActionKind.QuickFix `elem`) params.context.only
                              then map Just <$> gets LSPConf quickfixes
                              else pure []
        exprSearchActions <- map Just <$> exprSearch params
        splitAction <- caseSplit params
        lemmaAction <- makeLemma params
        withAction <- makeWith params
        clauseAction <- addClause params
        makeCaseAction <- makeCase params
        introActions <- map Just <$> intro params
        generateDefActions <- map Just <$> generateDef params
        generateDefNextActions <- map Just <$> generateDefNext params
        let resp = flatten $ quickfixActions
                               ++ [splitAction, lemmaAction, withAction, clauseAction, makeCaseAction]
                               ++ introActions ++ generateDefActions ++ generateDefNextActions ++ exprSearchActions
        pure $ pure $ make resp)
    where
      flatten : List (Maybe CodeAction) -> List (OneOf [Command, CodeAction])
      flatten [] = []
      flatten (Nothing :: xs) = flatten xs
      flatten ((Just x) :: xs) = make x :: flatten xs

      searchEq : Maybe CodeAction -> Maybe CodeAction -> Bool
      searchEq (Just (MkCodeAction {edit = Just (MkWorkspaceEdit {changes = edit1, _}), _}))
               (Just (MkCodeAction {edit = Just (MkWorkspaceEdit {changes = edit2, _}), _})) = edit1 == edit2
      searchEq _ _ = False

handleRequest TextDocumentSignatureHelp params = whenActiveRequest $ \conf => do
  logI Channel "Received signatureHelp request for \{show params.textDocument.uri}"
  False <- isDirty params.textDocument.uri
    | True => do logW SignatureHelp "\{show params.textDocument.uri} has unsaved changes, cannot complete the request"
                 pure $ pure $ make $ MkNull
  withURI conf params.textDocument.uri Nothing (pure $ pure $ make $ MkNull) $ do
    Just signatureHelpData <- signatureHelp params | Nothing => pure $ pure $ make MkNull
    pure $ pure $ make signatureHelpData

handleRequest TextDocumentDocumentSymbol params = whenActiveRequest $ \conf => do
  logI Channel "Received documentSymbol request for \{show params.textDocument.uri}"
  False <- isDirty params.textDocument.uri
    | True => do logW DocumentSymbol "\{show params.textDocument.uri} has unsaved changes, cannot complete the request"
                 pure $ pure $ make $ MkNull
  withURI conf params.textDocument.uri Nothing (pure $ pure $ make $ MkNull) $ do
    documentSymbolData <- documentSymbol params
    pure $ pure $ make documentSymbolData

handleRequest TextDocumentCodeLens params = whenActiveRequest $ \conf => do
  -- TODO: still unimplemented, but send empty message to avoid errors
  -- in clients since it's a common request
  logW Channel $ "Received an unsupported codeLens request"
  pure $ pure $ make MkNull

handleRequest TextDocumentCompletion params = whenActiveRequest $ \conf => do
  logW Channel $ "Received an completion request for \{show params.textDocument.uri}"
  case !(completion params) of
    Nothing => pure $ pure $ make MkNull
    Just cs => pure $ pure $ make cs

handleRequest TextDocumentDocumentLink params = whenActiveRequest $ \conf => do
  -- TODO: still unimplemented, but send empty message to avoid errors
  -- in clients since it's a common request
  logW Channel $ "Received an unsupported documentLink request"
  pure $ pure $ make MkNull

handleRequest TextDocumentSemanticTokensFull params = whenActiveRequest $ \conf => do
    False <- isDirty params.textDocument.uri
      | True => pure $ Left (MkResponseError RequestCancelled "Document Dirty" JNull)
    Nothing <- gets LSPConf (lookup params.textDocument.uri . semanticTokensSentFiles)
      | Just tokens => pure $ pure $ (make $ tokens)
    withURI conf params.textDocument.uri Nothing (pure $ Left (MkResponseError RequestCancelled "Document Errors" JNull)) $ do
      md <- get MD
      src <- getSource
      let srcLines = lines src
      let getLineLength = \lineNum => maybe 0 (cast . length) $ elemAt srcLines (integerToNat (cast lineNum))
      tokens <- getSemanticTokens md getLineLength
      update LSPConf ({ semanticTokensSentFiles $= insert params.textDocument.uri tokens })
      pure $ pure $ (make $ tokens)

handleRequest TextDocumentFormatting params = whenActiveRequest $ \conf => do
  logI Channel "Received formatting request for \{show params.textDocument.uri}"
  -- Formatting only needs the raw text; bypass withURI/loadIfNeeded so it
  -- works on first save and on files with type errors.
  let uri = params.textDocument.uri
  Just (_, src) <- gets LSPConf (lookup uri . virtualDocuments)
    | Nothing => do logW Formatting "No virtual document for \{show uri}, skipping format"
                    pure $ pure $ make (the (List TextEdit) [])
  let srcLines = lines src
  let isLiterate = isSuffixOf ".lidr" uri.path
  let formattedSrc = formatIdrisSource params.options src isLiterate
  -- Replace the entire document with a single edit to avoid conflicting
  -- sequential-apply issues (e.g. a line-content edit followed by a
  -- newline-insert edit referencing the original column).
  let numLines = cast (length srcLines)
  let endPos = if isSuffixOf "\n" src
                  then MkPosition numLines 0
                  else MkPosition (numLines - 1) (cast $ maybe 0 length (last' srcLines))
  let edits : List TextEdit = if src == formattedSrc
                 then []
                 else [MkTextEdit (MkRange (MkPosition 0 0) endPos) formattedSrc]
  logD Formatting "Formatted document: generated \{show (length edits)} edits"
  pure $ pure $ make edits

handleRequest TextDocumentRangeFormatting params = whenActiveRequest $ \conf => do
  logI Channel "Received range formatting request for \{show params.textDocument.uri}"
  let uri = params.textDocument.uri
  Just (_, src) <- gets LSPConf (lookup uri . virtualDocuments)
    | Nothing => do logW Formatting "No virtual document for \{show uri}, skipping range format"
                    pure $ pure $ make (the (List TextEdit) [])
  let srcLines = lines src
  let startLine = cast {to = Nat} params.range.start.line
  let endLine   = cast {to = Nat} params.range.end.line
  -- Extract the lines in the selected range (inclusive)
  let rangeLines = take (endLine `minus` startLine + 1) (drop startLine srcLines)
  let rangeSrc   = unlines rangeLines
  let isLiterate = isSuffixOf ".lidr" uri.path
  let formattedSrc = formatIdrisSourceRange params.options rangeSrc isLiterate
  -- Build a replacement edit covering exactly the selected lines.
  -- End position: start of the line after endLine (i.e. endLine+1, col 0),
  -- which includes the trailing newline of the last selected line.
  let startPos = MkPosition (cast startLine) 0
  let endPos   = MkPosition (cast (endLine + 1)) 0
  let edits : List TextEdit = if rangeSrc == formattedSrc
                 then []
                 else [MkTextEdit (MkRange startPos endPos) formattedSrc]
  logD Formatting "Formatted range [\{show startLine}-\{show endLine}]: generated \{show (length edits)} edits"
  pure $ pure $ make edits

handleRequest TextDocumentOnTypeFormatting params = whenActiveRequest $ \conf => do
  logI Channel "Received on-type formatting request for \{show params.textDocument.uri}"
  let uri = params.textDocument.uri
  Just (_, src) <- gets LSPConf (lookup uri . virtualDocuments)
    | Nothing => do logW Formatting "No virtual document for \{show uri}, skipping on-type format"
                    pure $ pure $ make $ MkNull
  let srcLines = lines src
  -- params.position is the cursor position after the typed character (\n),
  -- so the line to format is the one before: position.line - 1
  let lineNum = params.position.line - 1
  let Just line = elemAt srcLines (integerToNat (cast lineNum))
    | Nothing => pure $ pure $ make $ MkNull
  let isLiterate = isSuffixOf ".lidr" uri.path
  let formatted = formatIdrisSourceRange params.options (line ++ "\n") isLiterate
  -- formatIdrisSourceRange returns the line with a trailing newline; strip it for comparison
  let formattedLine = if isSuffixOf "\n" formatted
                         then substr 0 (length formatted `minus` 1) formatted
                         else formatted
  if formattedLine == line
     then pure $ pure $ make $ MkNull
     else do
       let range = MkRange (MkPosition (cast lineNum) 0) (MkPosition (cast lineNum) (cast $ length line))
       let edit : TextEdit = MkTextEdit range formattedLine
       pure $ pure $ make (the (List TextEdit) [edit])

handleRequest WorkspaceExecuteCommand
  (MkExecuteCommandParams partialResultToken "repl" (Just [json])) = whenActiveRequest $ \conf => do
    logI Channel "Received repl command request"
    let Just cmd = fromJSON {a = String} json
      | Nothing => pure $ Left (invalidParams "Expected String")
    c <- getColor
    -- Turn off the colour, because JSON
    -- seems to fail to correctly escape the codes.
    setColor False
    res <- interpret cmd
    doc <- replResultToDoc res
    str <- render doc
    setColor c
    pure $ Right (JString str)
handleRequest WorkspaceExecuteCommand (MkExecuteCommandParams _ "metavars" _) = whenActiveRequest $ \conf => do
  logI Channel "Received metavars command request"
  Right . toJSON <$> metavarsCmd
handleRequest WorkspaceExecuteCommand (MkExecuteCommandParams _ "exprSearchWithHints" (Just [json])) = whenActiveRequest $ \conf => do
  logI Channel "Received exprSearchWithHints command request"
  let Just params = fromJSON json
    | Nothing => pure $ Left (invalidParams "Expected ExprSearchWithHintsParams")
  actions <- exprSearchWithHints params
  pure $ Right (toJSON actions)
handleRequest WorkspaceExecuteCommand (MkExecuteCommandParams _ "refineHole" (Just [json])) = whenActiveRequest $ \conf => do
  logI Channel "Received refineHole command request"
  let Just params = fromJSON json
    | Nothing => pure $ Left (invalidParams "Expected RefineHoleParams")
  actions <- refineHole params
  pure $ Right (toJSON actions)
handleRequest WorkspaceExecuteCommand (MkExecuteCommandParams _ "browseNamespace" (Just [json])) = whenActiveRequest $ \conf => do
  logI Channel "Received browseNamespace command request"
  let Just params = fromJSON {a = String} json
    | Nothing => pure $ Left (invalidParams "Expected String")
  names <- browseNamespaceCmd params
  pure $ Right (toJSON names)
handleRequest method params = whenActiveRequest $ \conf => do
    logW Channel $ "Received a not supported \{show (toJSON method)} request"
    pure $ Left methodNotFound

export
handleNotification : Ref LSPConf LSPConfiguration
                  => Ref Ctxt Defs
                  => Ref UST UState
                  => Ref Syn SyntaxInfo
                  => Ref MD Metadata
                  => Ref ROpts REPLOpts
                  => (method : Method Client Notification)
                  -> (params : MessageParams method)
                  -> Core ()

handleNotification Exit params = do
  logI Channel "Received exit notification"
  status <- if !(gets LSPConf isShutdown)
               then logI Server "Quitting the server..." >> pure ExitSuccess
               else logC Server "Quitting the server without a proper shutdown" >> pure (ExitFailure 1)
  coreLift $ exitWith status

handleNotification WorkspaceDidChangeConfiguration params = whenActiveNotification $ \conf => do
  logI Channel "Received didChangeConfiguration notification"
  processSettings params.settings

handleNotification TextDocumentDidOpen params = whenActiveNotification $ \conf => do
  logI Channel "Received didOpen notification for \{show params.textDocument.uri}"
  ignore $ loadURI conf params.textDocument.uri (Just params.textDocument.version)
  update LSPConf
    { virtualDocuments
        $= insert params.textDocument.uri (params.textDocument.version, params.textDocument.text)
    }

handleNotification TextDocumentDidSave params = whenActiveNotification $ \conf => do
  logI Channel "Received didSave notification for \{show params.textDocument.uri}"
  update LSPConf (
    { dirtyFiles $= delete params.textDocument.uri
    , errorFiles $= delete params.textDocument.uri
    , semanticTokensSentFiles $= delete params.textDocument.uri
    })
  ignore $ loadURI conf params.textDocument.uri Nothing
  when (fromMaybe False $ conf.capabilities.workspace >>= semanticTokens >>= refreshSupport) $ do
    logI SemanticTokens "Sending semantic tokens for \{show params.textDocument.uri}"
    sendRequestMessage_ WorkspaceSemanticTokensRefresh Nothing

handleNotification TextDocumentDidChange params = whenActiveNotification $ \conf => do
  logI Channel "Received didChange notification for \{show params.textDocument.uri}"
  update LSPConf ({ dirtyFiles $= insert params.textDocument.uri})
  whenJust !(gets LSPConf (lookup params.textDocument.uri . virtualDocuments)) $ \(v,content) => do
    case changeVirtualContent params.contentChanges content of
      Left e => do
        update LSPConf ({ virtualDocuments $= delete params.textDocument.uri })
        logE Server "didChange notification for \{show params.textDocument.uri} invalidated virtual file: \{e}"
      Right n => do
        update LSPConf ({ virtualDocuments $= insert params.textDocument.uri (params.textDocument.version, n)})
  logI Server "File \{show params.textDocument.uri} marked as dirty"

handleNotification TextDocumentDidClose params = whenActiveNotification $ \conf => do
  logI Channel "Received didClose notification for \{show params.textDocument.uri}"
  update LSPConf ({ openFile := Nothing
                  , quickfixes := []
                  , cachedActions := empty
                  , cachedHovers := empty
                  , dirtyFiles $= delete params.textDocument.uri
                  , errorFiles $= delete params.textDocument.uri
                  , semanticTokensSentFiles $= delete params.textDocument.uri
                  , completionCache $= delete params.textDocument.uri
                  , virtualDocuments $= delete params.textDocument.uri
                  })
  logI Server $ "File \{show params.textDocument.uri} closed"

handleNotification method params = whenActiveNotification $ \conf =>
  logW Channel "Received unhandled notification for method \{stringify $ toJSON method}"

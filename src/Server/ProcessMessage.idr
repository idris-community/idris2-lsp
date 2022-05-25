||| Functions for the processing of received request and
||| notifications.
|||
||| (C) The Idris Community, 2021
module Server.ProcessMessage

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.List
import Data.List1
import Data.OneOf
import Data.SortedSet
import Data.String
import Idris.Doc.String
import Idris.Error
import Idris.IDEMode.Holes
import Idris.ModTree
import Idris.Package
import Idris.Pretty
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
import Language.LSP.CodeAction.MakeCase
import Language.LSP.CodeAction.MakeLemma
import Language.LSP.CodeAction.MakeWith
import Language.LSP.CodeAction.RefineHole
import Language.LSP.Definition
import Language.LSP.DocumentHighlight
import Language.LSP.DocumentSymbol
import Language.LSP.Message
import Language.LSP.Metavars
import Language.LSP.SignatureHelp
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
replResultToDoc (Edited (DisplayEdit Empty)) = pure (pretty0 "")
replResultToDoc (Edited (DisplayEdit xs)) = pure xs
replResultToDoc (Edited (EditError x)) = pure x
replResultToDoc (Edited (MadeLemma lit name pty pappstr)) = pure $ pretty0 (relit lit (show name ++ " : " ++ show pty ++ "\n") ++ pappstr)
replResultToDoc (Edited (MadeWith lit wapp)) = pure $ pretty0 $ showSep "\n" (map (relit lit) wapp)
replResultToDoc (Edited (MadeCase lit cstr)) = pure $ pretty0 $ showSep "\n" (map (relit lit) cstr)
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
  update LSPConf ({ openFile := Just (uri, fromMaybe 0 version) })
  resetContext (Virtual Interactive)
  let fpath = uri.path
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
  unless (null errs) (update LSPConf ({ errorFiles $= insert uri }))
  resetProofState
  let caps = (publishDiagnostics <=< textDocument) . capabilities $ conf
  update LSPConf ({ quickfixes := [], cachedActions := empty, cachedHovers := empty })
  traverse_ (findQuickfix caps uri) errs
  defs <- get Ctxt
  session <- getSession
  let warnings = if session.warningsAsErrors then [] else reverse (warnings defs)
  sendDiagnostics caps uri version warnings errs
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
  False <- isError uri
    | _ => logW Server "Trying to load \{show uri} which has errors" >> d
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
    update LSPConf ({ cachedHovers $= insert (cast loc, hover) })
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
        generateDefActions <- map Just <$> generateDef params
        let resp = flatten $ quickfixActions
                               ++ [splitAction, lemmaAction, withAction, clauseAction, makeCaseAction]
                               ++ generateDefActions ++ exprSearchActions
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
  -- TODO: still unimplemented, but send empty message to avoid errors
  -- in clients since it's a common request
  logW Channel $ "Received an unsupported completion request"
  pure $ pure $ make MkNull

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
  update LSPConf ({ dirtyFiles $= insert params.textDocument.uri })
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
                  })
  logI Server $ "File \{show params.textDocument.uri} closed"

handleNotification method params = whenActiveNotification $ \conf =>
  logW Channel "Received unhandled notification for method \{stringify $ toJSON method}"

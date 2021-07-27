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
import Data.OneOf
import Data.SortedSet
import Idris.ModTree
import Idris.Package
import Idris.Pretty
import Idris.REPL
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Idris.IDEMode.Holes
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.CodeAction.AddClause
import Language.LSP.CodeAction.CaseSplit
import Language.LSP.CodeAction.ExprSearch
import Language.LSP.CodeAction.RefineHole
import Language.LSP.CodeAction.GenerateDef
import Language.LSP.CodeAction.MakeCase
import Language.LSP.CodeAction.MakeLemma
import Language.LSP.CodeAction.MakeWith
import Language.LSP.Definition
import Language.LSP.DocumentSymbol
import Language.LSP.SignatureHelp
import Language.LSP.Message
import Libraries.Data.PosMap
import Libraries.Utils.Path
import Server.Capabilities
import Server.Configuration
import Server.Log
import Server.QuickFix
import Server.SemanticTokens
import Server.Response
import Server.Utils
import System
import System.Clock
import System.Directory
import System.File
import Data.List1
import Libraries.Data.List.Extra

displayType : {auto c : Ref Ctxt Defs} ->
              {auto s : Ref Syn SyntaxInfo} ->
              Defs -> (Name, Int, GlobalDef) ->
              Core (Doc IdrisAnn)
displayType defs (n, i, gdef)
    = maybe (do tm <- resugar [] !(normaliseHoles defs [] (type gdef))
                pure (pretty !(aliasName (fullname gdef)) <++> colon <++> prettyTerm tm))
            (\num => reAnnotate Syntax <$> prettyHole defs [] n num (type gdef))
            (isHole gdef)

displayTerm : {auto c : Ref Ctxt Defs} ->
              {auto s : Ref Syn SyntaxInfo} ->
              Defs -> ClosedTerm ->
              Core (Doc IdrisAnn)
displayTerm defs tm
    = do ptm <- resugar [] !(normaliseHoles defs [] tm)
         pure (prettyTerm ptm)

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
  update LSPConf (record {openFile = Just (uri, fromMaybe 0 version)})
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
  Just fname <- findIpkg (Just fpath)
    | Nothing => do let msg = "Cannot find ipkg file for \{show uri}"
                    logE Server msg
                    pure $ Left msg
  Right res <- coreLift $ File.readFile fname
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
  case errs of
    [] => pure ()
    (_::_) => update LSPConf (record { errorFiles $= insert uri })
  resetProofState
  let caps = (publishDiagnostics <=< textDocument) . capabilities $ conf
  update LSPConf (record { quickfixes = [], cachedActions = empty, cachedHovers = empty })
  traverse_ (findQuickfix caps uri) errs
  sendDiagnostics caps uri version errs
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
  -- TODO: Here we should analyze the client capabilities.
  logI Channel "Received initialization request"

  case params.initializationOptions of
       Just (JObject xs) => do
         case lookup "logFile" xs of
              Just (JString fname) => changeLogFile fname
              Just _ => logE Configuration "Incorrect type for log file location, expected string"
              Nothing => pure ()
         case lookup "longActionTimeout" xs of
              Just (JNumber v) => setSearchTimeout $ cast v
              Just _ => logE Configuration "Incorrect type for long action timeout, expected number"
              Nothing => pure ()
         case lookup "maxCodeActionResults" xs of
              Just (JNumber v) => update LSPConf (record { searchLimit = integerToNat $ cast v })
              Just _ => logE Configuration "Incorrect type for max code action results, expected number"
              Nothing => pure ()
       Just _ => logE Configuration "Incorrect type for initialization options"
       Nothing => pure ()

  update LSPConf (record {initialized = Just params})
  logI Server "Server initialized and configured"
  pure $ pure $ MkInitializeResult serverCapabilities (Just serverInfo)

handleRequest Shutdown params = do
  logI Channel "Received shutdown request"
  -- In a future multithreaded model, we must guarantee that all pending request are still executed.
  update LSPConf (record {isShutdown = True})
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

    let Just (loc, name) = findPointInTreeLoc (params.position.line, params.position.character) nameLocs
      | Nothing => pure $ pure $ (make $ MkHover (make $ MkMarkupContent PlainText "") Nothing)
    logD Hover "Found name \{show name}"
    -- Lookup the name globally
    globals <- lookupCtxtName name (gamma defs)
    globalResult <- the (Core $ Maybe $ Doc IdrisAnn) $ case globals of
      [] => pure Nothing
      ts => do tys <- traverse (displayType defs) ts
               pure $ Just (vsep tys)
    localResult <- findTypeAt $ anyWithName name $ within (params.position.line, params.position.character)
    line <- case (globalResult, localResult) of
      -- Give precedence to the local name, as it shadows the others
      (_, Just (n, _, type)) => pure $ renderString $ unAnnotateS $ layoutUnbounded $ pretty (nameRoot n) <++> colon <++> !(displayTerm defs type)
      (Just globalDoc, Nothing) => pure $ renderString $ unAnnotateS $ layoutUnbounded globalDoc
      (Nothing, Nothing) => pure ""
    let supportsMarkup = maybe False (Markdown `elem`) $ conf.capabilities.textDocument >>= .hover >>= .contentFormat
    let markupContent = the MarkupContent $ if supportsMarkup then MkMarkupContent Markdown $ "```idris\n" ++ line ++ "\n```" else MkMarkupContent PlainText line
    let hover = MkHover (make markupContent) Nothing
    update LSPConf (record { cachedHovers $= insert (cast loc, hover) })
    pure $ pure (make hover)

handleRequest TextDocumentDefinition params = whenActiveRequest $ \conf => do
  logI Channel "Received gotoDefinition request for \{show params.textDocument.uri}"
  False <- isDirty params.textDocument.uri
    | True => do logW GotoDefinition "\{show params.textDocument.uri} has unsaved changes, cannot complete the request"
                 pure $ pure $ make $ MkNull
  withURI conf params.textDocument.uri Nothing (pure $ pure $ make $ MkNull) $ do
    Just link <- gotoDefinition params | Nothing => pure $ pure $ make MkNull
    pure $ pure $ make link

handleRequest TextDocumentCodeAction params = whenActiveRequest $ \conf => do
  logI Channel "Received codeAction request for \{show params.textDocument.uri}"
  False <- isDirty params.textDocument.uri
    | True => do logW CodeAction "\{show params.textDocument.uri} has unsaved changes, cannot complete the request"
                 pure $ pure $ make $ MkNull
  withURI conf params.textDocument.uri Nothing (pure $ pure $ make $ MkNull) $ do
    quickfixActions <- map Just <$> gets LSPConf quickfixes
    exprSearchAction <- map Just <$> exprSearch params
    splitAction <- caseSplit params
    lemmaAction <- makeLemma params
    withAction <- makeWith params
    clauseAction <- addClause params
    makeCaseAction <- handleMakeCase params
    generateDefAction <- map Just <$> generateDef params
    refineHoleAction <- map Just <$> refineHole params
    let resp = flatten $ quickfixActions ++ refineHoleAction
                           ++ [splitAction, lemmaAction, withAction, clauseAction, makeCaseAction]
                           ++ generateDefAction ++ exprSearchAction
    pure $ pure $ make resp
    where
      flatten : List (Maybe CodeAction) -> List (OneOf [Command, CodeAction])
      flatten [] = []
      flatten (Nothing :: xs) = flatten xs
      flatten ((Just x) :: xs) = make x :: flatten xs

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
    False <- gets LSPConf (contains params.textDocument.uri . semanticTokensSentFiles)
      | True => pure $ Left (MkResponseError RequestCancelled "Semantic tokens already sent" JNull)
    withURI conf params.textDocument.uri Nothing (pure $ Left (MkResponseError RequestCancelled "Document Errors" JNull)) $ do
      md <- get MD
      src <- getSource
      let srcLines = forget $ lines src
      let getLineLength = \lineNum => maybe 0 (cast . length) $ elemAt srcLines (integerToNat (cast lineNum))
      tokens <- getSemanticTokens md getLineLength
      update LSPConf (record { semanticTokensSentFiles $= insert params.textDocument.uri })
      pure $ pure $ (make $ tokens)

handleRequest method params = whenActiveRequest $ \conf => do
    logW Channel $ "Received a not supported \{show (toJSON method)} request"
    pure $ Left methodNotFound

export
handleNotification :
       Ref LSPConf LSPConfiguration
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

handleNotification TextDocumentDidOpen params = whenActiveNotification $ \conf => do
  logI Channel "Received didOpen notification for \{show params.textDocument.uri}"
  ignore $ loadURI conf params.textDocument.uri (Just params.textDocument.version)

handleNotification TextDocumentDidSave params = whenActiveNotification $ \conf => do
  logI Channel "Received didSave notification for \{show params.textDocument.uri}"
  update LSPConf (record
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
  update LSPConf (record { dirtyFiles $= insert params.textDocument.uri })
  logI Server "File \{show params.textDocument.uri} marked as dirty"

handleNotification TextDocumentDidClose params = whenActiveNotification $ \conf => do
  logI Channel "Received didClose notification for \{show params.textDocument.uri}"
  update LSPConf (record { openFile = Nothing
                         , quickfixes = []
                         , cachedActions = empty
                         , cachedHovers = empty
                         , dirtyFiles $= delete params.textDocument.uri
                         , errorFiles $= delete params.textDocument.uri
                         , semanticTokensSentFiles $= delete params.textDocument.uri
                         })
  logI Server $ "File \{show params.textDocument.uri} closed"

handleNotification method params = whenActiveNotification $ \conf =>
  logW Channel "Received unhandled notification for method \{stringify $ toJSON method}"

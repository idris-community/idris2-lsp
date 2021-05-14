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
import Language.LSP.CodeAction.AddClause
import Language.LSP.CodeAction.CaseSplit
import Language.LSP.CodeAction.ExprSearch
import Language.LSP.CodeAction.GenerateDef
import Language.LSP.CodeAction.MakeLemma
import Language.LSP.CodeAction.MakeWith
import Language.LSP.CodeAction.MakeCase
import Language.LSP.Definition
import Language.LSP.DocumentSymbol
import Language.LSP.SignatureHelp
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Capabilities
import Server.Configuration
import Server.Log
import Server.QuickFix
import Server.SemanticTokens
import Server.Response
import Server.Utils
import System
import System.Clock
import System.Path
import System.Directory

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

||| Guard for messages that requires a successful initialization before being allowed.
whenInitialized : Ref LSPConf LSPConfiguration => (InitializeParams -> Core ()) -> Core ()
whenInitialized k =
  case !(gets LSPConf initialized) of
       Just conf => k conf
       Nothing => sendUnknownResponseMessage serverNotInitialized

||| Guard for messages that cannot be sent after the shutdown protocol.
whenNotShutdown : Ref LSPConf LSPConfiguration => Core () -> Core ()
whenNotShutdown k =
  if !(gets LSPConf isShutdown)
     then sendUnknownResponseMessage (invalidRequest "Server has been shutdown")
     else k

whenNotDirty : Ref LSPConf LSPConfiguration
            => Method Client Request -> OneOf [Int, String, Null] -> DocumentURI -> Core () -> Core ()
whenNotDirty method id uri k = do
  if !(gets LSPConf (contains uri . dirtyFiles))
    then let response = Failure id (MkResponseError (Custom 2) "File has changes that were not saved" JNull)
          in sendResponseMessage method response
    else k

loadURI : Ref LSPConf LSPConfiguration
       => Ref Ctxt Defs
       => Ref UST UState
       => Ref Syn SyntaxInfo
       => Ref MD Metadata
       => Ref ROpts REPLOpts
       => InitializeParams -> URI -> Maybe Int -> Core ()
loadURI conf uri version = do
  modify LSPConf (record {openFile = Just (uri, fromMaybe 0 version)})
  resetContext "(interactive)"
  let fpath = uri.path
  let Just (startFolder, startFile) = splitParent fpath | _ => do
    logString Error "Error in loadURI expected valid uri"
    sendUnknownResponseMessage (invalidParams "Expected valid file uri")
  logString Warning $ "start folder: " ++ startFolder
  logString Warning $ "start file: " ++ startFile
  True <- coreLift $ changeDir {io=IO} startFolder | False => do
    logString Error "Error in loadURI expected valid uri"
    sendUnknownResponseMessage (invalidParams "Expected valid file uri")
  Just fname <- findIpkg (Just startFile) | _ => do
    logString Error "Unable to find ipkg"
    sendUnknownResponseMessage (invalidParams "Unable to find ipkg")
  logString Warning $ "fname: " ++ fname
  logString Warning $ "directory: " ++ show !(coreLift currentDir)

  errs <- buildDeps fname -- FIXME: the compiler always dumps the errors on stdout, requires
                          --        a compiler change.
  Right res <- coreLift (readFile fname) | Left err => do
    setSource ""
    logString Error "Unable to read uri"
    sendUnknownResponseMessage (invalidParams "Unable to read uri")
  setSource res
  logString Warning $ "Source set too: " ++ show res
  resetProofState
  let caps = (publishDiagnostics <=< textDocument) . capabilities $ conf
  modify LSPConf (record { quickfixes = [], cachedActions = empty, cachedHovers = empty })
  traverse_ (findQuickfix caps uri) errs
  sendDiagnostics caps uri version errs

loadIfNeeded : Ref LSPConf LSPConfiguration
            => Ref Ctxt Defs
            => Ref UST UState
            => Ref Syn SyntaxInfo
            => Ref MD Metadata
            => Ref ROpts REPLOpts
            => InitializeParams -> URI -> Maybe Int -> Core ()
loadIfNeeded conf uri version = do
  Just (oldUri, oldVersion) <- gets LSPConf openFile
    | Nothing => loadURI conf uri version
  unless (oldUri == uri && (isNothing version || (Just oldVersion) == version)) $
    loadURI conf uri version

||| Process a parsed LSP message received from a client.
export
processMessage : Ref LSPConf LSPConfiguration
              => Ref Ctxt Defs
              => Ref UST UState
              => Ref Syn SyntaxInfo
              => Ref MD Metadata
              => Ref ROpts REPLOpts
              => {type : MethodType}
              -> (method : Method Client type)
              -> Message type method
              -> Core ()
processMessage Initialize msg@(MkRequestMessage id Initialize params) = do
  -- TODO: Here we should analyze the client capabilities.
  let response = Success (getResponseId msg) (MkInitializeResult serverCapabilities (Just serverInfo))
  -- The compiler does not work with the same root directory model that LSP operates with,
  -- may be a point of friction in particular with the location of generated binary files.
  {-
  let Just fpath = path <$> toMaybe params.rootUri
    | _ => do logString Error "Error in initialize expected valid root uri"
              sendUnknownResponseMessage (invalidParams "Expected valid root uri")
  fname <- fromMaybe fpath <$> findIpkg (Just fpath)
  -}
  --setSourceDir (Just fname) -- TODO Not needed

  case params.initializationOptions of
       Just (JObject xs) => do
         case lookup "logFile" xs of
              Just (JString fname) => changeLogFile fname
              Just _ => logString Error "Incorrect type for log file location, expected string"
              Nothing => pure ()
         case lookup "longActionTimeout" xs of
              Just (JNumber v) => do
                let n = 1000000 * cast v
                let scale       = 1000000000
                let seconds     = n `div` scale
                let nanoseconds = n `mod` scale
                modify LSPConf (record {longActionTimeout = makeDuration seconds nanoseconds})
              Just _ => logString Error "Incorrect type for long action timeout, expected number"
              Nothing => pure ()
       Just _ => logString Error "Incorrect type for initialization options"
       Nothing => pure ()

  sendResponseMessage Initialize response
  modify LSPConf (record {initialized = Just params})

processMessage Shutdown msg@(MkRequestMessage _ Shutdown _) = do
  -- In a future multithreaded model, we must guarantee that all pending request are still executed.
  let response = Success (getResponseId msg) (the (Maybe Null) Nothing)
  sendResponseMessage Shutdown response
  update LSPConf (record {isShutdown = True})

processMessage Exit (MkNotificationMessage Exit _) = do
  let status = if !(gets LSPConf isShutdown) then ExitSuccess else ExitFailure 1
  coreLift $ exitWith status

processMessage TextDocumentDidOpen msg@(MkNotificationMessage TextDocumentDidOpen params) =
  whenNotShutdown $ whenInitialized $ \conf =>
    loadURI conf params.textDocument.uri (Just params.textDocument.version)

processMessage TextDocumentDidSave msg@(MkNotificationMessage TextDocumentDidSave params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    modify LSPConf (record { dirtyFiles $= delete params.textDocument.uri })
    loadURI conf params.textDocument.uri Nothing

processMessage TextDocumentDidChange msg@(MkNotificationMessage TextDocumentDidChange params) =
  whenNotShutdown $ whenInitialized $ \conf =>
    modify LSPConf (record { dirtyFiles $= insert params.textDocument.uri })

processMessage TextDocumentDidClose msg@(MkNotificationMessage TextDocumentDidClose params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    modify LSPConf (record { openFile = Nothing
                           , quickfixes = []
                           , cachedActions = empty
                           , cachedHovers = empty
                           , dirtyFiles $= delete params.textDocument.uri
                           })
    logString Info $ "File \{params.textDocument.uri.path} closed"

processMessage TextDocumentHover msg@(MkRequestMessage id TextDocumentHover params) =
  whenNotShutdown $ whenInitialized $ \conf =>
    whenNotDirty TextDocumentHover (getResponseId msg) params.textDocument.uri $ do
      loadIfNeeded conf params.textDocument.uri Nothing

      Nothing <- gets LSPConf (map snd . head' . searchPos (cast params.position) . cachedHovers)
        | Just hover => do logString Debug "hover: found cached action"
                           let response = Success (getResponseId msg) (make hover)
                           sendResponseMessage TextDocumentHover response

      defs <- get Ctxt
      nameLocs <- gets MD nameLocMap

      let Just (loc, name) = findPointInTreeLoc (params.position.line, params.position.character) nameLocs
        | Nothing => do let response = Success (getResponseId msg) (make $ MkHover (make $ MkMarkupContent PlainText "") Nothing)
                        sendResponseMessage TextDocumentHover response
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
      modify LSPConf (record { cachedHovers $= insert (cast loc, hover) })
      let response = Success (getResponseId msg) (make hover)
      sendResponseMessage TextDocumentHover response

processMessage TextDocumentDefinition msg@(MkRequestMessage id TextDocumentDefinition params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    whenNotDirty TextDocumentDefinition (getResponseId msg) params.textDocument.uri $ do
      loadIfNeeded conf params.textDocument.uri Nothing
      link <- gotoDefinition params
      ignore $ traverseOpt
                 (sendResponseMessage TextDocumentDefinition . Success (getResponseId msg) . make)
                 link

processMessage TextDocumentCodeAction msg@(MkRequestMessage id TextDocumentCodeAction params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    whenNotDirty TextDocumentCodeAction (getResponseId msg) params.textDocument.uri $ do
      loadIfNeeded conf params.textDocument.uri Nothing

      quickfixActions <- map Just <$> gets LSPConf quickfixes
      exprSearchAction <- map Just <$> exprSearch params
      splitAction <- caseSplit params
      lemmaAction <- makeLemma params
      withAction <- makeWith params
      clauseAction <- addClause params
      makeCaseAction <- handleMakeCase params
      generateDefAction <- map Just <$> generateDef (getResponseId msg) params
      let resp = flatten $ quickfixActions
                             ++ [splitAction, lemmaAction, withAction, clauseAction, makeCaseAction]
                             ++ generateDefAction ++ exprSearchAction
      sendResponseMessage TextDocumentCodeAction (Success (getResponseId msg) (make resp))
      where
        flatten : List (Maybe CodeAction) -> List (OneOf [Command, CodeAction])
        flatten [] = []
        flatten (Nothing :: xs) = flatten xs
        flatten ((Just x) :: xs) = make x :: flatten xs

processMessage TextDocumentSignatureHelp msg@(MkRequestMessage id TextDocumentSignatureHelp params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    whenNotDirty TextDocumentCodeAction (getResponseId msg) params.textDocument.uri $ do
      loadIfNeeded conf params.textDocument.uri Nothing

      signatureHelpData <- signatureHelp params
      ignore $ traverseOpt
                 (sendResponseMessage TextDocumentSignatureHelp . Success (getResponseId msg) . make)
                 signatureHelpData

processMessage TextDocumentDocumentSymbol msg@(MkRequestMessage id TextDocumentDocumentSymbol params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    whenNotDirty TextDocumentCodeAction (getResponseId msg) params.textDocument.uri $ do
      loadIfNeeded conf params.textDocument.uri Nothing

      documentSymbolData <- documentSymbol params
      sendResponseMessage TextDocumentDocumentSymbol $ Success (getResponseId msg) $ make documentSymbolData

processMessage TextDocumentCodeLens msg@(MkRequestMessage id TextDocumentCodeLens params) =
  whenNotShutdown $ whenInitialized $ \conf =>
    sendResponseMessage TextDocumentCodeLens (Success (getResponseId msg) (make MkNull))

processMessage TextDocumentCompletion msg@(MkRequestMessage id TextDocumentCompletion params) =
  whenNotShutdown $ whenInitialized $ \conf =>
    sendResponseMessage TextDocumentCompletion (Success (getResponseId msg) (make MkNull))

processMessage TextDocumentDocumentLink msg@(MkRequestMessage id TextDocumentDocumentLink params) =
  whenNotShutdown $ whenInitialized $ \conf =>
    sendResponseMessage TextDocumentDocumentLink (Success (getResponseId msg) (make MkNull))

processMessage TextDocumentSemanticTokensFull msg@(MkRequestMessage id TextDocumentSemanticTokensFull params) = do
  whenNotShutdown $ whenInitialized $ \conf => do
    whenNotDirty TextDocumentCodeAction (getResponseId msg) params.textDocument.uri $ do
      loadIfNeeded conf params.textDocument.uri Nothing
      md <- get MD
      src <- getSource
      logString Warning $ "MySrc: " ++ src
      let srcLines = forget $ lines src
      let getLineLength = \lineNum => maybe 0 (cast . length) $ elemAt srcLines (integerToNat (cast lineNum))
      let tokens = getSemanticTokens md getLineLength
      sendResponseMessage TextDocumentSemanticTokensFull $ Success (getResponseId msg) (make $ tokens)

processMessage {type = Request} method msg =
  whenNotShutdown $ whenInitialized $ \conf => do
    logString Warning $ "received a not supported \{show (toJSON method)} request"
    sendResponseMessage method (methodNotFound msg)

processMessage {type = Notification} method msg =
  whenNotShutdown $ whenInitialized $ \conf =>
    logString Warning "unhandled notification"

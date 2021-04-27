||| Functions for the processing of received request and notifications.
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
import Idris.ModTree
import Idris.Package
import Idris.Pretty
import Idris.REPL
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Idris.IDEMode.Holes
import Language.LSP.CodeAction.CaseSplit
import Language.JSON
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Capabilities
import Server.Configuration
import Server.Log
import Server.Response
import Server.Utils
import System

displayType : {auto c : Ref Ctxt Defs} ->
              {auto s : Ref Syn SyntaxInfo} ->
              Defs -> (Name, Int, GlobalDef) ->
              Core (Doc IdrisAnn)
displayType defs (n, i, gdef)
    = maybe (do tm <- resugar [] !(normaliseHoles defs [] (type gdef))
                pure (pretty !(aliasName (fullname gdef)) <++> colon <++> prettyTerm tm))
            (\num => prettyHole defs [] n num (type gdef))
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
  let Just fpath = path <$> toMaybe params.rootUri
    | _ => do logString Error $ "Error in initialize expected valid root uri"
              sendUnknownResponseMessage (invalidParams "Expected valid root uri")
  fname <- fromMaybe fpath <$> findIpkg (Just fpath)
  setSourceDir (Just fname)
  sendResponseMessage Initialize response
  update LSPConf (record {initialized = Just params})

processMessage Shutdown msg@(MkRequestMessage _ Shutdown _) = do
  -- In a future multithreaded model, we must guarantee that all pending request are still executed.
  let response = Success (getResponseId msg) (the (Maybe Null) Nothing)
  sendResponseMessage Shutdown response
  update LSPConf (record {isShutdown = True})

processMessage Exit (MkNotificationMessage Exit _) = do
  let status = if !(gets LSPConf isShutdown) then ExitSuccess else ExitFailure 1
  coreLift $ exitWith status

processMessage TextDocumentDidOpen msg@(MkNotificationMessage TextDocumentDidOpen params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    update LSPConf (record {openFile = Just (params.textDocument.uri, params.textDocument.version)})
    -- TODO: the compiler works directly with the file, we should upstream some changes to allow it to work with the
    --       source passed by the client.
    resetContext
    let fpath = params.textDocument.uri.path
    fname <- fromMaybe fpath <$> findIpkg (Just fpath)
    errs <- buildDeps fname -- FIXME: the compiler always dumps the errors on stdout, requires
                            --        a compiler change.
    setSource params.textDocument.text
    resetProofState
    sendDiagnostics params.textDocument.uri (Just params.textDocument.version) errs

processMessage TextDocumentDidSave msg@(MkNotificationMessage TextDocumentDidSave params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    update LSPConf (record {openFile = Just (params.textDocument.uri, 0)})
    -- TODO: same issue of TextDocumentDidOpen
    resetContext
    let fpath = params.textDocument.uri.path
    fname <- fromMaybe fpath <$> findIpkg (Just fpath)
    let Just source = params.text
      | _  => do logString Error $ "Error in textDocument/didSave expected format"
                 sendUnknownResponseMessage (invalidParams "Expected full text after save")
    errs <- buildDeps fname -- FIXME: add string version to the compiler (requires upstream change, probably)
                            -- FIXME: stop emitting errors on stdout (requires upstream change)
    logString Debug $ "errors : \{show errs}"
    setSource source
    resetProofState
    sendDiagnostics params.textDocument.uri Nothing errs

processMessage TextDocumentDidClose msg@(MkNotificationMessage TextDocumentDidClose params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    update LSPConf (record {openFile = Nothing})
    logString Info $ "File \{params.textDocument.uri.path} closed"

processMessage TextDocumentHover msg@(MkRequestMessage id TextDocumentHover params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    let fpath = params.textDocument.uri.path
    fname <- fromMaybe fpath <$> findIpkg (Just fpath)
    res <- loadMainFile fname

    defs <- get Ctxt
    meta <- get MD

    let name = findInTree (params.position.line, params.position.character) (nameLocMap meta)
    -- Lookup the name globally
    globals <- maybe (pure []) (\n => lookupCtxtName n (gamma defs)) name
    globalResult <- the (Core $ Maybe $ Doc IdrisAnn) $ case globals of
      [] => pure Nothing
      ts => do tys <- traverse (displayType defs) ts
               pure $ Just (vsep tys)
    localResult <- findTypeAt $ anyAt $ within (params.position.line, params.position.character)
    line <- case (globalResult, localResult) of
      -- Give precedence to the local name, as it shadows the others
      (_, Just (n, _, type)) => pure $ renderString $ unAnnotateS $ layoutUnbounded $ pretty (nameRoot n) <++> colon <++> !(displayTerm defs type)
      (Just globalDoc, Nothing) => pure $ renderString $ unAnnotateS $ layoutUnbounded globalDoc
      (Nothing, Nothing) => pure ""
    let response = Success (getResponseId msg) (make $ MkHover (make $ MkMarkupContent PlainText line) Nothing)
    sendResponseMessage TextDocumentHover response

processMessage TextDocumentCodeAction msg@(MkRequestMessage id TextDocumentCodeAction params) =
  whenNotShutdown $ whenInitialized $ \_ => do
    splitAction <- caseSplit (getResponseId msg) params
    let resp = maybe [] (\act => [make act]) splitAction
    sendResponseMessage TextDocumentCodeAction (Success (getResponseId msg) (Here resp))

processMessage {type = Request} method msg =
  whenNotShutdown $ whenInitialized $ \conf => do
    logString Warning $ "received a not supported \{show (toJSON method)} request"
    sendResponseMessage method (methodNotFound msg)
processMessage {type = Notification} method msg =
  whenNotShutdown $ whenInitialized $ \conf =>
    logString Warning "unhandled notification"

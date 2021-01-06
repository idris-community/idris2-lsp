||| Functions for the processing of received request and notifications.
|||
||| (C) The Idris Community, 2021
module Server.ProcessMessage

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Idris.ModTree
import Idris.Package
import Idris.Pretty
import Idris.REPL
import Idris.REPLOpts
import Idris.Resugar
import Idris.Syntax
import Language.JSON
import Language.LSP.Message
import Server.Capabilities
import Server.Configuration
import Server.Log
import Server.Response
import Server.Utils
import System

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
  setSourceDir (path <$> toMaybe params.rootUri)
  sendResponseMessage Initialize response
  update LSPConf (record {initialized = Just params})
processMessage Shutdown (MkRequestMessage _ Shutdown _) =
  -- In a future multithreaded model, we must guarantee that all pending request are still executed.
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
    setSourceDir (path <$> toMaybe conf.rootUri)
    errs <- buildDeps params.textDocument.uri.path -- FIXME: the compiler always dumps the errors on stdout, requires
                                                   --        a compiler change.
    setSource params.textDocument.text
    resetProofState
    sendDiagnostics params.textDocument.uri (Just params.textDocument.version) errs
processMessage TextDocumentDidChange msg@(MkNotificationMessage TextDocumentDidChange params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    update LSPConf (record {openFile = Just (params.textDocument.uri, params.textDocument.version)})
    -- TODO: same issue of TextDocumentDidOpen
    resetContext
    setSourceDir (path <$> toMaybe conf.rootUri)
    let (Left source :: _) = params.contentChanges
      | _  => do logString Error $ "Error in textDocument/didChange expected format"
                 sendUnknownResponseMessage (invalidParams "Expected one full TextDocumentContentChangeEvent")
    errs <- buildDeps params.textDocument.uri.path -- FIXME: add string version to the compiler (requires upstream change, probably)
                                                   -- FIXME: stop emitting errors on stdout (requires upstream change)
    setSource source.text
    resetProofState
    sendDiagnostics params.textDocument.uri (Just params.textDocument.version) errs
processMessage TextDocumentDidClose msg@(MkNotificationMessage TextDocumentDidClose params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    update LSPConf (record {openFile = Nothing})
    logString Info $ "File " ++ params.textDocument.uri.path ++ " closed"
processMessage TextDocumentHover msg@(MkRequestMessage id TextDocumentHover params) =
  whenNotShutdown $ whenInitialized $ \conf => do
    -- Temporary measure for examples
    let fpath = params.textDocument.uri.path
    fname <- fromMaybe fpath <$> findIpkg (Just fpath)
    res <- loadMainFile fname
    -- TODO: do something with the errors

    -- FIXME: Checks local definitions only because we currently do not store the FC of global name calls :(
    Just (n, num, tm) <- findTypeAt (\p, n => within (params.position.line - 1, params.position.character - 1) p)
      | Nothing => do let response = Success (getResponseId msg) (Left $ MkHover (Right $ Right $ MkMarkupContent PlainText "") Nothing)
                      sendResponseMessage TextDocumentHover response
    defs <- get Ctxt
    term <- resugar [] !(normaliseHoles defs [] tm)
    let typeline = renderString $ unAnnotateS $ layoutUnbounded $ pretty (nameRoot n) <++> colon <++> prettyTerm term
    let markup = MkMarkupContent PlainText typeline
    let response = Success (getResponseId msg) (Left $ MkHover (Right $ Right markup) Nothing)
    sendResponseMessage TextDocumentHover response
processMessage {type = Request} method msg =
  whenNotShutdown $ whenInitialized $ \conf => do
    logString Warning $ "received a not supported" ++ show (toJSON method) ++ " request"
    sendResponseMessage method (methodNotFound msg)
processMessage {type = Notification} method msg =
  whenNotShutdown $ whenInitialized $ \conf =>
    logString Warning "unhandled notification"

||| Helper functions for LSP server responses and notifications.
|||
||| (C) The Idris Community, 2021
module Server.Response

import Core.Context
import Core.Core
import Core.Env
import Core.FC
import Data.OneOf
import Data.String
import Idris.Pretty
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Language.JSON
import Language.LSP.Message
import Language.LSP.Utils
import Parser.Support
import Server.Configuration
import Server.Diagnostics
import Server.Log
import Server.Utils
import System.File

||| Header for messages on-the-wire.
||| By specification only Content-Length is mandatory.
header : Int -> String
header l = "Content-Length: \{show l}\{headerLineEnd}\{headerLineEnd}"

||| Response message for method not found or not implemented yet.
export
methodNotFound : ResponseError
methodNotFound = MkResponseError MethodNotFound "Method not implemented yet" JNull

||| Response message for error while parsing a new message received.
export
parseError : ResponseError
parseError = MkResponseError ParseError "Parse error" JNull

||| Response message for internal server errors.
export
internalError : String -> ResponseError
internalError msg = MkResponseError InternalError msg JNull

||| Response message for invalid requests.
export
invalidRequest : String -> ResponseError
invalidRequest msg = MkResponseError InvalidRequest msg JNull

||| Response message for invalid parameters to an otherwise valid method.
export
invalidParams : String -> ResponseError
invalidParams msg = MkResponseError InvalidParams msg JNull

||| Response message for messages received before initialization.
export
serverNotInitialized : ResponseError
serverNotInitialized = MkResponseError ServerNotInitialized "" JNull

writeResponse : Ref LSPConf LSPConfiguration
             => JSON
             -> Core ()
writeResponse msg = do
  let body = stringify msg
  let header = header (cast $ length body)
  outputHandle <- gets LSPConf outputHandle
  coreLift_ $ fPutStr outputHandle (header ++ body)
  coreLift_ $ fflush outputHandle

||| Sends a new notification from the server to the client.
export
sendNotificationMessage : Ref LSPConf LSPConfiguration
                       => (method : Method Server Notification)
                       -> (params : MessageParams method)
                       -> Core ()
sendNotificationMessage method params = do
  let msg = toJSON $ MkNotificationMessage method params
  writeResponse msg
  logI Channel "Sent notification message for method \{stringify $ toJSON method}"
  logD Channel "Notification sent: \{stringify msg}"

||| Sends a request from the server to the client ignoring the result
||| TODO when client sends response we fail to parse it
export
sendRequestMessage_ : Ref LSPConf LSPConfiguration
                   => (method : Method Server Request)
                   -> (params : MessageParams method)
                   -> Core ()
sendRequestMessage_ method params = do
  requestId <- gets LSPConf nextRequestId
  let msg = toJSON $ MkRequestMessage (make $ cast {to = Int} requestId) method params
  update LSPConf ({ nextRequestId := requestId + 1 })
  writeResponse (toJSON msg)
  logI Channel "Sent request message for method \{stringify $ toJSON method}"
  logD Channel "Request sent: \{stringify msg}"

||| Sends a response message to a request received from the client.
export
sendResponseMessage : Ref LSPConf LSPConfiguration
                   => (method : Method Client Request)
                   -> ResponseMessage method
                   -> Core ()
sendResponseMessage method resp = do
  let msg = toJSON resp
  writeResponse msg
  logI Channel "Sent response message for method \{stringify $ toJSON method}"
  logD Channel "Response sent: \{stringify msg}"

||| Sends an error response to an unknown method.
export
sendUnknownResponseMessage : Ref LSPConf LSPConfiguration => ResponseError -> Core ()
sendUnknownResponseMessage err = do
  -- The method type Initialize is irrelevant since the message is unknown, can use any method, the message would be the same.
  writeResponse (toJSON {a = ResponseMessage Initialize} (Failure (make MkNull) err))
  logI Channel "Sent response to unknown method"

||| Sends a LSP `Diagnostic` message for a given, potentially versioned, source
||| that produces some errors.
|||
||| @caps The client capabilities related to diagnostics
||| @uri The source URI.
||| @version Optional source version.
||| @errs Errors produced while trying to check the source.
export
sendDiagnostics : Ref LSPConf LSPConfiguration
               => Ref Syn SyntaxInfo
               => Ref Ctxt Defs
               => Ref ROpts REPLOpts
               => (caps : Maybe PublishDiagnosticsClientCapabilities)
               -> (uri : DocumentURI)
               -> (version : Maybe Int)
               -> (warnings : List Warning)
               -> (errs : List Error)
               -> Core ()
sendDiagnostics caps uri version warnings errs = do
  warningDiagnostics <- traverse (warningToDiagnostic caps uri) warnings
  errorDiagnostics <- traverse (errorToDiagnostic caps uri) errs
  let diagnostics = warningDiagnostics ++ errorDiagnostics
  let params = MkPublishDiagnosticsParams uri version diagnostics
  logI Diagnostic "Sending diagnostic message for \{show uri}"
  sendNotificationMessage TextDocumentPublishDiagnostics params

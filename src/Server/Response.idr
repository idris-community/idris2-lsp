||| Helper functions for LSP server responses and notifications.
|||
||| (C) The Idris Community, 2021
module Server.Response

import Core.Context
import Core.Core
import Core.Directory
import Core.Env
import Core.FC
import Data.OneOf
import Data.String
import Data.List
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
import System.Path

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

groupByFile : Ref Ctxt Defs => List Error -> Core (List (Maybe String, List1 Error))
groupByFile list = do
  withFileName <- traverse (\err => pure (!(getFilePath err), err)) list
  pure $ map pull (groupBy sameFileName withFileName)
 where
  pull : List1 (a, b) -> (a, List1 b)
  pull ((x, y) ::: []) = (x, singleton y)
  pull ((_, y) ::: t :: ts) = mapSnd (y `cons`) (pull (t ::: ts))

  sameFileName : (Maybe String, Error) -> (Maybe String, Error) -> Bool
  sameFileName = on (==) fst

  getFilePath : Error -> Core (Maybe String)
  getFilePath err = do
    defs <- get Ctxt
    let wdir = defs.options.dirs.working_dir
    let loc = getErrorLoc err
    traverseOpt (pure . (wdir </>) <=< nsToSource replFC) ((\case PhysicalIdrSrc ident => Just ident; _ => Nothing) . fst <=< isNonEmptyFC =<< loc)

||| Sends a LSP `Diagnostic` message for the whole project
|||
||| @caps The client capabilities related to diagnostics
||| @version Optional source version.
||| @errs Errors produced while trying to check the source.
||| @return list of files that contain at least one error
export
sendDiagnostics : Ref LSPConf LSPConfiguration
               => Ref Syn SyntaxInfo
               => Ref Ctxt Defs
               => Ref ROpts REPLOpts
               => (caps : Maybe PublishDiagnosticsClientCapabilities)
               -> (warnings : List Warning)
               -> (errs : List Error)
               -> Core (List DocumentURI)
sendDiagnostics caps warnings errs = do
  catMaybes <$> for !(groupByFile errs) mbForFile
 where
  forFile : DocumentURI -> List Error -> Core ()
  forFile uri errs = do
    warningDiagnostics <- traverse (warningToDiagnostic caps) warnings
    errorDiagnostics <- traverse (errorToDiagnostic caps) errs
    let diagnostics = warningDiagnostics ++ errorDiagnostics
    let params = MkPublishDiagnosticsParams uri Nothing diagnostics
    logI Diagnostic "Sending diagnostic message for \{show uri}"
    sendNotificationMessage TextDocumentPublishDiagnostics params

  mbForFile : (Maybe String, List1 Error) -> Core (Maybe DocumentURI)
  mbForFile (Nothing, _) = pure Nothing -- TODO: Currently silently suppressed, not good
  mbForFile (Just path, errs) = Just (pathToURI path) <$ forFile (pathToURI path) (forget errs)

export
sendEmptyDiagnostic : Ref LSPConf LSPConfiguration
                   => Ref Syn SyntaxInfo
                   => Ref Ctxt Defs
                   => Ref ROpts REPLOpts
                   => DocumentURI
                   -> Core ()
sendEmptyDiagnostic uri = do
  let params = MkPublishDiagnosticsParams uri Nothing []
  logI Diagnostic "Sending empty diagnostic message for \{show uri}"
  sendNotificationMessage TextDocumentPublishDiagnostics params

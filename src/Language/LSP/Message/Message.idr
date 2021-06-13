||| Definitions of messages and associated payloads and responses.
|||
||| (C) The Idris Community, 2021
module Language.LSP.Message.Message

import Data.OneOf
import Language.JSON
import Language.LSP.Message.CallHierarchy
import Language.LSP.Message.Cancel
import Language.LSP.Message.CodeAction
import Language.LSP.Message.CodeLens
import Language.LSP.Message.Command
import Language.LSP.Message.Completion
import Language.LSP.Message.Declaration
import Language.LSP.Message.Definition
import Language.LSP.Message.Derive
import Language.LSP.Message.Diagnostics
import Language.LSP.Message.DocumentColor
import Language.LSP.Message.DocumentFormatting
import Language.LSP.Message.DocumentHighlight
import Language.LSP.Message.DocumentLink
import Language.LSP.Message.DocumentSymbols
import Language.LSP.Message.FoldingRange
import Language.LSP.Message.Hover
import Language.LSP.Message.Implementation
import Language.LSP.Message.Initialize
import Language.LSP.Message.LinkedEditingRange
import Language.LSP.Message.Location
import Language.LSP.Message.Method
import Language.LSP.Message.Moniker
import Language.LSP.Message.Progress
import Language.LSP.Message.References
import Language.LSP.Message.Registration
import Language.LSP.Message.Rename
import Language.LSP.Message.SelectionRange
import Language.LSP.Message.SemanticTokens
import Language.LSP.Message.SignatureHelp
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Trace
import Language.LSP.Message.Utils
import Language.LSP.Message.Window
import Language.LSP.Message.Workspace
import Language.Reflection

%language ElabReflection
%default total

||| Maps the parameters associated to each type of method.
public export
MessageParams : (method : Method from type) -> Type
MessageParams Initialize                          = InitializeParams
MessageParams Initialized                         = InitializedParams
MessageParams Shutdown                            = Maybe Null
MessageParams Exit                                = Maybe Null
MessageParams SetTrace                            = SetTraceParams
MessageParams WindowWorkDoneProgressCancel        = WorkDoneProgressCancelParams
MessageParams WorkspaceDidChangeWorkspaceFolders  = DidChangeWorkspaceFoldersParams
MessageParams WorkspaceDidChangeConfiguration     = DidChangeConfigurationParams
MessageParams WorkspaceDidChangeWatchedFiles      = DidChangeWatchedFilesParams
MessageParams WorkspaceSymbol                     = WorkspaceSymbolParams
MessageParams WorkspaceExecuteCommand             = ExecuteCommandParams
MessageParams WorkspaceWillCreateFiles            = CreateFilesParams
MessageParams TextDocumentDidOpen                 = DidOpenTextDocumentParams
MessageParams TextDocumentDidChange               = DidChangeTextDocumentParams
MessageParams TextDocumentWillSave                = WillSaveTextDocumentParams
MessageParams TextDocumentWillSaveWaitUntil       = WillSaveTextDocumentParams
MessageParams TextDocumentDidSave                 = DidSaveTextDocumentParams
MessageParams TextDocumentDidClose                = DidCloseTextDocumentParams
MessageParams CompletionItemResolve               = CompletionItem
MessageParams TextDocumentHover                   = HoverParams
MessageParams TextDocumentSignatureHelp           = SignatureHelpParams
MessageParams TextDocumentDeclaration             = DeclarationParams
MessageParams TextDocumentDefinition              = DefinitionParams
MessageParams TextDocumentTypeDefinition          = TypeDefinitionParams
MessageParams TextDocumentImplementation          = ImplementationParams
MessageParams TextDocumentReferences              = ReferenceParams
MessageParams TextDocumentDocumentHighlight       = DocumentHighlightParams
MessageParams TextDocumentDocumentSymbol          = DocumentSymbolParams
MessageParams TextDocumentCodeAction              = CodeActionParams
MessageParams CodeActionResolve                   = CodeAction
MessageParams TextDocumentCodeLens                = CodeLensParams
MessageParams CodeLensResolve                     = CodeLens
MessageParams TextDocumentDocumentLink            = DocumentLinkParams
MessageParams DocumentLinkResolve                 = DocumentLink
MessageParams TextDocumentDocumentColor           = DocumentColorParams
MessageParams TextDocumentFormatting              = DocumentFormattingParams
MessageParams TextDocumentRangeFormatting         = DocumentRangeFormattingParams
MessageParams TextDocumentOnTypeFormatting        = DocumentOnTypeFormattingParams
MessageParams TextDocumentRename                  = RenameParams
MessageParams TextDocumentPrepareRename           = PrepareRenameParams
MessageParams TextDocumentFoldingRange            = FoldingRangeParams
MessageParams TextDocumentSelectionRange          = SelectionRangeParams
MessageParams TextDocumentPrepareCallHierarchy    = CallHierarchyParams
MessageParams CallHierarchyIncomingCalls          = CallHierarchyIncomingCallsParams
MessageParams CallHierarchyOutgoingCalls          = CallHierarchyOutgoingCallsParams
MessageParams TextDocumentSemanticTokensFull      = SemanticTokensParams
MessageParams TextDocumentSemanticTokensFullDelta = SemanticTokensDeltaParams
MessageParams TextDocumentSemanticTokensRange     = SemanticTokensRangeParams
MessageParams WorkspaceSemanticTokensRefresh      = Maybe Null
MessageParams TextDocumentLinkedEditingRange      = LinkedEditingRangeParams
MessageParams TextDocumentMoniker                 = MonikerParams
MessageParams LogTrace                            = LogTraceParams
MessageParams WindowShowMessage                   = ShowMessageParams
MessageParams WindowShowMessageRequest            = ShowMessageRequestParams
MessageParams WindowShowDocument                  = ShowDocumentParams
MessageParams WindowLogMessage                    = LogMessageParams
MessageParams WindowWorkDoneProgressCreate        = WorkDoneProgressCreateParams
MessageParams TelemetryEvent                      = JSON
MessageParams ClientRegisterCapability            = RegistrationParams
MessageParams ClientUnregisterCapability          = UnregistrationParams
MessageParams WorkspaceWorkspaceFolders           = Maybe Null
MessageParams WorkspaceConfiguration              = ConfigurationParams
MessageParams WorkspaceApplyEdit                  = ApplyWorkspaceEditParams
MessageParams TextDocumentPublishDiagnostics      = PublishDiagnosticsParams
MessageParams TextDocumentCompletion              = CompletionOptions
MessageParams WorkspaceCodeLensRefresh            = Maybe Null
MessageParams CancelRequest                       = CancelParams
MessageParams Progress                            = OneOf [WorkDoneProgressBegin, WorkDoneProgressReport, WorkDoneProgressEnd]

-- Hacky, but avoids having to carry a FromJSON (MessageParams method) inside sigma types
findParamsImpl : (method : Method from type) -> FromJSON (MessageParams method)
findParamsImpl Initialize                          = %search
findParamsImpl Initialized                         = %search
findParamsImpl Shutdown                            = %search
findParamsImpl Exit                                = %search
findParamsImpl SetTrace                            = %search
findParamsImpl WindowWorkDoneProgressCancel        = %search
findParamsImpl WorkspaceDidChangeWorkspaceFolders  = %search
findParamsImpl WorkspaceDidChangeConfiguration     = %search
findParamsImpl WorkspaceDidChangeWatchedFiles      = %search
findParamsImpl WorkspaceSymbol                     = %search
findParamsImpl WorkspaceExecuteCommand             = %search
findParamsImpl WorkspaceWillCreateFiles            = %search
findParamsImpl TextDocumentDidOpen                 = %search
findParamsImpl TextDocumentDidChange               = %search
findParamsImpl TextDocumentWillSave                = %search
findParamsImpl TextDocumentWillSaveWaitUntil       = %search
findParamsImpl TextDocumentDidSave                 = %search
findParamsImpl TextDocumentDidClose                = %search
findParamsImpl CompletionItemResolve               = %search
findParamsImpl TextDocumentHover                   = %search
findParamsImpl TextDocumentSignatureHelp           = %search
findParamsImpl TextDocumentDeclaration             = %search
findParamsImpl TextDocumentDefinition              = %search
findParamsImpl TextDocumentTypeDefinition          = %search
findParamsImpl TextDocumentImplementation          = %search
findParamsImpl TextDocumentReferences              = %search
findParamsImpl TextDocumentDocumentHighlight       = %search
findParamsImpl TextDocumentDocumentSymbol          = %search
findParamsImpl TextDocumentCodeAction              = %search
findParamsImpl CodeActionResolve                   = %search
findParamsImpl TextDocumentCodeLens                = %search
findParamsImpl CodeLensResolve                     = %search
findParamsImpl TextDocumentDocumentLink            = %search
findParamsImpl DocumentLinkResolve                 = %search
findParamsImpl TextDocumentDocumentColor           = %search
findParamsImpl TextDocumentFormatting              = %search
findParamsImpl TextDocumentRangeFormatting         = %search
findParamsImpl TextDocumentOnTypeFormatting        = %search
findParamsImpl TextDocumentRename                  = %search
findParamsImpl TextDocumentPrepareRename           = %search
findParamsImpl TextDocumentFoldingRange            = %search
findParamsImpl TextDocumentSelectionRange          = %search
findParamsImpl TextDocumentPrepareCallHierarchy    = %search
findParamsImpl CallHierarchyIncomingCalls          = %search
findParamsImpl CallHierarchyOutgoingCalls          = %search
findParamsImpl TextDocumentSemanticTokensFull      = %search
findParamsImpl TextDocumentSemanticTokensFullDelta = %search
findParamsImpl TextDocumentSemanticTokensRange     = %search
findParamsImpl WorkspaceSemanticTokensRefresh      = %search
findParamsImpl TextDocumentLinkedEditingRange      = %search
findParamsImpl TextDocumentMoniker                 = %search
findParamsImpl LogTrace                            = %search
findParamsImpl WindowShowMessage                   = %search
findParamsImpl WindowShowMessageRequest            = %search
findParamsImpl WindowShowDocument                  = %search
findParamsImpl WindowLogMessage                    = %search
findParamsImpl WindowWorkDoneProgressCreate        = %search
findParamsImpl TelemetryEvent                      = %search
findParamsImpl ClientRegisterCapability            = %search
findParamsImpl ClientUnregisterCapability          = %search
findParamsImpl WorkspaceWorkspaceFolders           = %search
findParamsImpl WorkspaceConfiguration              = %search
findParamsImpl WorkspaceApplyEdit                  = %search
findParamsImpl TextDocumentPublishDiagnostics      = %search
findParamsImpl TextDocumentCompletion              = %search
findParamsImpl WorkspaceCodeLensRefresh            = %search
findParamsImpl CancelRequest                       = %search
findParamsImpl Progress                            = %search

||| Maps the response associated to each type of method.
public export
ResponseResult : (method : Method from Request) -> Type
ResponseResult Initialize                          = InitializeResult
ResponseResult Shutdown                            = Maybe Null
ResponseResult WorkspaceSymbol                     = OneOf [List SymbolInformation, Null]
ResponseResult WorkspaceExecuteCommand             = JSON
ResponseResult WorkspaceWillCreateFiles            = OneOf [WorkspaceEdit, Null]
ResponseResult TextDocumentWillSaveWaitUntil       = OneOf [List TextEdit, Null]
ResponseResult CompletionItemResolve               = CompletionItem
ResponseResult TextDocumentHover                   = OneOf [Hover, Null]
ResponseResult TextDocumentSignatureHelp           = OneOf [SignatureHelp, Null]
ResponseResult TextDocumentDeclaration             = OneOf [Location, List Location, List LocationLink, Null]
ResponseResult TextDocumentDefinition              = OneOf [Location, List Location, List LocationLink, Null]
ResponseResult TextDocumentTypeDefinition          = OneOf [Location, List Location, List LocationLink, Null]
ResponseResult TextDocumentImplementation          = OneOf [Location, List Location, List LocationLink, Null]
ResponseResult TextDocumentReferences              = OneOf [List Location, Null]
ResponseResult TextDocumentDocumentHighlight       = OneOf [List DocumentHighlight, Null]
ResponseResult TextDocumentDocumentSymbol          = OneOf [List DocumentSymbol, List SymbolInformation, Null]
ResponseResult TextDocumentCodeAction              = OneOf [List (OneOf [Command, CodeAction]), Null]
ResponseResult CodeActionResolve                   = CodeAction
ResponseResult TextDocumentCodeLens                = OneOf [List CodeLens, Null]
ResponseResult CodeLensResolve                     = Maybe Null
ResponseResult TextDocumentDocumentLink            = OneOf [List DocumentLink, Null]
ResponseResult DocumentLinkResolve                 = DocumentLink
ResponseResult TextDocumentDocumentColor           = List ColorInformation
ResponseResult TextDocumentFormatting              = OneOf [List TextEdit, Null]
ResponseResult TextDocumentRangeFormatting         = OneOf [List TextEdit, Null]
ResponseResult TextDocumentOnTypeFormatting        = OneOf [List TextEdit, Null]
ResponseResult TextDocumentRename                  = OneOf [WorkspaceEdit, Null]
ResponseResult TextDocumentPrepareRename           = OneOf [Range, PrepareRenamePlaceholderResponse, PrepareRenameDefaultResponse, Null]
ResponseResult TextDocumentFoldingRange            = OneOf [List FoldingRange, Null]
ResponseResult TextDocumentSelectionRange          = OneOf [List SelectionRange, Null]
ResponseResult TextDocumentPrepareCallHierarchy    = OneOf [List CallHierarchyItem, Null]
ResponseResult CallHierarchyIncomingCalls          = OneOf [List CallHierarchyIncomingCall, Null]
ResponseResult CallHierarchyOutgoingCalls          = OneOf [List CallHierarchyOutgoingCall, Null]
ResponseResult TextDocumentSemanticTokensFull      = OneOf [SemanticTokens, Null]
ResponseResult TextDocumentSemanticTokensFullDelta = OneOf [SemanticTokens, SemanticTokensDelta, Null]
ResponseResult TextDocumentSemanticTokensRange     = OneOf [SemanticTokens, Null]
ResponseResult WorkspaceSemanticTokensRefresh      = Maybe Null
ResponseResult TextDocumentLinkedEditingRange      = OneOf [LinkedEditingRanges, Null]
ResponseResult TextDocumentMoniker                 = OneOf [List Moniker, Null]
ResponseResult WindowShowMessageRequest            = OneOf [MessageActionItem, Null]
ResponseResult WindowShowDocument                  = ShowDocumentResult
ResponseResult WindowWorkDoneProgressCreate        = Maybe Null
ResponseResult ClientRegisterCapability            = Maybe Null
ResponseResult ClientUnregisterCapability          = Maybe Null
ResponseResult WorkspaceWorkspaceFolders           = OneOf [List WorkspaceFolder, Null]
ResponseResult WorkspaceConfiguration              = List JSON
ResponseResult WorkspaceApplyEdit                  = ApplyWorkspaceEditResponse
ResponseResult TextDocumentCompletion              = OneOf [List CompletionItem, CompletionList, Null]
ResponseResult WorkspaceCodeLensRefresh            = Maybe Null

findNotificationImpl : (method : Method from Notification) -> ToJSON (MessageParams method)
findNotificationImpl Initialized = %search
findNotificationImpl Exit = %search
findNotificationImpl SetTrace = %search
findNotificationImpl WindowWorkDoneProgressCancel = %search
findNotificationImpl WorkspaceDidChangeWorkspaceFolders = %search
findNotificationImpl WorkspaceDidChangeConfiguration = %search
findNotificationImpl WorkspaceDidChangeWatchedFiles = %search
findNotificationImpl TextDocumentDidOpen = %search
findNotificationImpl TextDocumentDidChange = %search
findNotificationImpl TextDocumentWillSave = %search
findNotificationImpl TextDocumentDidSave = %search
findNotificationImpl TextDocumentDidClose = %search
findNotificationImpl LogTrace = %search
findNotificationImpl WindowShowMessage = %search
findNotificationImpl WindowLogMessage = %search
findNotificationImpl TelemetryEvent = %search
findNotificationImpl TextDocumentPublishDiagnostics = %search
findNotificationImpl CancelRequest = %search
findNotificationImpl Progress = %search

findRequestImpl : (method : Method from Request) -> ToJSON (MessageParams method)
findRequestImpl Initialize = %search
findRequestImpl Shutdown = %search
findRequestImpl WorkspaceSymbol = %search
findRequestImpl WorkspaceExecuteCommand = %search
findRequestImpl WorkspaceWillCreateFiles = %search
findRequestImpl TextDocumentWillSaveWaitUntil = %search
findRequestImpl CompletionItemResolve = %search
findRequestImpl TextDocumentHover = %search
findRequestImpl TextDocumentSignatureHelp = %search
findRequestImpl TextDocumentDeclaration = %search
findRequestImpl TextDocumentDefinition = %search
findRequestImpl TextDocumentTypeDefinition = %search
findRequestImpl TextDocumentImplementation = %search
findRequestImpl TextDocumentReferences = %search
findRequestImpl TextDocumentDocumentHighlight = %search
findRequestImpl TextDocumentDocumentSymbol = %search
findRequestImpl TextDocumentCodeAction = %search
findRequestImpl CodeActionResolve = %search
findRequestImpl TextDocumentCodeLens = %search
findRequestImpl CodeLensResolve = %search
findRequestImpl TextDocumentDocumentLink = %search
findRequestImpl DocumentLinkResolve = %search
findRequestImpl TextDocumentDocumentColor = %search
findRequestImpl TextDocumentFormatting = %search
findRequestImpl TextDocumentRangeFormatting = %search
findRequestImpl TextDocumentOnTypeFormatting = %search
findRequestImpl TextDocumentRename = %search
findRequestImpl TextDocumentPrepareRename = %search
findRequestImpl TextDocumentFoldingRange = %search
findRequestImpl TextDocumentSelectionRange = %search
findRequestImpl TextDocumentPrepareCallHierarchy = %search
findRequestImpl CallHierarchyIncomingCalls = %search
findRequestImpl CallHierarchyOutgoingCalls = %search
findRequestImpl TextDocumentSemanticTokensFull = %search
findRequestImpl TextDocumentSemanticTokensFullDelta = %search
findRequestImpl TextDocumentSemanticTokensRange = %search
findRequestImpl WorkspaceSemanticTokensRefresh = %search
findRequestImpl TextDocumentLinkedEditingRange = %search
findRequestImpl TextDocumentMoniker = %search
findRequestImpl WindowShowMessageRequest = %search
findRequestImpl WindowShowDocument = %search
findRequestImpl WindowWorkDoneProgressCreate = %search
findRequestImpl ClientRegisterCapability = %search
findRequestImpl ClientUnregisterCapability = %search
findRequestImpl WorkspaceWorkspaceFolders = %search
findRequestImpl WorkspaceConfiguration = %search
findRequestImpl WorkspaceApplyEdit = %search
findRequestImpl TextDocumentCompletion = %search
findRequestImpl WorkspaceCodeLensRefresh = %search

findResultImpl : (method : Method from Request) -> ToJSON (ResponseResult method)
findResultImpl Initialize = %search
findResultImpl Shutdown = %search
findResultImpl WorkspaceSymbol = %search
findResultImpl WorkspaceExecuteCommand = %search
findResultImpl WorkspaceWillCreateFiles = %search
findResultImpl TextDocumentWillSaveWaitUntil = %search
findResultImpl CompletionItemResolve = %search
findResultImpl TextDocumentHover = %search
findResultImpl TextDocumentSignatureHelp = %search
findResultImpl TextDocumentDeclaration = %search
findResultImpl TextDocumentDefinition = %search
findResultImpl TextDocumentTypeDefinition = %search
findResultImpl TextDocumentImplementation = %search
findResultImpl TextDocumentReferences = %search
findResultImpl TextDocumentDocumentHighlight = %search
findResultImpl TextDocumentDocumentSymbol = %search
findResultImpl TextDocumentCodeAction = %search
findResultImpl CodeActionResolve = %search
findResultImpl TextDocumentCodeLens = %search
findResultImpl CodeLensResolve = %search
findResultImpl TextDocumentDocumentLink = %search
findResultImpl DocumentLinkResolve = %search
findResultImpl TextDocumentDocumentColor = %search
findResultImpl TextDocumentFormatting = %search
findResultImpl TextDocumentRangeFormatting = %search
findResultImpl TextDocumentOnTypeFormatting = %search
findResultImpl TextDocumentRename = %search
findResultImpl TextDocumentPrepareRename = %search
findResultImpl TextDocumentFoldingRange = %search
findResultImpl TextDocumentSelectionRange = %search
findResultImpl TextDocumentPrepareCallHierarchy = %search
findResultImpl CallHierarchyIncomingCalls = %search
findResultImpl CallHierarchyOutgoingCalls = %search
findResultImpl TextDocumentSemanticTokensFull = %search
findResultImpl TextDocumentSemanticTokensFullDelta = %search
findResultImpl TextDocumentSemanticTokensRange = %search
findResultImpl WorkspaceSemanticTokensRefresh = %search
findResultImpl TextDocumentLinkedEditingRange = %search
findResultImpl TextDocumentMoniker = %search
findResultImpl WindowShowMessageRequest = %search
findResultImpl WindowShowDocument = %search
findResultImpl WindowWorkDoneProgressCreate = %search
findResultImpl ClientRegisterCapability = %search
findResultImpl ClientUnregisterCapability = %search
findResultImpl WorkspaceWorkspaceFolders = %search
findResultImpl WorkspaceConfiguration = %search
findResultImpl WorkspaceApplyEdit = %search
findResultImpl TextDocumentCompletion = %search
findResultImpl WorkspaceCodeLensRefresh = %search

||| Parse parameters
||| Since the params are sometimes optional we must parse Maybe JSON
||| TODO hacky replace with something better
export
fromMaybeJSONParameters : (method : Method from type) -> Maybe JSON -> Maybe (MessageParams method)
fromMaybeJSONParameters Exit arg =
  pure $ join $ arg >>= (fromJSON @{findParamsImpl Exit})
fromMaybeJSONParameters Shutdown arg =
  pure $ join $ arg >>= (fromJSON @{findParamsImpl Shutdown})
fromMaybeJSONParameters WorkspaceSemanticTokensRefresh arg =
  pure $ join $ arg >>= (fromJSON @{findParamsImpl WorkspaceSemanticTokensRefresh})
fromMaybeJSONParameters WorkspaceWorkspaceFolders arg =
  pure $ join $ arg >>= (fromJSON @{findParamsImpl WorkspaceWorkspaceFolders})
fromMaybeJSONParameters WorkspaceCodeLensRefresh arg =
  pure $ join $ arg >>= (fromJSON @{findParamsImpl WorkspaceCodeLensRefresh})
fromMaybeJSONParameters method arg = arg >>= (fromJSON @{findParamsImpl method})

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#notificationMessage
public export
data NotificationMessage : Method from Notification -> Type where
  MkNotificationMessage : (method : Method from Notification)
                       -> (params : MessageParams method)
                       -> NotificationMessage method

export
{method : Method from Notification} -> ToJSON (NotificationMessage method) where
  toJSON (MkNotificationMessage method params) =
    JObject ([("jsonrpc", JString "2.0"), ("method", toJSON method), ("params", toJSON @{findNotificationImpl method} params)])

export
FromJSON (from ** method : Method from Notification ** NotificationMessage method) where
  fromJSON (JObject arg) = do
    lookup "jsonrpc" arg >>= (guard . (== JString "2.0"))
    (from ** meth) <- lookup "method" arg >>= fromJSON {a = (from ** Method from Notification)}
    case meth of
      Exit => do let par = lookup "params" arg >>= (fromJSON @{findParamsImpl meth})
                 pure (from ** meth ** MkNotificationMessage meth (join par))
      _ => do par <- lookup "params" arg >>= (fromJSON @{findParamsImpl meth})
              pure (from ** meth ** MkNotificationMessage meth par)
  fromJSON _ = neutral

namespace NotificationMessage
  export
  method : {0 m : Method from Notification} -> NotificationMessage m -> Method from Notification
  method (MkNotificationMessage m _) = m

  export
  params : {0 method : Method from Notification} -> NotificationMessage method -> MessageParams method
  params (MkNotificationMessage _ p) = p

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#requestMessage
public export
data RequestMessage : Method from Request -> Type where
  MkRequestMessage : (id : OneOf [Int, String])
                  -> (method : Method from Request)
                  -> (params : MessageParams method)
                  -> RequestMessage method

export
{method : Method from Request} -> ToJSON (RequestMessage method) where
  toJSON (MkRequestMessage id method params) =
    JObject ([("jsonrpc", JString "2.0"), ("id", toJSON id), ("method", toJSON method), ("params", toJSON @{findRequestImpl method} params)])

export
FromJSON (from ** method : Method from Request ** RequestMessage method) where
  fromJSON (JObject arg) = do
    lookup "jsonrpc" arg >>= (guard . (== JString "2.0"))
    id <- lookup "id" arg >>= fromJSON
    (from ** method) <- lookup "method" arg >>= fromJSON {a = (from ** Method from Request)}
    case method of
      Shutdown => do let params = lookup "params" arg >>= (fromJSON @{findParamsImpl method})
                     pure (from ** method ** MkRequestMessage id method (join params))
      WorkspaceSemanticTokensRefresh =>
        do let params = lookup "params" arg >>= (fromJSON @{findParamsImpl method})
           pure (from ** method ** MkRequestMessage id method (join params))
      WorkspaceWorkspaceFolders =>
        do let params = lookup "params" arg >>= (fromJSON @{findParamsImpl method})
           pure (from ** method ** MkRequestMessage id method (join params))
      WorkspaceCodeLensRefresh =>
        do let params = lookup "params" arg >>= (fromJSON @{findParamsImpl method})
           pure (from ** method ** MkRequestMessage id method (join params))
      _ => do params <- lookup "params" arg >>= (fromJSON @{findParamsImpl method})
              pure (from ** method ** MkRequestMessage id method params)
  fromJSON _ = neutral

namespace RequestMessage
  export
  id : RequestMessage m -> OneOf [Int, String]
  id (MkRequestMessage i _ _) = i

  export
  method : {0 m : Method from Request} -> RequestMessage m -> Method from Request
  method (MkRequestMessage _ m _) = m

  export
  params : {0 method : Method from Request} -> RequestMessage method -> MessageParams method
  params (MkRequestMessage _ _ p) = p

||| Maps the message payload to each type of method.
public export
Message : (type : MethodType) -> (Method from type -> Type)
Message Notification = NotificationMessage
Message Request = RequestMessage

export
FromJSON (from ** type ** method : Method from type ** Message type method) where
  fromJSON arg =
    (fromJSON arg >>= \(f ** m ** msg) : (from ** method : Method from Request ** RequestMessage method) => pure (f ** _ ** m ** msg))
      <|> (fromJSON arg >>= \(f ** m ** msg) : (from ** method : Method from Notification ** NotificationMessage method) => pure (f ** _ ** m ** msg))

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#responseMessage
namespace ErrorCodes
  public export
  data ErrorCodes
    = ParseError
    | InvalidRequest
    | MethodNotFound
    | InvalidParams
    | InternalError
    | ServerNotInitialized
    | UnknownErrorCode
    | ContentModified
    | RequestCancelled
    | JSONRPCReserved Int
    | LSPReserved Int
    | Custom Int

export
ToJSON ErrorCodes where
  toJSON ParseError             = JNumber (-32700)
  toJSON InvalidRequest         = JNumber (-32600)
  toJSON MethodNotFound         = JNumber (-32601)
  toJSON InvalidParams          = JNumber (-32602)
  toJSON InternalError          = JNumber (-32603)
  toJSON ServerNotInitialized   = JNumber (-32002)
  toJSON UnknownErrorCode       = JNumber (-32001)
  toJSON ContentModified        = JNumber (-32801)
  toJSON RequestCancelled       = JNumber (-32800)
  toJSON (JSONRPCReserved code) = JNumber (cast code)
  toJSON (LSPReserved code)     = JNumber (cast code)
  toJSON (Custom code)          = JNumber (cast code)

export
FromJSON ErrorCodes where
  -- TODO: Can't match on negative numbers :(, temporary fix until compiler PR.
  fromJSON (JNumber code) =
    if code == (-32700) then pure ParseError
    else if code == (-32600) then pure InvalidRequest
    else if code == (-32601) then pure MethodNotFound
    else if code == (-32602) then pure InvalidParams
    else if code == (-32603) then pure InternalError
    else if code == (-32002) then pure ServerNotInitialized
    else if code == (-32001) then pure UnknownErrorCode
    else if code == (-32801) then pure ContentModified
    else if code == (-32800) then pure RequestCancelled
    else if (-32099) <= code && code <= (-32000) then pure (JSONRPCReserved $ cast code)
    else if (-32899) <= code && code <= (-32800) then pure (LSPReserved $ cast code)
    else pure (Custom $ cast code)
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#responseMessage
public export
record ResponseError where
  constructor MkResponseError
  code : ErrorCodes
  message : String
  data_ : JSON
%runElab deriveJSON (record {renames = [("data_", "data")]} defaultOpts) `{{ResponseError}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#responseMessage
public export
data ResponseMessage : Method from type -> Type where
  Success : (id : OneOf [Int, String, Null]) -> (result : ResponseResult method) -> ResponseMessage method
  Failure : (id : OneOf [Int, String, Null]) -> (error : ResponseError) -> ResponseMessage method

export
{method : Method from Request} -> ToJSON (ResponseMessage method) where
  toJSON (Success id result) =
    JObject ([("jsonrpc", JString "2.0"), ("id", toJSON id), ("result", toJSON @{findResultImpl method} result)])
  toJSON (Failure id error) =
    JObject ([("jsonrpc", JString "2.0"), ("id", toJSON id), ("error", toJSON error)])

export
FromJSON (ResponseResult method) => FromJSON (ResponseMessage method) where
  fromJSON (JObject arg) = do
    lookup "jsonrpc" arg >>= (guard . (== JString "2.0"))
    id <- lookup "id" arg >>= fromJSON
    case lookup "result" arg of
         Just v => Success id <$> fromJSON v
         Nothing => Failure id <$> (lookup "error" arg >>= fromJSON)
  fromJSON _ = neutral

namespace ResponseMessage
  export
  id : ResponseMessage method -> OneOf [Int, String, Null]
  id (Success i _) = i
  id (Failure i _) = i

  export
  getResponseId : RequestMessage method -> OneOf [Int, String, Null]
  getResponseId = extend . RequestMessage.id

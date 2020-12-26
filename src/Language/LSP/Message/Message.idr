||| Definitions of messages and associated payloads and responses.
|||
||| (C) The Idris Community, 2020
module Language.LSP.Message.Message

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
MessageParams Shutdown                            = Null
MessageParams Exit                                = Null
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
MessageParams WorkspaceSemanticTokensRefresh      = Null
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
MessageParams WorkspaceWorkspaceFolders           = Null
MessageParams WorkspaceConfiguration              = ConfigurationParams
MessageParams WorkspaceApplyEdit                  = ApplyWorkspaceEditParams
MessageParams TextDocumentPublishDiagnostics      = PublishDiagnosticsParams
MessageParams TextDocumentCompletion              = CompletionOptions
MessageParams WorkspaceCodeLensRefresh            = Null
MessageParams CancelRequest                       = CancelParams
MessageParams Progress                            = WorkDoneProgressBegin .+. WorkDoneProgressReport .+. WorkDoneProgressEnd

-- Hacky, but avoids having to carry a FromJSON (MessageParams method) inside sigma types
findImpl : (method : Method from type) -> FromJSON (MessageParams method)
findImpl Initialize = %search
findImpl Initialized = %search
findImpl Shutdown = %search
findImpl Exit = %search
findImpl SetTrace = %search
findImpl WindowWorkDoneProgressCancel = %search
findImpl WorkspaceDidChangeWorkspaceFolders = %search
findImpl WorkspaceDidChangeConfiguration = %search
findImpl WorkspaceDidChangeWatchedFiles = %search
findImpl WorkspaceSymbol = %search
findImpl WorkspaceExecuteCommand = %search
findImpl WorkspaceWillCreateFiles = %search
findImpl TextDocumentDidOpen = %search
findImpl TextDocumentDidChange = %search
findImpl TextDocumentWillSave = %search
findImpl TextDocumentWillSaveWaitUntil = %search
findImpl TextDocumentDidSave = %search
findImpl TextDocumentDidClose = %search
findImpl CompletionItemResolve = %search
findImpl TextDocumentHover = %search
findImpl TextDocumentSignatureHelp = %search
findImpl TextDocumentDeclaration = %search
findImpl TextDocumentDefinition = %search
findImpl TextDocumentTypeDefinition = %search
findImpl TextDocumentImplementation = %search
findImpl TextDocumentReferences = %search
findImpl TextDocumentDocumentHighlight = %search
findImpl TextDocumentDocumentSymbol = %search
findImpl TextDocumentCodeAction = %search
findImpl CodeActionResolve = %search
findImpl TextDocumentCodeLens = %search
findImpl CodeLensResolve = %search
findImpl TextDocumentDocumentLink = %search
findImpl DocumentLinkResolve = %search
findImpl TextDocumentDocumentColor = %search
findImpl TextDocumentFormatting = %search
findImpl TextDocumentRangeFormatting = %search
findImpl TextDocumentOnTypeFormatting = %search
findImpl TextDocumentRename = %search
findImpl TextDocumentPrepareRename = %search
findImpl TextDocumentFoldingRange = %search
findImpl TextDocumentSelectionRange = %search
findImpl TextDocumentPrepareCallHierarchy = %search
findImpl CallHierarchyIncomingCalls = %search
findImpl CallHierarchyOutgoingCalls = %search
findImpl TextDocumentSemanticTokensFull = %search
findImpl TextDocumentSemanticTokensFullDelta = %search
findImpl TextDocumentSemanticTokensRange = %search
findImpl WorkspaceSemanticTokensRefresh = %search
findImpl TextDocumentLinkedEditingRange = %search
findImpl TextDocumentMoniker = %search
findImpl LogTrace = %search
findImpl WindowShowMessage = %search
findImpl WindowShowMessageRequest = %search
findImpl WindowShowDocument = %search
findImpl WindowLogMessage = %search
findImpl WindowWorkDoneProgressCreate = %search
findImpl TelemetryEvent = %search
findImpl ClientRegisterCapability = %search
findImpl ClientUnregisterCapability = %search
findImpl WorkspaceWorkspaceFolders = %search
findImpl WorkspaceConfiguration = %search
findImpl WorkspaceApplyEdit = %search
findImpl TextDocumentPublishDiagnostics = %search
findImpl TextDocumentCompletion = %search
findImpl WorkspaceCodeLensRefresh = %search
findImpl CancelRequest = %search
findImpl Progress = %search

||| Maps the response associated to each type of method.
public export
ResponseResult : (method : Method from Request) -> Type
ResponseResult Initialize                          = InitializeResult
ResponseResult Shutdown                            = Null
ResponseResult WorkspaceSymbol                     = List SymbolInformation .+. Null
ResponseResult WorkspaceExecuteCommand             = JSON
ResponseResult WorkspaceWillCreateFiles            = WorkspaceEdit .+. Null
ResponseResult TextDocumentWillSaveWaitUntil       = List TextEdit .+. Null
ResponseResult CompletionItemResolve               = CompletionItem
ResponseResult TextDocumentHover                   = Hover .+. Null
ResponseResult TextDocumentSignatureHelp           = SignatureHelp .+. Null
ResponseResult TextDocumentDeclaration             = Location .+. List Location .+. List LocationLink .+. Null
ResponseResult TextDocumentDefinition              = Location .+. List Location .+. List LocationLink .+. Null
ResponseResult TextDocumentTypeDefinition          = Location .+. List Location .+. List LocationLink .+. Null
ResponseResult TextDocumentImplementation          = Location .+. List Location .+. List LocationLink .+. Null
ResponseResult TextDocumentReferences              = List Location .+. Null
ResponseResult TextDocumentDocumentHighlight       = List DocumentHighlight .+. Null
ResponseResult TextDocumentDocumentSymbol          = List DocumentSymbol .+. SymbolInformation .+. Null
ResponseResult TextDocumentCodeAction              = List (Command .+. CodeAction) .+. Null
ResponseResult CodeActionResolve                   = CodeAction
ResponseResult TextDocumentCodeLens                = List CodeLens .+. Null
ResponseResult CodeLensResolve                     = Null
ResponseResult TextDocumentDocumentLink            = List DocumentLink .+. Null
ResponseResult DocumentLinkResolve                 = DocumentLink
ResponseResult TextDocumentDocumentColor           = List ColorInformation
ResponseResult TextDocumentFormatting              = List TextEdit .+. Null
ResponseResult TextDocumentRangeFormatting         = List TextEdit .+. Null
ResponseResult TextDocumentOnTypeFormatting        = List TextEdit .+. Null
ResponseResult TextDocumentRename                  = WorkspaceEdit .+. Null
ResponseResult TextDocumentPrepareRename           = Range .+. PrepareRenamePlaceholderResponse .+. PrepareRenameDefaultResponse .+. Null
ResponseResult TextDocumentFoldingRange            = List FoldingRange .+. Null
ResponseResult TextDocumentSelectionRange          = List SelectionRange .+. Null
ResponseResult TextDocumentPrepareCallHierarchy    = List CallHierarchyItem .+. Null
ResponseResult CallHierarchyIncomingCalls          = List CallHierarchyIncomingCall .+. Null
ResponseResult CallHierarchyOutgoingCalls          = List CallHierarchyOutgoingCall .+. Null
ResponseResult TextDocumentSemanticTokensFull      = SemanticTokens .+. Null
ResponseResult TextDocumentSemanticTokensFullDelta = SemanticTokens .+. SemanticTokensDelta .+. Null
ResponseResult TextDocumentSemanticTokensRange     = SemanticTokens .+. Null
ResponseResult WorkspaceSemanticTokensRefresh      = Null
ResponseResult TextDocumentLinkedEditingRange      = LinkedEditingRanges .+. Null
ResponseResult TextDocumentMoniker                 = List Moniker .+. Null
ResponseResult WindowShowMessageRequest            = MessageActionItem .+. Null
ResponseResult WindowShowDocument                  = ShowDocumentResult
ResponseResult WindowWorkDoneProgressCreate        = Null
ResponseResult ClientRegisterCapability            = Null
ResponseResult ClientUnregisterCapability          = Null
ResponseResult WorkspaceWorkspaceFolders           = List WorkspaceFolder .+. Null
ResponseResult WorkspaceConfiguration              = List JSON
ResponseResult WorkspaceApplyEdit                  = ApplyWorkspaceEditResponse
ResponseResult TextDocumentCompletion              = List CompletionItem .+. CompletionList .+. Null
ResponseResult WorkspaceCodeLensRefresh            = Null

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#notificationMessage
public export
data NotificationMessage : Method from Notification -> Type where
  MkNotificationMessage : (method : Method from Notification)
                       -> (params : MessageParams method)
                       -> NotificationMessage method

export
ToJSON (MessageParams method) => ToJSON (NotificationMessage method) where
  toJSON (MkNotificationMessage m p) =
    JObject [("jsonrpc", JString "2.0"), ("method", toJSON m), ("params", toJSON p)]

export
FromJSON (from ** method : Method from Notification ** NotificationMessage method) where
  fromJSON (JObject arg) = do
    lookup "jsonrpc" arg >>= (guard . (== JString "2.0"))
    (from ** meth) <- lookup "method" arg >>= fromJSON {a = (from ** Method from Notification)}
    par <- lookup "params" arg >>= (fromJSON @{findImpl meth})
    pure (from ** meth ** MkNotificationMessage meth par)
  fromJSON _ = neutral

namespace NotificationMessage
  export
  method : {0 m : Method from Notification} -> NotificationMessage m -> Method from Notification
  method (MkNotificationMessage m _) = m

  export
  params : {0 m : Method from Notification} -> NotificationMessage m -> MessageParams m
  params (MkNotificationMessage _ p) = p

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#requestMessage
public export
data RequestMessage : Method from Request -> Type where
  MkRequestMessage : (id : Int .+. String)
                  -> (method : Method from Request)
                  -> (params : MessageParams method)
                  -> RequestMessage method

export
ToJSON (MessageParams method) => ToJSON (RequestMessage method) where
  toJSON (MkRequestMessage i m p) =
    JObject [("jsonrpc", JString "2.0"), ("id", toJSON i), ("method", toJSON m), ("params", toJSON p)]

export
FromJSON (from ** method : Method from Request ** RequestMessage method) where
  fromJSON (JObject arg) = do
    lookup "jsonrpc" arg >>= (guard . (== JString "2.0"))
    id_ <- lookup "id" arg >>= fromJSON
    (from ** meth) <- lookup "method" arg >>= fromJSON {a = (from ** Method from Request)}
    par <- lookup "params" arg >>= (fromJSON @{findImpl meth})
    pure (from ** meth ** MkRequestMessage id_ meth par)
  fromJSON _ = neutral

namespace RequestMessage
  export
  id : RequestMessage m -> Int .+. String
  id (MkRequestMessage id_ _ _) = id_

  export
  method : {0 m : Method from Request} -> RequestMessage m -> Method from Request
  method (MkRequestMessage _ m _) = m

  export
  params : {0 m : Method from Request} -> RequestMessage m -> MessageParams m
  params (MkRequestMessage _ _ p) = p

||| Maps the message payload to each type of method.
public export
Message : (type : MethodType) -> (Method from type -> Type)
Message Notification = NotificationMessage
Message Request = RequestMessage

export
FromJSON (from ** type ** method : Method from type ** Message type method) where
  fromJSON arg =
    (fromJSON arg >>= \(f ** m ** msg) : (from ** method : Method from Notification ** NotificationMessage method) => pure (f ** _ ** m ** msg))
      <|> (fromJSON arg >>= \(f ** m ** msg) : (from ** method : Method from Request ** RequestMessage method) => pure (f ** _ ** m ** msg))

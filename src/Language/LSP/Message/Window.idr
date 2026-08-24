module Language.LSP.Message.Window

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

namespace MessageType
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
  public export
  data MessageType = Error | Warning | Info | Log

export
ToJSON MessageType where
  toJSON Error   = JInteger 1
  toJSON Warning = JInteger 2
  toJSON Info    = JInteger 3
  toJSON Log     = JInteger 4

export
FromJSON MessageType where
  fromJSON (JInteger 1) = pure Error
  fromJSON (JInteger 2) = pure Warning
  fromJSON (JInteger 3) = pure Info
  fromJSON (JInteger 4) = pure Log
  fromJSON _ = fail "invalid message type, [1-4]"

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
public export
record ShowMessageParams where
  constructor MkShowMessageParams
  type    : MessageType
  message : String
%runElab derive "ShowMessageParams" [FromJSON, ToJSON]

namespace ShowMessageRequestClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
  public export
  record ShowMessageActionItem where
    constructor MkShowMessageActionItem
    additionalPropertiesSupport : Maybe Bool
  %runElab derive "ShowMessageActionItem" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
public export
record ShowMessageRequestClientCapabilities where
  constructor MkShowMessageRequestClientCapabilities
  messageActionItem : Maybe ShowMessageActionItem
%runElab derive "ShowMessageRequestClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
public export
record MessageActionItem where
  constructor MkMessageActionItem
  title : String
%runElab derive "MessageActionItem" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
public export
record ShowMessageRequestParams where
  constructor MkShowMessageRequestParams
  type    : MessageType
  message : String
  actions : Maybe (List MessageActionItem)
%runElab derive "ShowMessageRequestParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showDocument
public export
record ShowDocumentClientCapabilities where
  constructor MkShowDocumentClientCapabilities
  support : Bool
%runElab derive "ShowDocumentClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showDocument
public export
record ShowDocumentParams where
  constructor MkShowDocumentParams
  uri       : URI
  external_ : Maybe Bool
  takeFocus : Maybe Bool
  selection : Maybe Range
%runElab derive "ShowDocumentParams" [FromJSONLSP, ToJSONLSP]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showDocument
public export
record ShowDocumentResult where
  constructor MkShowDocumentResult
  success : Bool
%runElab derive "ShowDocumentResult" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_logMessage
public export
record LogMessageParams where
  constructor MkLogMessageParams
  type    : MessageType
  message : String
%runElab derive "LogMessageParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_workDoneProgress_create
public export
record WorkDoneProgressCreateParams where
  constructor MkWorkDoneProgressCreateParams
  token : ProgressToken
%runElab derive "WorkDoneProgressCreateParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_workDoneProgress_cancel
public export
record WorkDoneProgressCancelParams where
  constructor MkWorkDoneProgressCancelParams
  token : ProgressToken
%runElab derive "WorkDoneProgressCancelParams" [FromJSON, ToJSON]

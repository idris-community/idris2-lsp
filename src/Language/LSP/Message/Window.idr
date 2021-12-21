module Language.LSP.Message.Window

import Language.JSON
import Language.LSP.Message.Derive
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
  toJSON Error   = JNumber 1
  toJSON Warning = JNumber 2
  toJSON Info    = JNumber 3
  toJSON Log     = JNumber 4

export
FromJSON MessageType where
  fromJSON (JNumber 1) = pure Error
  fromJSON (JNumber 2) = pure Warning
  fromJSON (JNumber 3) = pure Info
  fromJSON (JNumber 4) = pure Log
  fromJSON _ = Nothing

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
public export
record ShowMessageParams where
  constructor MkShowMessageParams
  type    : MessageType
  message : String
%runElab deriveJSON defaultOpts `{ShowMessageParams}

namespace ShowMessageRequestClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
  public export
  record ShowMessageActionItem where
    constructor MkShowMessageActionItem
    additionalPropertiesSupport : Maybe Bool
  %runElab deriveJSON defaultOpts `{ShowMessageActionItem}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
public export
record ShowMessageRequestClientCapabilities where
  constructor MkShowMessageRequestClientCapabilities
  messageActionItem : Maybe ShowMessageActionItem
%runElab deriveJSON defaultOpts `{ShowMessageRequestClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
public export
record MessageActionItem where
  constructor MkMessageActionItem
  title : String
%runElab deriveJSON defaultOpts `{MessageActionItem}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showMessage
public export
record ShowMessageRequestParams where
  constructor MkShowMessageRequestParams
  type    : MessageType
  message : String
  actions : Maybe (List MessageActionItem)
%runElab deriveJSON defaultOpts `{ShowMessageRequestParams}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showDocument
public export
record ShowDocumentClientCapabilities where
  constructor MkShowDocumentClientCapabilities
  support : Bool
%runElab deriveJSON defaultOpts `{ShowDocumentClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showDocument
public export
record ShowDocumentParams where
  constructor MkShowDocumentParams
  uri       : URI
  external_ : Maybe Bool
  takeFocus : Maybe Bool
  selection : Maybe Range
%runElab deriveJSON ({renames := [("external_", "external")]} defaultOpts) `{ShowDocumentParams}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_showDocument
public export
record ShowDocumentResult where
  constructor MkShowDocumentResult
  success : Bool
%runElab deriveJSON defaultOpts `{ShowDocumentResult}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_logMessage
public export
record LogMessageParams where
  constructor MkLogMessageParams
  type    : MessageType
  message : String
%runElab deriveJSON defaultOpts `{LogMessageParams}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_workDoneProgress_create
public export
record WorkDoneProgressCreateParams where
  constructor MkWorkDoneProgressCreateParams
  token : ProgressToken
%runElab deriveJSON defaultOpts `{WorkDoneProgressCreateParams}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#window_workDoneProgress_cancel
public export
record WorkDoneProgressCancelParams where
  constructor MkWorkDoneProgressCancelParams
  token : ProgressToken
%runElab deriveJSON defaultOpts `{WorkDoneProgressCancelParams}

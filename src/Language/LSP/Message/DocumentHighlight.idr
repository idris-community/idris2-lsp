module Language.LSP.Message.DocumentHighlight

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
public export
record DocumentHighlightClientCapabilities where
  constructor MkDocumentHighlightClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentHighlightClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
public export
record DocumentHighlightOptions where
  constructor MkDocumentHighlightOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentHighlightOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
public export
record DocumentHighlightRegistrationOptions where
  constructor MkDocumentHighlightRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : DocumentSelector .+. Null
%runElab deriveJSON defaultOpts `{{DocumentHighlightRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
public export
record DocumentHighlightParams where
  constructor MkDocumentHighlightParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab deriveJSON defaultOpts `{{DocumentHighlightParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
namespace DocumentHighlightKind
  public export
  data DocumentHighlightKind = Text | Read | Write

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
export
ToJSON DocumentHighlightKind where
  toJSON Text  = JNumber 1
  toJSON Read  = JNumber 2
  toJSON Write = JNumber 3

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
export
FromJSON DocumentHighlightKind where
  fromJSON (JNumber 1) = pure Text
  fromJSON (JNumber 2) = pure Read
  fromJSON (JNumber 3) = pure Write
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
public export
record DocumentHighlight where
  constructor MkDocumentHighlight
  range : Range
  kind  : Maybe DocumentHighlightKind
%runElab deriveJSON defaultOpts `{{DocumentHighlight}}

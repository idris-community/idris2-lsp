module Language.LSP.Message.DocumentHighlight

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total
%hide Text.Bounds.Position

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
public export
record DocumentHighlightClientCapabilities where
  constructor MkDocumentHighlightClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab derive "DocumentHighlightClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
public export
record DocumentHighlightOptions where
  constructor MkDocumentHighlightOptions
  workDoneProgress : Maybe Bool
%runElab derive "DocumentHighlightOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
public export
record DocumentHighlightRegistrationOptions where
  constructor MkDocumentHighlightRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab derive "DocumentHighlightRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
public export
record DocumentHighlightParams where
  constructor MkDocumentHighlightParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab derive "DocumentHighlightParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
namespace DocumentHighlightKind
  public export
  data DocumentHighlightKind = Text | Read | Write

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
export
ToJSON DocumentHighlightKind where
  toJSON Text  = JInteger 1
  toJSON Read  = JInteger 2
  toJSON Write = JInteger 3

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
export
FromJSON DocumentHighlightKind where
  fromJSON (JInteger 1) = pure Text
  fromJSON (JInteger 2) = pure Read
  fromJSON (JInteger 3) = pure Write
  fromJSON _ = fail "not a document highlight, 1|2|3"

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentHighlight
public export
record DocumentHighlight where
  constructor MkDocumentHighlight
  range : Range
  kind  : Maybe DocumentHighlightKind
%runElab derive "DocumentHighlight" [FromJSON, ToJSON]

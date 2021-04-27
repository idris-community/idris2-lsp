module Language.LSP.Message.LinkedEditingRange

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_linkedEditingRange
public export
record LinkedEditingRangeClientCapabilities where
  constructor MkLinkedEditingRangeClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab deriveJSON defaultOpts `{{LinkedEditingRangeClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_linkedEditingRange
public export
record LinkedEditingRangeOptions where
  constructor MkLinkedEditingRangesOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{LinkedEditingRangeOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_linkedEditingRange
public export
record LinkedEditingRangeRegistrationOptions where
  constructor MkLinkedEditingRangesRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe Bool
%runElab deriveJSON defaultOpts `{{LinkedEditingRangeRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_linkedEditingRange
public export
record LinkedEditingRangeParams where
  constructor MkLinkedEditingRangesParams
  workDoneToken : Maybe ProgressToken
  textDocument  : TextDocumentIdentifier
  position      : Position
%runElab deriveJSON defaultOpts `{{LinkedEditingRangeParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_linkedEditingRange
public export
record LinkedEditingRanges where
  constructor MkLinkedEditingRanges
  ranges      : List Range
  wordPattern : Maybe String
%runElab deriveJSON defaultOpts `{{LinkedEditingRanges}}

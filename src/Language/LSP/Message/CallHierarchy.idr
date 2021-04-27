module Language.LSP.Message.CallHierarchy

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.DocumentSymbols
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareCallHierarchy
public export
record CallHierarchyClientCapabilities where
  constructor MkCallHierarchyClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab deriveJSON defaultOpts `{{CallHierarchyClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareCallHierarchy
public export
record CallHierarchyOptions where
  constructor MkCallHierarchyOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{CallHierarchyOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareCallHierarchy
public export
record CallHierarchyRegistrationOptions where
  constructor MkCallHierarchyRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe String
%runElab deriveJSON defaultOpts `{{CallHierarchyRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareCallHierarchy
public export
record CallHierarchyParams where
  constructor MkCallHierarchyParams
  workDoneToken : Maybe ProgressToken
  textDocument  : TextDocumentIdentifier
%runElab deriveJSON defaultOpts `{{CallHierarchyParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareCallHierarchy
public export
record CallHierarchyItem where
  constructor MkCallHierarchyItem
  name           : String
  kind           : SymbolKind
  tags           : Maybe (List SymbolTag)
  detail         : Maybe String
  uri            : DocumentURI
  range          : Range
  selectionRange : Range
  data_          : Maybe JSON
%runElab deriveJSON (record {renames = [("data_", "data")]} defaultOpts) `{{CallHierarchyItem}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#callHierarchy_incomingCalls
public export
record CallHierarchyIncomingCallsParams where
  constructor MkCallHierarchyIncomingCallsParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  item               : CallHierarchyItem
%runElab deriveJSON defaultOpts `{{CallHierarchyIncomingCallsParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#callHierarchy_incomingCalls
public export
record CallHierarchyIncomingCall where
  constructor MkCallHierarchyIncomingCall
  from       : CallHierarchyItem
  fromRanges : List Range
%runElab deriveJSON defaultOpts `{{CallHierarchyIncomingCall}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#callHierarchy_outgoingCalls
public export
record CallHierarchyOutgoingCallsParams where
  constructor MkCallHierarchyOutgoingCallsParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  item               : CallHierarchyItem
%runElab deriveJSON defaultOpts `{{CallHierarchyOutgoingCallsParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#callHierarchy_outgoingCalls
public export
record CallHierarchyOutgoingCall where
  constructor MkCallHierarchyOutgoingCall
  to         : CallHierarchyItem
  fromRanges : List Range
%runElab deriveJSON defaultOpts `{{CallHierarchyOutgoingCall}}

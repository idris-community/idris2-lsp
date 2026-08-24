module Language.LSP.Message.CallHierarchy

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
%runElab derive "CallHierarchyClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareCallHierarchy
public export
record CallHierarchyOptions where
  constructor MkCallHierarchyOptions
  workDoneProgress : Maybe Bool
%runElab derive "CallHierarchyOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareCallHierarchy
public export
record CallHierarchyRegistrationOptions where
  constructor MkCallHierarchyRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe String
%runElab derive "CallHierarchyRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareCallHierarchy
public export
record CallHierarchyParams where
  constructor MkCallHierarchyParams
  workDoneToken : Maybe ProgressToken
  textDocument  : TextDocumentIdentifier
%runElab derive "CallHierarchyParams" [FromJSON, ToJSON]

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

%runElab derive "CallHierarchyItem" [FromJSONLSP, ToJSONLSP]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#callHierarchy_incomingCalls
public export
record CallHierarchyIncomingCallsParams where
  constructor MkCallHierarchyIncomingCallsParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  item               : CallHierarchyItem
%runElab derive "CallHierarchyIncomingCallsParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#callHierarchy_incomingCalls
public export
record CallHierarchyIncomingCall where
  constructor MkCallHierarchyIncomingCall
  from       : CallHierarchyItem
  fromRanges : List Range
%runElab derive "CallHierarchyIncomingCall" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#callHierarchy_outgoingCalls
public export
record CallHierarchyOutgoingCallsParams where
  constructor MkCallHierarchyOutgoingCallsParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  item               : CallHierarchyItem
%runElab derive "CallHierarchyOutgoingCallsParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#callHierarchy_outgoingCalls
public export
record CallHierarchyOutgoingCall where
  constructor MkCallHierarchyOutgoingCall
  to         : CallHierarchyItem
  fromRanges : List Range
%runElab derive "CallHierarchyOutgoingCall" [FromJSON, ToJSON]

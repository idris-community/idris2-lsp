module Language.LSP.Message.References

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_references
public export
record ReferenceClientCapabilities where
  constructor MkReferenceClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab derive "ReferenceClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_references
public export
record ReferenceOptions where
  constructor MkReferenceOptions
  workDoneProgress : Maybe Bool
%runElab derive "ReferenceOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_references
public export
record ReferenceRegistrationOptions where
  constructor MkReferenceRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab derive "ReferenceRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_references
public export
record ReferenceContext where
  constructor MkReferenceContext
  includeDeclaration : Bool
%runElab derive "ReferenceContext" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_references
public export
record ReferenceParams where
  constructor MkReferenceParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
  context            : ReferenceContext
%runElab derive "ReferenceParams" [FromJSON, ToJSON]

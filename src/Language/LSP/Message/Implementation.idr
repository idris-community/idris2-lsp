module Language.LSP.Message.Implementation

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total
%hide Text.Bounds.Position

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_implementation
public export
record ImplementationClientCapabilities where
  constructor MkImplementationClientCapabilities
  dynamicRegistration : Maybe Bool
  linkSupport         : Maybe Bool
%runElab derive "ImplementationClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_implementation
public export
record ImplementationOptions where
  constructor MkImplementationOptions
  workDoneProgress : Maybe Bool
%runElab derive "ImplementationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_implementation
public export
record ImplementationRegistrationOptions where
  constructor MkImplementationRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe String
%runElab derive "ImplementationRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_implementation
public export
record ImplementationParams where
  constructor MkImplementationParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab derive "ImplementationParams" [FromJSON, ToJSON]

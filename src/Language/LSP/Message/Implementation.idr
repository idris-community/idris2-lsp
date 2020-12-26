module Language.LSP.Message.Implementation

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_implementation
public export
record ImplementationClientCapabilities where
  constructor MkImplementationClientCapabilities
  dynamicRegistration : Maybe Bool
  linkSupport         : Maybe Bool
%runElab deriveJSON defaultOpts `{{ImplementationClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_implementation
public export
record ImplementationOptions where
  constructor MkImplementationOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{ImplementationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_implementation
public export
record ImplementationRegistrationOptions where
  constructor MkImplementationRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : DocumentSelector .+. Null
  id               : Maybe String
%runElab deriveJSON defaultOpts `{{ImplementationRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_implementation
public export
record ImplementationParams where
  constructor MkImplementationParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab deriveJSON defaultOpts `{{ImplementationParams}}

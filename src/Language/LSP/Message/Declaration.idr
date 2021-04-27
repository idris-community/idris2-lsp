module Language.LSP.Message.Declaration

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_declaration
public export
record DeclarationClientCapabilities where
  constructor MkDeclarationClientCapabilities
  dynamicRegistration : Maybe Bool
  linkSupport         : Maybe Bool
%runElab deriveJSON defaultOpts `{{DeclarationClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_declaration
public export
record DeclarationOptions where
  constructor MkDeclarationOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{DeclarationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_declaration
public export
record DeclarationRegistrationOptions where
  constructor MkDeclarationRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe String
%runElab deriveJSON defaultOpts `{{DeclarationRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_declaration
public export
record DeclarationParams where
  constructor MkDeclarationParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab deriveJSON defaultOpts `{{DeclarationParams}}

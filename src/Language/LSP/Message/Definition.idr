module Language.LSP.Message.Definition

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_definition
public export
record DefinitionClientCapabilities where
  constructor MkDefinitionClientCapabilities
  dynamicRegistration : Maybe Bool
  linkSupport         : Maybe Bool
%runElab deriveJSON defaultOpts `{{DefinitionClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_definition
public export
record DefinitionOptions where
  constructor MkDefinitionOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{DefinitionOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_definition
public export
record DefinitionRegistrationOptions where
  constructor MkDefinitionRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab deriveJSON defaultOpts `{{DefinitionRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_definition
public export
record DefinitionParams where
  constructor MkDefinitionParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab deriveJSON defaultOpts `{{DefinitionParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_typeDefinition
public export
record TypeDefinitionClientCapabilities where
  constructor MkTypeDefinitionClientCapabilities
  dynamicRegistration : Maybe Bool
  linkSupport         : Maybe Bool
%runElab deriveJSON defaultOpts `{{TypeDefinitionClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_typeDefinition
public export
record TypeDefinitionOptions where
  constructor MkTypeDefinitionOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{TypeDefinitionOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_typeDefinition
public export
record TypeDefinitionRegistrationOptions where
  constructor MkTypeDefinitionRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe String
%runElab deriveJSON defaultOpts `{{TypeDefinitionRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_typeDefinition
public export
record TypeDefinitionParams where
  constructor MkTypeDefinitionParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab deriveJSON defaultOpts `{{TypeDefinitionParams}}

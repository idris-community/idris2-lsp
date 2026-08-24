module Language.LSP.Message.Definition

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total
%hide Text.Bounds.Position

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_definition
public export
record DefinitionClientCapabilities where
  constructor MkDefinitionClientCapabilities
  dynamicRegistration : Maybe Bool
  linkSupport         : Maybe Bool
%runElab derive "DefinitionClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_definition
public export
record DefinitionOptions where
  constructor MkDefinitionOptions
  workDoneProgress : Maybe Bool
%runElab derive "DefinitionOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_definition
public export
record DefinitionRegistrationOptions where
  constructor MkDefinitionRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab derive "DefinitionRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_definition
public export
record DefinitionParams where
  constructor MkDefinitionParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab derive "DefinitionParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_typeDefinition
public export
record TypeDefinitionClientCapabilities where
  constructor MkTypeDefinitionClientCapabilities
  dynamicRegistration : Maybe Bool
  linkSupport         : Maybe Bool
%runElab derive "TypeDefinitionClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_typeDefinition
public export
record TypeDefinitionOptions where
  constructor MkTypeDefinitionOptions
  workDoneProgress : Maybe Bool
%runElab derive "TypeDefinitionOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_typeDefinition
public export
record TypeDefinitionRegistrationOptions where
  constructor MkTypeDefinitionRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe String
%runElab derive "TypeDefinitionRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_typeDefinition
public export
record TypeDefinitionParams where
  constructor MkTypeDefinitionParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab derive "TypeDefinitionParams" [FromJSON, ToJSON]

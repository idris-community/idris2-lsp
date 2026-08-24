module Language.LSP.Message.Declaration

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total
%hide Text.Bounds.Position

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_declaration
public export
record DeclarationClientCapabilities where
  constructor MkDeclarationClientCapabilities
  dynamicRegistration : Maybe Bool
  linkSupport         : Maybe Bool
%runElab derive "DeclarationClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_declaration
public export
record DeclarationOptions where
  constructor MkDeclarationOptions
  workDoneProgress : Maybe Bool
%runElab derive "DeclarationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_declaration
public export
record DeclarationRegistrationOptions where
  constructor MkDeclarationRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe String
%runElab derive "DeclarationRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_declaration
public export
record DeclarationParams where
  constructor MkDeclarationParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab derive "DeclarationParams" [FromJSON, ToJSON]

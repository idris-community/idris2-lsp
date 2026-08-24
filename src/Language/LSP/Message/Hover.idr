module Language.LSP.Message.Hover

import Language.LSP.Message.Location
import Language.LSP.Message.Markup
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total
%hide Text.Bounds.Position

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
record HoverClientCapabilities where
  constructor MkHoverClientCapabilities
  dynamicRegistration : Maybe Bool
  contentFormat : Maybe (List MarkupKind)
%runElab derive "HoverClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
record HoverOptions where
  constructor MkHoverOptions
  workDoneProgress : Maybe Bool
%runElab derive "HoverOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
record HoverRegistrationOptions where
  constructor MkHoverRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab derive "HoverRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
record HoverParams where
  constructor MkHoverParams
  workDoneToken : Maybe ProgressToken
  textDocument : TextDocumentIdentifier
  position : Position
%runElab derive "HoverParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
record Hover where
  constructor MkHover
  contents : OneOf [MarkedString, List MarkedString, MarkupContent]
  range : Maybe Range
%runElab derive "Hover" [FromJSON, ToJSON]

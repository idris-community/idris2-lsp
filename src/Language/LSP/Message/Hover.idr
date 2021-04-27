module Language.LSP.Message.Hover

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Markup
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
record HoverClientCapabilities where
  constructor MkHoverClientCapabilities
  dynamicRegistration : Maybe Bool
  contentFormat : Maybe (List MarkupKind)
%runElab deriveJSON defaultOpts `{{HoverClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
record HoverOptions where
  constructor MkHoverOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{HoverOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
record HoverRegistrationOptions where
  constructor MkHoverRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab deriveJSON defaultOpts `{{HoverRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
record HoverParams where
  constructor MkHoverParams
  workDoneToken : Maybe ProgressToken
  textDocument : TextDocumentIdentifier
  position : Position
%runElab deriveJSON defaultOpts `{{HoverParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
record Hover where
  constructor MkHover
  contents : OneOf [MarkedString, List MarkedString, MarkupContent]
  range : Maybe Range
%runElab deriveJSON defaultOpts `{{Hover}}

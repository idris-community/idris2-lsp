module Language.LSP.Message.DocumentLink

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentLink
public export
record DocumentLinkClientCapabilities where
  constructor MkDocumentLinkClientCapabilities
  dynamicRegistration : Maybe Bool
  tooltipSupport      : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentLinkClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentLink
public export
record DocumentLinkOptions where
  constructor MkDocumentLinkOptions
  workDoneProgress : Maybe Bool
  resolveProvider  : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentLinkOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentLink
public export
record DocumentLinkRegistrationOptions where
  constructor MkDocumentLinkRegistrationOptions
  workDoneProgress : Maybe Bool
  resolveProvider  : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab deriveJSON defaultOpts `{{DocumentLinkRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentLink
public export
record DocumentLinkParams where
  constructor MkDocumentLinkParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
%runElab deriveJSON defaultOpts `{{DocumentLinkParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentLink
public export
record DocumentLink where
  constructor MkDocumentLink
  range   : Range
  target  : Maybe DocumentURI
  tooltip : Maybe String
  data_   : Maybe JSON
%runElab deriveJSON (record {renames = [("data_", "data")]} defaultOpts) `{{DocumentLink}}

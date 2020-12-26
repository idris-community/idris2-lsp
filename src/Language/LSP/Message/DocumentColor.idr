module Language.LSP.Message.DocumentColor

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.LSP.Message.Workspace
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record DocumentColorClientCapabilities where
  constructor MkDocumentColorClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentColorClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record DocumentColorOptions where
  constructor MkDocumentColorOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentColorOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record DocumentColorRegistrationOptions where
  constructor MkDocumentColorRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : DocumentSelector .+. Null
  id               : Maybe String
%runElab deriveJSON defaultOpts `{{DocumentColorRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record DocumentColorParams where
  constructor MkDocumentColorParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
%runElab deriveJSON defaultOpts `{{DocumentColorParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record Color where
  constructor MkColor
  red   : Double
  green : Double
  blue  : Double
  alpha : Double
%runElab deriveJSON defaultOpts `{{Color}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record ColorInformation where
  constructor MkColorInformation
  range : Range
  color : Color
%runElab deriveJSON defaultOpts `{{ColorInformation}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_colorPresentation
public export
record ColorPresentationParams where
  constructor MkColorPresentationParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  color              : Color
  range              : Range
%runElab deriveJSON defaultOpts `{{ColorPresentationParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_colorPresentation
public export
record ColorPresentation where
  constructor MkColorPresentation
  label               : String
  textEdit            : Maybe TextEdit
  additionalTextEdits : Maybe (List TextEdit)
%runElab deriveJSON defaultOpts `{{ColorPresentation}}

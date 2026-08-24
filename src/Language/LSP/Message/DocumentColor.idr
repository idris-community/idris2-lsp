module Language.LSP.Message.DocumentColor

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
%runElab derive "DocumentColorClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record DocumentColorOptions where
  constructor MkDocumentColorOptions
  workDoneProgress : Maybe Bool
%runElab derive "DocumentColorOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record DocumentColorRegistrationOptions where
  constructor MkDocumentColorRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe String
%runElab derive "DocumentColorRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record DocumentColorParams where
  constructor MkDocumentColorParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
%runElab derive "DocumentColorParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record Color where
  constructor MkColor
  red   : Double
  green : Double
  blue  : Double
  alpha : Double
%runElab derive "Color" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentColor
public export
record ColorInformation where
  constructor MkColorInformation
  range : Range
  color : Color
%runElab derive "ColorInformation" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_colorPresentation
public export
record ColorPresentationParams where
  constructor MkColorPresentationParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  color              : Color
  range              : Range
%runElab derive "ColorPresentationParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_colorPresentation
public export
record ColorPresentation where
  constructor MkColorPresentation
  label               : String
  textEdit            : Maybe TextEdit
  additionalTextEdits : Maybe (List TextEdit)
%runElab derive "ColorPresentation" [FromJSON, ToJSON]

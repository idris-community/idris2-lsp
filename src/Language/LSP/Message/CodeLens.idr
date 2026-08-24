module Language.LSP.Message.CodeLens

import Language.LSP.Message.Command
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeLens
public export
record CodeLensClientCapabilities where
  constructor MkCodeLensClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab derive "CodeLensClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeLens
public export
record CodeLensOptions where
  constructor MkCodeLensOptions
  workDoneProgress : Maybe Bool
  resolveProvider  : Maybe Bool
%runElab derive "CodeLensOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeLens
public export
record CodeLensRegistrationOptions where
  constructor MkCodeLensRegistrationOptions
  workDoneProgress : Maybe Bool
  resolveProvider  : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab derive "CodeLensRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeLens
public export
record CodeLensParams where
  constructor MkCodeLensParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
%runElab derive "CodeLensParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeLens
public export
record CodeLens where
  constructor MkCodeLens
  range   : Range
  command : Maybe Command
  data_   : Maybe JSON
%runElab derive "CodeLens" [FromJSONLSP, ToJSONLSP]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#codeLens_refresh
public export
record CodeLensWorkspaceClientCapabilities where
  constructor MkCodeLensWorkspaceClientCapabilities
  refreshSupport : Maybe Bool
%runElab derive "CodeLensWorkspaceClientCapabilities" [FromJSON, ToJSON]

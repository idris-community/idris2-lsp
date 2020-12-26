module Language.LSP.Message.CodeLens

import Language.JSON
import Language.LSP.Message.Command
import Language.LSP.Message.Derive
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
%runElab deriveJSON defaultOpts `{{CodeLensClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeLens
public export
record CodeLensOptions where
  constructor MkCodeLensOptions
  workDoneProgress : Maybe Bool
  resolveProvider  : Maybe Bool
%runElab deriveJSON defaultOpts `{{CodeLensOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeLens
public export
record CodeLensRegistrationOptions where
  constructor MkCodeLensRegistrationOptions
  workDoneProgress : Maybe Bool
  resolveProvider  : Maybe Bool
  documentSelector : DocumentSelector .+. Null
%runElab deriveJSON defaultOpts `{{CodeLensRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeLens
public export
record CodeLensParams where
  constructor MkCodeLensParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
%runElab deriveJSON defaultOpts `{{CodeLensParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeLens
public export
record CodeLens where
  constructor MkCodeLens
  range   : Range
  command : Maybe Command
  data_   : Maybe JSON
%runElab deriveJSON (record {renames = [("data_", "data")]} defaultOpts) `{{CodeLens}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#codeLens_refresh
public export
record CodeLensWorkspaceClientCapabilities where
  constructor MkCodeLensWorkspaceClientCapabilities
  refreshSupport : Maybe Bool
%runElab deriveJSON defaultOpts `{{CodeLensWorkspaceClientCapabilities}}

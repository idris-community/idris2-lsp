module Language.LSP.Message.Diagnostics

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

namespace DiagnosticTag
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#diagnostic
  public export
  data DiagnosticTag = Unnecessary | Deprecated

export
ToJSON DiagnosticTag where
  toJSON Unnecessary = JNumber 1
  toJSON Deprecated  = JNumber 2

export
FromJSON DiagnosticTag where
  fromJSON (JNumber 1) = pure Unnecessary
  fromJSON (JNumber 2) = pure Deprecated
  fromJSON _ = neutral

namespace DiagnosticSeverity
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#diagnostic
  public export
  data DiagnosticSeverity = Error | Warning | Information | Hint

export
ToJSON DiagnosticSeverity where
  toJSON Error       = JNumber 1
  toJSON Warning     = JNumber 2
  toJSON Information = JNumber 3
  toJSON Hint        = JNumber 4

export
FromJSON DiagnosticSeverity where
  fromJSON (JNumber 1) = pure Error
  fromJSON (JNumber 2) = pure Warning
  fromJSON (JNumber 3) = pure Information
  fromJSON (JNumber 4) = pure Hint
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#diagnostic
public export
record DiagnosticRelatedInformation where
  constructor MkDiagnosticRelatedInformation
  location : Location
  message  : String
%runElab deriveJSON defaultOpts `{DiagnosticRelatedInformation}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#diagnostic
public export
record CodeDescription where
  constructor MkCodeDescription
  href : URI
%runElab deriveJSON defaultOpts `{CodeDescription}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#diagnostic
public export
record Diagnostic where
  constructor MkDiagnostic
  range              : Range
  severity           : Maybe DiagnosticSeverity
  code               : Maybe (OneOf [Int, String])
  codeDescription    : Maybe CodeDescription
  source             : Maybe String
  message            : String
  tags               : Maybe (List DiagnosticTag)
  relatedInformation : Maybe (List DiagnosticRelatedInformation)
  data_              : Maybe JSON
%runElab deriveJSON ({renames := [("data_", "data")]} defaultOpts) `{Diagnostic}

namespace PublishDiagnosticsClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_publishDiagnostics
  public export
  record TagSupport where
    constructor MkTagSupport
    valueSet : List DiagnosticTag
  %runElab deriveJSON defaultOpts `{TagSupport}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_publishDiagnostics
public export
record PublishDiagnosticsClientCapabilities where
  constructor MkPublishDiagnosticsClientCapabilities
  relatedInformation     : Maybe Bool
  tagSupport             : Maybe TagSupport
  versionSupport         : Maybe Bool
  codeDescriptionSupport : Maybe Bool
  dataSupport            : Maybe Bool
%runElab deriveJSON defaultOpts `{PublishDiagnosticsClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_publishDiagnostics
public export
record PublishDiagnosticsParams where
  constructor MkPublishDiagnosticsParams
  uri         : DocumentURI
  version     : Maybe Int
  diagnostics : List Diagnostic
%runElab deriveJSON defaultOpts `{PublishDiagnosticsParams}

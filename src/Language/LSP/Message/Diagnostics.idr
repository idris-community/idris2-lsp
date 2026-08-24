module Language.LSP.Message.Diagnostics

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
  toJSON Unnecessary = JInteger 1
  toJSON Deprecated  = JInteger 2

export
FromJSON DiagnosticTag where
  fromJSON (JInteger 1) = pure Unnecessary
  fromJSON (JInteger 2) = pure Deprecated
  fromJSON _ = fail "not a diagnostic tag, 1|2"

namespace DiagnosticSeverity
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#diagnostic
  public export
  data DiagnosticSeverity = Error | Warning | Information | Hint

export
ToJSON DiagnosticSeverity where
  toJSON Error       = JInteger 1
  toJSON Warning     = JInteger 2
  toJSON Information = JInteger 3
  toJSON Hint        = JInteger 4

export
FromJSON DiagnosticSeverity where
  fromJSON (JInteger 1) = pure Error
  fromJSON (JInteger 2) = pure Warning
  fromJSON (JInteger 3) = pure Information
  fromJSON (JInteger 4) = pure Hint
  fromJSON _ = fail "not a diagnostic severity [1-4]"

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#diagnostic
public export
record DiagnosticRelatedInformation where
  constructor MkDiagnosticRelatedInformation
  location : Location
  message  : String
%runElab derive "DiagnosticRelatedInformation" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#diagnostic
public export
record CodeDescription where
  constructor MkCodeDescription
  href : URI
%runElab derive "CodeDescription" [FromJSON, ToJSON]

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
%runElab derive "Diagnostic" [FromJSONLSP, ToJSONLSP]

namespace PublishDiagnosticsClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_publishDiagnostics
  public export
  record TagSupport where
    constructor MkTagSupport
    valueSet : List DiagnosticTag
  %runElab derive "TagSupport" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_publishDiagnostics
public export
record PublishDiagnosticsClientCapabilities where
  constructor MkPublishDiagnosticsClientCapabilities
  relatedInformation     : Maybe Bool
  tagSupport             : Maybe TagSupport
  versionSupport         : Maybe Bool
  codeDescriptionSupport : Maybe Bool
  dataSupport            : Maybe Bool
%runElab derive "PublishDiagnosticsClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_publishDiagnostics
public export
record PublishDiagnosticsParams where
  constructor MkPublishDiagnosticsParams
  uri         : DocumentURI
  version     : Maybe Int
  diagnostics : List Diagnostic
%runElab derive "PublishDiagnosticsParams" [FromJSON, ToJSON]

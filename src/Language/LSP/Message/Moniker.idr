module Language.LSP.Message.Moniker

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_moniker
public export
record MonikerClientCapabilities where
  constructor MkMonikerClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab deriveJSON defaultOpts `{{MonikerClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_moniker
public export
record MonikerOptions where
  constructor MkMonikersOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{MonikerOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_moniker
public export
record MonikerRegistrationOptions where
  constructor MkMonikersRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : DocumentSelector .+. Null
%runElab deriveJSON defaultOpts `{{MonikerRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_moniker
public export
record MonikerParams where
  constructor MkMonikersParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  position           : Position
%runElab deriveJSON defaultOpts `{{MonikerParams}}

namespace UniquenessLevel
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_moniker
  public export
  data UniquenessLevel = Document | Project | Group | Scheme | Global

export
ToJSON UniquenessLevel where
  toJSON Document = JString "document"
  toJSON Project = JString "project"
  toJSON Group = JString "group"
  toJSON Scheme = JString "scheme"
  toJSON Global = JString "global"

export
FromJSON UniquenessLevel where
  fromJSON (JString "document") = pure Document
  fromJSON (JString "project") = pure Project
  fromJSON (JString "group") = pure Group
  fromJSON (JString "scheme") = pure Scheme
  fromJSON (JString "global") = pure Global
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_moniker
namespace MonikerKind
  public export
  data MonikerKind = Import | Export | Local

export
ToJSON MonikerKind where
  toJSON Import = JString "import"
  toJSON Export = JString "export"
  toJSON Local  = JString "local"

export
FromJSON MonikerKind where
  fromJSON (JString "import") = pure Import
  fromJSON (JString "export") = pure Export
  fromJSON (JString "local")  = pure Local
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_moniker
public export
record Moniker where
  constructor MkMoniker
  scheme     : String
  identifier : String
  unique     : UniquenessLevel
  kind       : Maybe MonikerKind
%runElab deriveJSON defaultOpts `{{Moniker}}

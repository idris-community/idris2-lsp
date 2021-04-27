module Language.LSP.Message.SignatureHelp

import Language.JSON
import Language.LSP.Message.Location
import Language.LSP.Message.Derive
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.LSP.Message.Markup
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Progress
import Language.Reflection

%language ElabReflection
%default total

namespace SignatureHelpClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
  public export
  record SignatureHelpParameterInformation where
    constructor MkSignatureHelpParameterInformation
    labelOffsetSupport : Maybe Bool
  %runElab deriveJSON defaultOpts `{{SignatureHelpParameterInformation}}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
  public export
  record SignatureHelpInformation where
    constructor MkSignatureHelpInformation
    documentationFormat    : Maybe (List MarkupKind)
    parameterInformation   : Maybe SignatureHelpParameterInformation
    activeParameterSupport : Maybe Bool
  %runElab deriveJSON defaultOpts `{{SignatureHelpInformation}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelpClientCapabilities where
  constructor MkSignatureHelpClientCapabilities
  dynamicRegistration  : Maybe Bool
  signatureInformation : Maybe SignatureHelpInformation
  contextSupport       : Maybe Bool
%runElab deriveJSON defaultOpts `{{SignatureHelpClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelpOptions where
  constructor MkSignatureHelpOptions
  workDoneProgress    : Maybe Bool
  triggerCharacters   : Maybe (List Char)
  retriggerCharacters : Maybe (List Char)
%runElab deriveJSON defaultOpts `{{SignatureHelpOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelpRegistrationOptions where
  constructor MkSignatureHelpRegistrationOptions
  workDoneProgress    : Maybe Bool
  triggerCharacters   : Maybe (List Char)
  retriggerCharacters : Maybe (List Char)
  documentSelector    : OneOf [DocumentSelector, Null]
%runElab deriveJSON defaultOpts `{{SignatureHelpRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
namespace SignatureHelpTriggerKind
  public export
  data SignatureHelpTriggerKind = Invoked | TriggerCharacter | ContentChange

export
ToJSON SignatureHelpTriggerKind where
  toJSON Invoked          = JNumber 1
  toJSON TriggerCharacter = JNumber 2
  toJSON ContentChange    = JNumber 3

export
FromJSON SignatureHelpTriggerKind where
  fromJSON (JNumber 1) = pure Invoked
  fromJSON (JNumber 2) = pure TriggerCharacter
  fromJSON (JNumber 3) = pure ContentChange
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record ParameterInformation where
  constructor MkParameterInformation
  label         : OneOf [String, (Int, Int)]
  documentation : Maybe (OneOf [String, MarkupContent])
%runElab deriveJSON defaultOpts `{{ParameterInformation}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureInformation where
  constructor MkSignatureInformation
  label           : String
  documentation   : Maybe (OneOf [String, MarkupContent])
  parameters_     : Maybe (List ParameterInformation)
  activeParameter : Maybe Int
%runElab deriveJSON (record {renames = [("parameters_", "parameters")]} defaultOpts) `{{SignatureInformation}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelp where
  constructor MkSignatureHelp
  signatures      : List SignatureInformation
  activeSignature : Maybe Int
  activeParameter : Maybe Int
%runElab deriveJSON defaultOpts `{{SignatureHelp}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelpContext where
  constructor MkSignatureHelpContext
  triggerKind         : SignatureHelpTriggerKind
  triggerCharacter    : Maybe Char
  isRetrigger         : Bool
  activeSignatureHelp : Maybe SignatureHelp
%runElab deriveJSON defaultOpts `{{SignatureHelpContext}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelpParams where
  constructor MkSignatureHelpParams
  workDoneToken : Maybe ProgressToken
  textDocument  : TextDocumentIdentifier
  position      : Position
  context       : Maybe SignatureHelpContext
%runElab deriveJSON defaultOpts `{{SignatureHelpParams}}

module Language.LSP.Message.SignatureHelp

import Language.LSP.Message.Location
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.LSP.Message.Markup
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Progress
import Language.Reflection

%language ElabReflection
%default total
%hide Text.Bounds.Position

namespace SignatureHelpClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
  public export
  record SignatureHelpParameterInformation where
    constructor MkSignatureHelpParameterInformation
    labelOffsetSupport : Maybe Bool
  %runElab derive "SignatureHelpParameterInformation" [FromJSON, ToJSON]

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
  public export
  record SignatureHelpInformation where
    constructor MkSignatureHelpInformation
    documentationFormat    : Maybe (List MarkupKind)
    parameterInformation   : Maybe SignatureHelpParameterInformation
    activeParameterSupport : Maybe Bool
  %runElab derive "SignatureHelpInformation" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelpClientCapabilities where
  constructor MkSignatureHelpClientCapabilities
  dynamicRegistration  : Maybe Bool
  signatureInformation : Maybe SignatureHelpInformation
  contextSupport       : Maybe Bool
%runElab derive "SignatureHelpClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelpOptions where
  constructor MkSignatureHelpOptions
  workDoneProgress    : Maybe Bool
  triggerCharacters   : Maybe (List Char)
  retriggerCharacters : Maybe (List Char)
%runElab derive "SignatureHelpOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelpRegistrationOptions where
  constructor MkSignatureHelpRegistrationOptions
  workDoneProgress    : Maybe Bool
  triggerCharacters   : Maybe (List Char)
  retriggerCharacters : Maybe (List Char)
  documentSelector    : OneOf [DocumentSelector, Null]
%runElab derive "SignatureHelpRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
namespace SignatureHelpTriggerKind
  public export
  data SignatureHelpTriggerKind = Invoked | TriggerCharacter | ContentChange

export
ToJSON SignatureHelpTriggerKind where
  toJSON Invoked          = JInteger 1
  toJSON TriggerCharacter = JInteger 2
  toJSON ContentChange    = JInteger 3

export
FromJSON SignatureHelpTriggerKind where
  fromJSON (JInteger 1) = pure Invoked
  fromJSON (JInteger 2) = pure TriggerCharacter
  fromJSON (JInteger 3) = pure ContentChange
  fromJSON _ = fail "invalid signature help trigger, 1|2|3"

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record ParameterInformation where
  constructor MkParameterInformation
  label         : OneOf [String, (Int, Int)]
  documentation : Maybe (OneOf [String, MarkupContent])
%runElab derive "ParameterInformation" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureInformation where
  constructor MkSignatureInformation
  label           : String
  documentation   : Maybe (OneOf [String, MarkupContent])
  parameters_     : Maybe (List ParameterInformation)
  activeParameter : Maybe Int
%runElab derive "SignatureInformation" [FromJSONLSP, ToJSONLSP]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelp where
  constructor MkSignatureHelp
  signatures      : List SignatureInformation
  activeSignature : Maybe Int
  activeParameter : Maybe Int
%runElab derive "SignatureHelp" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelpContext where
  constructor MkSignatureHelpContext
  triggerKind         : SignatureHelpTriggerKind
  triggerCharacter    : Maybe Char
  isRetrigger         : Bool
  activeSignatureHelp : Maybe SignatureHelp
%runElab derive "SignatureHelpContext" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_signatureHelp
public export
record SignatureHelpParams where
  constructor MkSignatureHelpParams
  workDoneToken : Maybe ProgressToken
  textDocument  : TextDocumentIdentifier
  position      : Position
  context       : Maybe SignatureHelpContext
%runElab derive "SignatureHelpParams" [FromJSON, ToJSON]

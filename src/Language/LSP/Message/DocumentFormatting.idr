module Language.LSP.Message.DocumentFormatting

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_formatting
public export
record DocumentFormattingClientCapabilities where
  constructor MkDocumentFormattingClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab derive "DocumentFormattingClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_formatting
public export
record DocumentFormattingOptions where
  constructor MkDocumentFormattingOptions
  workDoneProgress : Maybe Bool
%runElab derive "DocumentFormattingOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_formatting
public export
record DocumentFormattingRegistrationOptions where
  constructor MkDocumentFormattingRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab derive "DocumentFormattingRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_formatting
public export
record FormattingOptions where
  constructor MkFormattingOptions
  tabSize                : Int
  insertSpaces           : Bool
  trimTrailingWhitespace : Maybe Bool
  insertFinalNewline     : Maybe Bool
  trimFinalNewlines      : Maybe Bool
  other                  : List (String, OneOf [Bool, Int, String])

%runElab derive "FormattingOptions"
  [customFromJSON Export nullMissingFields,
   customToJSON Export nullMissingFields]


||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_formatting
public export
record DocumentFormattingParams where
  constructor MkDocumentFormattingParams
  workDoneToken : Maybe ProgressToken
  textDocument  : TextDocumentIdentifier
  options       : FormattingOptions
%runElab derive "DocumentFormattingParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rangeFormatting
public export
record DocumentRangeFormattingClientCapabilities where
  constructor MkDocumentRangeFormattingClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab derive "DocumentRangeFormattingClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rangeFormatting
public export
record DocumentRangeFormattingOptions where
  constructor MkDocumentRangeFormattingOptions
  workDoneProgress : Maybe Bool
%runElab derive "DocumentRangeFormattingOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rangeFormatting
public export
record DocumentRangeFormattingRegistrationOptions where
  constructor MkDocumentRangeFormattingRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab derive "DocumentRangeFormattingRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rangeFormatting
public export
record DocumentRangeFormattingParams where
  constructor MkDocumentRangeFormattingParams
  workDoneToken : Maybe ProgressToken
  textDocument  : TextDocumentIdentifier
  range         : Range
  options       : FormattingOptions
%runElab derive "DocumentRangeFormattingParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_onTypeFormatting
public export
record DocumentOnTypeFormattingClientCapabilities where
  constructor MkDocumentOnTypeFormattingClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab derive "DocumentOnTypeFormattingClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_onTypeFormatting
public export
record DocumentOnTypeFormattingOptions where
  constructor MkDocumentOnTypeFormattingOptions
  firstTriggerCharacter : Char
  moreTriggerCharacter  : Maybe (List Char)
%runElab derive "DocumentOnTypeFormattingOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_onTypeFormatting
public export
record DocumentOnTypeFormattingRegistrationOptions where
  constructor MkDocumentOnTypeFormattingRegistrationOptions
  firstTriggerCharacter : Char
  moreTriggerCharacter  : Maybe (List Char)
  documentSelector      : OneOf [DocumentSelector, Null]
%runElab derive "DocumentOnTypeFormattingRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_onTypeFormatting
public export
record DocumentOnTypeFormattingParams where
  constructor MkDocumentOnTypeFormattingParams
  textDocument : TextDocumentIdentifier
  ch           : Char
  options      : FormattingOptions
%runElab derive "DocumentOnTypeFormattingParams" [FromJSON, ToJSON]

module Language.LSP.Message.DocumentFormatting

import Language.JSON
import Language.LSP.Message.Derive
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
%runElab deriveJSON defaultOpts `{{DocumentFormattingClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_formatting
public export
record DocumentFormattingOptions where
  constructor MkDocumentFormattingOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentFormattingOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_formatting
public export
record DocumentFormattingRegistrationOptions where
  constructor MkDocumentFormattingRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab deriveJSON defaultOpts `{{DocumentFormattingRegistrationOptions}}

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

export
ToJSON FormattingOptions where
  toJSON opts =
    JObject ((catMaybes [ Just ("tabSize", toJSON opts.tabSize)
                        , Just ("insertSpaces", toJSON opts.insertSpaces)
                        , ("trimTrailingWhitespace",) . toJSON <$> opts.trimTrailingWhitespace
                        , ("insertFinalNewline",) . toJSON <$> opts.insertFinalNewline
                        , ("trimFinalNewlines",) . toJSON <$> opts.trimFinalNewlines
                        ]) ++ (mapSnd toJSON <$> opts.other))

export
FromJSON FormattingOptions where
  fromJSON (JObject arg) =
      pure MkFormattingOptions <*> (lookup "tabSize" arg >>= fromJSON)
                               <*> (lookup "insertSpaces" arg >>= fromJSON)
                               <*> (pure $ lookup "trimTrailingWhitespace" arg >>= fromJSON)
                               <*> (pure $ lookup "insertFinalNewline" arg >>= fromJSON)
                               <*> (pure $ lookup "trimFinalNewlines" arg >>= fromJSON)
                               <*> (pure $ catMaybes $ map sequence $ map (mapSnd fromJSON) $ filter (\(k, _) => not $ k `elem` fields) $ toList arg)
    where
      fields : List String
      fields = ["tabSize", "insertSpaces", "trimTrailingWhitespace", "insertFinalNewline", "trimFinalNewlines"]
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_formatting
public export
record DocumentFormattingParams where
  constructor MkDocumentFormattingParams
  workDoneToken : Maybe ProgressToken
  textDocument  : TextDocumentIdentifier
  options       : FormattingOptions
%runElab deriveJSON defaultOpts `{{DocumentFormattingParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rangeFormatting
public export
record DocumentRangeFormattingClientCapabilities where
  constructor MkDocumentRangeFormattingClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentRangeFormattingClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rangeFormatting
public export
record DocumentRangeFormattingOptions where
  constructor MkDocumentRangeFormattingOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentRangeFormattingOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rangeFormatting
public export
record DocumentRangeFormattingRegistrationOptions where
  constructor MkDocumentRangeFormattingRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab deriveJSON defaultOpts `{{DocumentRangeFormattingRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rangeFormatting
public export
record DocumentRangeFormattingParams where
  constructor MkDocumentRangeFormattingParams
  workDoneToken : Maybe ProgressToken
  textDocument  : TextDocumentIdentifier
  range         : Range
  options       : FormattingOptions
%runElab deriveJSON defaultOpts `{{DocumentRangeFormattingParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_onTypeFormatting
public export
record DocumentOnTypeFormattingClientCapabilities where
  constructor MkDocumentOnTypeFormattingClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentOnTypeFormattingClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_onTypeFormatting
public export
record DocumentOnTypeFormattingOptions where
  constructor MkDocumentOnTypeFormattingOptions
  firstTriggerCharacter : Char
  moreTriggerCharacter  : Maybe (List Char)
%runElab deriveJSON defaultOpts `{{DocumentOnTypeFormattingOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_onTypeFormatting
public export
record DocumentOnTypeFormattingRegistrationOptions where
  constructor MkDocumentOnTypeFormattingRegistrationOptions
  firstTriggerCharacter : Char
  moreTriggerCharacter  : Maybe (List Char)
  documentSelector      : OneOf [DocumentSelector, Null]
%runElab deriveJSON defaultOpts `{{DocumentOnTypeFormattingRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_onTypeFormatting
public export
record DocumentOnTypeFormattingParams where
  constructor MkDocumentOnTypeFormattingParams
  textDocument : TextDocumentIdentifier
  ch           : Char
  options      : FormattingOptions
%runElab deriveJSON defaultOpts `{{DocumentOnTypeFormattingParams}}

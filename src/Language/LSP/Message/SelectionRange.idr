module Language.LSP.Message.SelectionRange

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_selectionRange
public export
record SelectionRangeClientCapabilities where
  constructor MkSelectionRangeClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab derive "SelectionRangeClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_selectionRange
public export
record SelectionRangeOptions where
  constructor MkSelectionRangeOptions
  workDoneProgress : Maybe Bool
%runElab derive "SelectionRangeOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_selectionRange
public export
record SelectionRangeRegistrationOptions where
  constructor MkSelectionRangeRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe String
%runElab derive "SelectionRangeRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_selectionRange
public export
record SelectionRangeParams where
  constructor MkSelectionRangeParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  positions          : List Position
%runElab derive "SelectionRangeParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_selectionRange
public export
record SelectionRange where
  constructor MkSelectionRange
  range  : Range
  parent : Maybe (Inf SelectionRange)

export -- FIXME: Can I avoid asser_total? Maybe indexing by the depth of the structure?
ToJSON SelectionRange where
  toJSON (MkSelectionRange range parent) = assert_total $
    JObject (catMaybes [ Just ("range", toJSON range)
                       , (MkPair "parent" . toJSON) <$> parent
                       ])

export covering
FromJSON SelectionRange where
  fromJSON (JObject arg) =
    pure MkSelectionRange <*> (lookup "range" arg >>= fromJSON)
                          <*> (pure $ lookup "parent" arg >>= fromJSON)
  fromJSON _ = neutral

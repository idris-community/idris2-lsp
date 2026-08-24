module Language.LSP.Message.FoldingRange

import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_foldingRange
public export
record FoldingRangeClientCapabilities where
  constructor MkFoldingRangeClientCapabilities
  dynamicRegistration : Maybe Bool
  rangeLimit          : Maybe Int
  lineFoldingOnly     : Maybe Bool
%runElab derive "FoldingRangeClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_foldingRange
public export
record FoldingRangeOptions where
  constructor MkFoldingRangeOptions
  workDoneProgress : Maybe Bool
%runElab derive "FoldingRangeOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_foldingRange
public export
record FoldingRangeRegistrationOptions where
  constructor MkFoldingRangeRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe String
%runElab derive "FoldingRangeRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_foldingRange
public export
record FoldingRangeParams where
  constructor MkFoldingRangeParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
%runElab derive "FoldingRangeParams" [FromJSON, ToJSON]

namespace FoldingRangeKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_foldingRange
  public export
  data FoldingRangeKind = Comment | Imports | Region

export
ToJSON FoldingRangeKind where
  toJSON Comment = JString "comment"
  toJSON Imports = JString "imports"
  toJSON Region  = JString "region"

export
FromJSON FoldingRangeKind where
  fromJSON (JString "comment") = pure Comment
  fromJSON (JString "imports") = pure Imports
  fromJSON (JString "region")  = pure Region
  fromJSON _ = fail "not a folding range, comment|imports|region"

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_foldingRange
public export
record FoldingRange where
  constructor MkFoldingRange
  startLine      : Int
  startCharacter : Maybe Int
  endLine        : Int
  endCharacter   : Maybe Int
  kind           : Maybe String
%runElab derive "FoldingRange" [FromJSON, ToJSON]

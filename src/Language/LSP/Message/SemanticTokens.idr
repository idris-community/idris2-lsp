module Language.LSP.Message.SemanticTokens

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

namespace SemanticTokenClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
  public export
  record SemanticTokenRequestsFull where
    constructor MkSemanticTokenRequestsFull
    delta : Maybe Bool
  %runElab derive "SemanticTokenRequestsFull" [FromJSON, ToJSON]

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
  public export
  record SemanticTokenRequests where
    constructor MkSemanticTokenRequests
    range : Maybe (OneOf [Bool, ()])
    full  : Maybe (OneOf [Bool, SemanticTokenRequestsFull])
  %runElab derive "SemanticTokenRequests" [FromJSON, ToJSON]

namespace TokenFormat
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
  public export
  data TokenFormat = Relative

export
ToJSON TokenFormat where
  toJSON Relative = JString "relative"

export
FromJSON TokenFormat where
  fromJSON (JString "relative") = pure Relative
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensLegend where
  constructor MkSemanticTokensLegend
  tokenTypes     : List String
  tokenModifiers : List String
%runElab derive "SemanticTokensLegend" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensClientCapabilities where
  constructor MkSemanticTokensClientCapabilities
  dynamicRegistration     : Maybe Bool
  requests                : SemanticTokenRequests
  tokenTypes              : List String
  tokenModifiers          : List String
  formats                 : List TokenFormat
  overlappingTokenSupport : Maybe Bool
  multilineTokenSupport   : Maybe Bool
%runElab derive "SemanticTokensClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensOptions where
  constructor MkSemanticTokensOptions
  legend : SemanticTokensLegend
  range  : Maybe (OneOf [Bool, ()])
  full   : Maybe (OneOf [Bool, SemanticTokenRequestsFull])
%runElab derive "SemanticTokensOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensRegistrationOptions where
  constructor MkSemanticTokensRegistrationOptions
  legend           : SemanticTokensLegend
  range            : Maybe (OneOf [Bool, ()])
  full             : Maybe (OneOf [Bool, SemanticTokenRequestsFull])
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe Bool
%runElab derive "SemanticTokensRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensParams where
  constructor MkSemanticTokensParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
%runElab derive "SemanticTokensParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokens where
  constructor MkSemanticTokens
  resultId : Maybe String
  data_    : List Int
%runElab deriveJSON ({renames := [("data_", "data")]} defaultOpts) `{SemanticTokens}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensPartialResult where
  constructor MkSemanticTokensPartialResult
  data_ : List Int
%runElab deriveJSON ({renames := [("data_", "data")]} defaultOpts) `{SemanticTokensPartialResult}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensDeltaParams where
  constructor MkSemanticTokensDeltaParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  previousResultId   : String
%runElab derive "SemanticTokensDeltaParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensEdit where
  constructor MkSemanticTokensEdit
  start       : Int
  deleteCount : Int
  data_       : Maybe (List Int)
%runElab deriveJSON ({renames := [("data_", "data")]} defaultOpts) `{SemanticTokensEdit}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensDelta where
  constructor MkSemanticTokensDelta
  resultId : Maybe String
  edits    : List SemanticTokensEdit
%runElab derive "SemanticTokensDelta" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensDeltaPartialResult where
  constructor MkSemanticTokensDeltaPartialResult
  edits : List SemanticTokensEdit
%runElab derive "SemanticTokensDeltaPartialResult" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensRangeParams where
  constructor MkSemanticTokensRangeParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  range              : Range
%runElab derive "SemanticTokensRangeParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensWorkspaceClientCapabilities where
  constructor MkSemanticTokensWorkspaceClientCapabilities
  refreshSupport : Maybe Bool
%runElab derive "SemanticTokensWorkspaceClientCapabilities" [FromJSON, ToJSON]

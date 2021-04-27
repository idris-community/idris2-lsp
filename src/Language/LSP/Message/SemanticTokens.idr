module Language.LSP.Message.SemanticTokens

import Language.JSON
import Language.LSP.Message.Derive
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
  %runElab deriveJSON defaultOpts `{{SemanticTokenRequestsFull}}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
  public export
  record SemanticTokenRequests where
    constructor MkSemanticTokenRequests
    range : Maybe (OneOf [Bool, ()])
    full  : Maybe (OneOf [Bool, SemanticTokenRequestsFull])
  %runElab deriveJSON defaultOpts `{{SemanticTokenRequests}}

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
  constructor MkSemanticTokensLenged
  tokenTypes     : List String
  tokenModifiers : List String
%runElab deriveJSON defaultOpts `{{SemanticTokensLegend}}

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
%runElab deriveJSON defaultOpts `{{SemanticTokensClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensOptions where
  constructor MkSemanticTokensOptions
  legend : SemanticTokensLegend
  range  : Maybe (OneOf [Bool, ()])
  full   : Maybe (OneOf [Bool, SemanticTokenRequestsFull])
%runElab deriveJSON defaultOpts `{{SemanticTokensOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensRegistrationOptions where
  constructor MkSemanticTokensRegistrationOptions
  legend           : SemanticTokensLegend
  range            : Maybe (OneOf [Bool, ()])
  full             : Maybe (OneOf [Bool, SemanticTokenRequestsFull])
  documentSelector : OneOf [DocumentSelector, Null]
  id               : Maybe Bool
%runElab deriveJSON defaultOpts `{{SemanticTokensRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensParams where
  constructor MkSemanticTokensParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
%runElab deriveJSON defaultOpts `{{SemanticTokensParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokens where
  constructor MkSemanticTokens
  resultId : Maybe String
  data_    : List Int
%runElab deriveJSON (record {renames = [("data_", "data")]} defaultOpts) `{{SemanticTokens}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensPartialResult where
  constructor MkSemanticTokensPartialResult
  data_ : List Int
%runElab deriveJSON (record {renames = [("data_", "data")]} defaultOpts) `{{SemanticTokensPartialResult}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensDeltaParams where
  constructor MkSemanticTokensDeltaParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  previousResultId   : String
%runElab deriveJSON defaultOpts `{{SemanticTokensDeltaParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensEdit where
  constructor MkSemanticTokensEdit
  start       : Int
  deleteCount : Int
  data_       : Maybe (List Int)
%runElab deriveJSON (record {renames = [("data_", "data")]} defaultOpts) `{{SemanticTokensEdit}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensDelta where
  constructor MkSemanticTokensDelta
  resultId : Maybe String
  edits    : List SemanticTokensEdit
%runElab deriveJSON defaultOpts `{{SemanticTokensDelta}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensDeltaPartialResult where
  constructor MkSemanticTokensDeltaPartialResult
  edits : List SemanticTokensEdit
%runElab deriveJSON defaultOpts `{{SemanticTokensDeltaPartialResult}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensRangeParams where
  constructor MkSemanticTokensRangeParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  range              : Range
%runElab deriveJSON defaultOpts `{{SemanticTokensRangeParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_semanticTokens
public export
record SemanticTokensWorkspaceClientCapabilities where
  constructor MkSemanticTokensWorkspaceClientCapabilities
  refreshSupport : Maybe Bool
%runElab deriveJSON defaultOpts `{{SemanticTokensWorkspaceClientCapabilities}}

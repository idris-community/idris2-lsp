module Language.LSP.Message.Cancel

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#cancelRequest
public export
record CancelParams where
  constructor MkCancelParams
  id : Int .+. String
%runElab deriveJSON defaultOpts `{{CancelParams}}

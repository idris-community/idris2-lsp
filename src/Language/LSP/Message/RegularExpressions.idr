module Language.LSP.Message.RegularExpressions

import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#regExp
public export
record RegularExpressionsClientCapabilities where
  constructor MkRegularExpressionsClientCapabilities
  engine  : String
  version : Maybe String
%runElab derive "RegularExpressionsClientCapabilities" [FromJSON, ToJSON]

module Language.LSP.Message.Trace

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

namespace Trace
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#traceValue
  public export
  data Trace = TraceOff | TraceMessages | TraceVerbose

export
ToJSON Trace where
  toJSON TraceOff      = JString "off"
  toJSON TraceMessages = JString "messages"
  toJSON TraceVerbose  = JString "verbose"

export
FromJSON Trace where
  fromJSON (JString "off")      = pure TraceOff
  fromJSON (JString "messages") = pure TraceMessages
  fromJSON (JString "verbose")  = pure TraceVerbose
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#setTrace
public export
record SetTraceParams where
  constructor MkSetTraceParams
  value : Trace
%runElab deriveJSON defaultOpts `{SetTraceParams}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#logTrace
public export
record LogTraceParams where
  constructor MkLogTraceParams
  message : String
  verbose : Maybe String
%runElab deriveJSON defaultOpts `{LogTraceParams}

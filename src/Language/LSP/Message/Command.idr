module Language.LSP.Message.Command

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Progress
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#command
public export
record Command where
  constructor MkCommand
  title     : String
  command   : String
  arguments : Maybe (List JSON)
%runElab deriveJSON defaultOpts `{Command}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_executeCommand
public export
record ExecuteCommandClientCapabilities where
  constructor MkExecuteCommandClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab deriveJSON defaultOpts `{ExecuteCommandClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_executeCommand
public export
record ExecuteCommandOptions where
  constructor MkExecuteCommandOptions
  workDoneProgress : Maybe Bool
  commands         : List String
%runElab deriveJSON defaultOpts `{ExecuteCommandOptions}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_executeCommand
public export
record ExecuteCommandRegistrationOptions where
  constructor MkExecuteCommandRegistrationOptions
  workDoneProgress : Maybe Bool
  commands         : List String
%runElab deriveJSON defaultOpts `{ExecuteCommandRegistrationOptions}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_executeCommand
public export
record ExecuteCommandParams where
  constructor MkExecuteCommandParams
  partialResultToken : Maybe ProgressToken
  command            : String
  arguments          : Maybe (List JSON)
%runElab deriveJSON defaultOpts `{ExecuteCommandParams}

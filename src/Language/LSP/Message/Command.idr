module Language.LSP.Message.Command

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
%runElab derive "Command" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_executeCommand
public export
record ExecuteCommandClientCapabilities where
  constructor MkExecuteCommandClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab derive "ExecuteCommandClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_executeCommand
public export
record ExecuteCommandOptions where
  constructor MkExecuteCommandOptions
  workDoneProgress : Maybe Bool
  commands         : List String
%runElab derive "ExecuteCommandOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_executeCommand
public export
record ExecuteCommandRegistrationOptions where
  constructor MkExecuteCommandRegistrationOptions
  workDoneProgress : Maybe Bool
  commands         : List String
%runElab derive "ExecuteCommandRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_executeCommand
public export
record ExecuteCommandParams where
  constructor MkExecuteCommandParams
  partialResultToken : Maybe ProgressToken
  command            : String
  arguments          : Maybe (List JSON)
%runElab derive "ExecuteCommandParams" [FromJSON, ToJSON]

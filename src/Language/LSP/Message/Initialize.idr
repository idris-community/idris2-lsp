module Language.LSP.Message.Initialize

import Language.LSP.Message.ClientCapabilities
import Language.LSP.Message.Progress
import Language.LSP.Message.ServerCapabilities
import Language.LSP.Message.Trace
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.LSP.Message.Workspace
import Language.Reflection

%language ElabReflection
%default total

namespace InitializeParams
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
  public export
  record ClientInfo where
    name    : String
    version : Maybe String
  %runElab derive "ClientInfo" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record InitializeParams where
  constructor MkInitializeParams
  processId             : OneOf [Integer, Null]
  clientInfo            : Maybe ClientInfo
  locale                : Maybe String
  rootPath              : Maybe (OneOf [String, Null])
  rootUri               : OneOf [URI, Null]
  initializationOptions : Maybe JSON
  capabilities          : ClientCapabilities
  trace                 : Maybe Trace
  workspaceFolders      : Maybe (OneOf [List WorkspaceFolder, Null])
  workDoneToken         : Maybe ProgressToken
%runElab derive "InitializeParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record InitializeResult where
  constructor MkInitializeResult
  capabilities : ServerCapabilities
  serverInfo   : Maybe ServerInfo
%runElab derive "InitializeResult" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record InitializeError where
  constructor MkInitializeError
  retry : Bool
%runElab derive "InitializeError" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialized
public export
record InitializedParams where
  constructor MkInitializedParams
%runElab derive "InitializedParams" [FromJSON, ToJSON]

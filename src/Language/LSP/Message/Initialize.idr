module Language.LSP.Message.Initialize

import Language.JSON
import Language.LSP.Message.ClientCapabilities
import Language.LSP.Message.Derive
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
  %runElab deriveJSON defaultOpts `{{ClientInfo}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record InitializeParams where
  constructor MkInitializeParams
  processId          : Maybe Integer
  clientInfo         : ClientInfo
  locale             : Maybe String
  rootPath           : Maybe String
  rootUri            : Maybe URI
  initializeOptions  : Maybe JSON
  clientCapabilities : ClientCapabilities
  trace              : Maybe Trace
  workspaceFolders   : Maybe (List WorkspaceFolder)
  workDoneToken      : Maybe ProgressToken
%runElab deriveJSON defaultOpts `{{InitializeParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record InitializeResult where
  constructor MkInitializeResult
  capabilities : ServerCapabilities
  serverInfo   : Maybe ServerInfo
%runElab deriveJSON defaultOpts `{{InitializeResult}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record InitializeError where
  constructor MkInitializeError
  retry : Bool
%runElab deriveJSON defaultOpts `{{InitializeError}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialized
public export
record InitializedParams where
  constructor MkInitializedParams
%runElab deriveJSON defaultOpts `{{InitializedParams}}

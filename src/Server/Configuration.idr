||| Definition and helper functions related to the LSP server implementation configuration.
|||
||| (C) The Idris Community, 2021
module Server.Configuration

import Language.LSP.Message.Initialize
import Language.LSP.Message.URI
import System.File

||| Label for the configuration reference.
public export
data LSPConf : Type where

||| Type for the LSP server configuration.
public export
record LSPConfiguration where
  constructor MkLSPConfiguration
  ||| File handle where to read LSP messages.
  inputHandle : File
  ||| File handle where to output LSP messages.
  outputHandle : File
  ||| If the initialization protocol has succeded, it contains the client configuration.
  ||| @see https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#initialize
  initialized : Maybe InitializeParams
  ||| True if the client has completed the shutdown protocol.
  ||| @see https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#shutdown
  isShutdown : Bool
  ||| The currently loaded file, if any, and its version.
  openFile : Maybe (DocumentURI, Int)

||| Server default configuration. Uses standard input and standard output for input/output.
export
defaultConfig : LSPConfiguration
defaultConfig = MkLSPConfiguration { inputHandle = stdin
                                   , outputHandle = stdout
                                   , initialized = Nothing
                                   , isShutdown = False
                                   , openFile = Nothing
                                   }

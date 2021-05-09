||| Definition and helper functions related to the LSP server implementation
||| configuration.
|||
||| (C) The Idris Community, 2021
module Server.Configuration

import Core.FC
import Language.LSP.CodeAction
import Language.LSP.Message.CodeAction
import Language.LSP.Message.Initialize
import Language.LSP.Message.Hover
import Language.LSP.Message.Location
import Language.LSP.Message.URI
import Libraries.Data.PosMap
import System.File
import System.Clock

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
  ||| File handle where to put log messages.
  logHandle : File
  ||| If the initialization protocol has succeded, it contains the client configuration.
  ||| @see https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#initialize
  initialized : Maybe InitializeParams
  ||| True if the client has completed the shutdown protocol.
  ||| @see https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#shutdown
  isShutdown : Bool
  ||| The currently loaded file, if any, and its version.
  openFile : Maybe (DocumentURI, Int)
  ||| Limit for multiple search results
  searchLimit : Nat
  ||| List of quickfixes to be send in addition to other code actions
  quickfixes : List CodeAction
  ||| Cached code actions by position
  cachedActions : PosMap (Range, IdrisAction, List CodeAction)
  ||| Cached hovers
  cachedHovers : PosMap (Range, Hover)
  ||| Timeout in ms for long operations (currently stops multiple commands, e.g. ExprSearch)
  ||| TODO: extend it to any operation and report the timeout, making it overridable
  longActionTimeout : Clock Duration

||| Server default configuration. Uses standard input and standard output for input/output.
export
defaultConfig : LSPConfiguration
defaultConfig =
  MkLSPConfiguration
    { inputHandle       = stdin
    , outputHandle      = stdout
    , logHandle         = stderr
    , initialized       = Nothing
    , isShutdown        = False
    , openFile          = Nothing
    , searchLimit       = 5
    , quickfixes        = []
    , cachedActions     = empty
    , cachedHovers      = empty
    , longActionTimeout = makeDuration 5 0
    }

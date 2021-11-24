||| Definition and helper functions related to the LSP server implementation
||| configuration.
|||
||| (C) The Idris Community, 2021
module Server.Configuration

import Core.FC
import Data.SortedSet
import Language.LSP.CodeAction
import Language.LSP.Message.CodeAction
import Language.LSP.Message.Initialize
import Language.LSP.Message.Hover
import Language.LSP.Message.Location
import Language.LSP.Message.URI
import public Libraries.Data.PosMap
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
  ||| Files with modification not saved. Command will fail on these files.
  dirtyFiles : SortedSet DocumentURI
  ||| Files with errors
  errorFiles : SortedSet DocumentURI
  ||| Semantic tokens have been sent
  semanticTokensSentFiles : SortedSet DocumentURI
  ||| Limit for multiple search results
  searchLimit : Nat
  ||| List of quickfixes to be send in addition to other code actions
  quickfixes : List CodeAction
  ||| Cached code actions by position
  cachedActions : PosMap (Range, IdrisAction, List CodeAction)
  ||| Cached hovers
  cachedHovers : PosMap (Range, Hover)
  ||| next id for requests to the server
  nextRequestId : Nat

||| Server default configuration. Uses standard input and standard output for input/output.
export
defaultConfig : LSPConfiguration
defaultConfig =
  MkLSPConfiguration
    { inputHandle             = stdin
    , outputHandle            = stdout
    , logHandle               = stderr
    , initialized             = Nothing
    , isShutdown              = False
    , openFile                = Nothing
    , dirtyFiles              = empty
    , errorFiles              = empty
    , semanticTokensSentFiles = empty
    , searchLimit             = 5
    , quickfixes              = []
    , cachedActions           = empty
    , cachedHovers            = empty
    , nextRequestId           = 0
    }

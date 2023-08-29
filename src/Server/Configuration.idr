||| Definition and helper functions related to the LSP server implementation
||| configuration.
|||
||| (C) The Idris Community, 2021
module Server.Configuration

import public Libraries.Data.PosMap
import public Libraries.Data.NameMap

import Core.FC
import Core.Name
import Data.SortedMap
import Data.SortedSet
import Language.LSP.CodeAction
import Language.LSP.Completion.Info
import Language.LSP.Message.CodeAction
import Language.LSP.Message.Hover
import Language.LSP.Message.Initialize
import Language.LSP.Message.Location
import Language.LSP.Message.SemanticTokens
import Language.LSP.Message.URI
import Language.LSP.Severity
import System.File
import Data.String
import Data.List1


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
  ||| Level of logging; everything lighter than this will be ignored.
  logSeverity : Severity
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
  semanticTokensSentFiles : SortedMap DocumentURI SemanticTokens
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
  ||| Caches for completions, created, updated when URIs are opened, extracted from Context.
  completionCache : SortedMap DocumentURI (SortedMap Completion.Info.NameCategory (List Entry))
  ||| Virtual file content caches
  virtualDocuments : SortedMap DocumentURI (Int, String) -- Version content
  ||| Insert only function name for completions
  briefCompletions : Bool

||| Server default configuration. Uses standard input and standard output for input/output.
export
defaultConfig : LSPConfiguration
defaultConfig =
  MkLSPConfiguration
    { inputHandle             = stdin
    , outputHandle            = stdout
    , logHandle               = stderr
    , logSeverity             = Debug
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
    , completionCache         = empty
    , virtualDocuments        = empty
    , briefCompletions       = False
    }

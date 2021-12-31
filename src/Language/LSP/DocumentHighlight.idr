module Language.LSP.DocumentHighlight

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.Name
import Data.List
import Idris.Doc.String
import Language.LSP.Message
import Libraries.Data.NameMap
import Server.Configuration
import Server.Log
import Server.Utils

makeHighlight : NonEmptyFC -> DocumentHighlight
makeHighlight fc = MkDocumentHighlight
  { range = cast fc
  , kind = Nothing
  }

export
documentHighlights : Ref Ctxt Defs
                  => Ref MD Metadata
                  => Ref LSPConf LSPConfiguration
                  => DocumentHighlightParams -> Core (List DocumentHighlight)
documentHighlights params = do
  logI DocumentHighlight "Searching for \{show params.textDocument.uri}"
  Just (uri, _) <- gets LSPConf openFile
    | Nothing => logE DocumentHighlight "No open file" >> pure []
  let True = uri == params.textDocument.uri
    | False => do
        logD DocumentHighlight "Expected request for the currently opened file \{show uri}, instead received \{show params.textDocument.uri}"
        pure []

  let line = params.position.line
  let col  = params.position.character
  nameLocs <- gets MD nameLocMap
  let Just (_, name) = findPointInTreeLoc (line, col) nameLocs
    | Nothing => logD DocumentHighlight "No name found at \{show line}:\{show col}}" >> pure []
  logI DocumentHighlight "Found name \{show name}"

  matchingNames <- gets MD (nub . map fst . filter ((==) name . snd) . toList . nameLocMap)
  logI DocumentHighlight "Found \{show $ length matchingNames} matches"
  pure (makeHighlight <$> matchingNames)

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
  let uri = show params.textDocument.uri
  logI DocumentHighlight "Searching for \{uri}"

  let line = params.position.line
  let col  = params.position.character
  nameLocs <- gets MD nameLocMap
  let Just ((origin, _, _), name) = findPointInTreeLoc (line, col) nameLocs
    | Nothing => logD DocumentHighlight "No name found at \{show line}:\{show col}}" >> pure []
  logI DocumentHighlight "Found name \{show name}"

  matchingNames <- gets MD (nub . map fst . filter (\((o, _, _), n) => o == origin && n == name) . toList . nameLocMap)
  logI DocumentHighlight "Found \{show $ length matchingNames} matches"
  pure (makeHighlight <$> matchingNames)

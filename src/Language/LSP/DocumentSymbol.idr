module Language.LSP.DocumentSymbol

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Data.List
import Idris.Doc.String
import Language.LSP.Message
import Server.Configuration
import Server.Utils
import Server.Log

-- NOTE: Structured information should be rendereded from the actual Term of the expressions
-- in a file, that would give a better high-level overview. For now this is more like a placeholder
-- rather than a useful functionality.

||| Renders all the local names and type declarations from the Metadata of the active file.
export
documentSymbol : Ref MD Metadata
              => Ref LSPConf LSPConfiguration
              => DocumentSymbolParams -> Core (List DocumentSymbol)
documentSymbol params = do
  Just (uri, _) <- gets LSPConf openFile
    | Nothing => do
        logString Debug "documentSymbol: openFile returned Nothing. Weird."
        pure []
  let True = uri == params.textDocument.uri
      | False => do
          logString Debug "documentSymbol: different URI than expected \{show (uri, params.textDocument.uri)}"
          pure []
  meta <- get MD
  let localDocSymbols
        = map (\((_, (sline,scol), (eline, ecol)), (n,_,_)) =>
                let range = MkRange (MkPosition sline scol) (MkPosition eline ecol)
                    name = show $ dropNS n
                 in ( name
                    , MkDocumentSymbol
                      { name           = name
                      , detail         = Nothing
                      , kind           = Variable
                      , tags           = Nothing
                      , deprecated     = Nothing
                      , range          = range
                      , selectionRange = range
                      , children       = Nothing
                      }
                    ))
              meta.names
  let localTypeDecSymbols
        = map (\((_,(sline,scol),(eline,ecol)), (n,_,_)) =>
                let range = MkRange (MkPosition sline scol) (MkPosition eline ecol)
                    name = show $ dropNS n
                 in ( name
                    , MkDocumentSymbol
                      { name           = name
                      , detail         = Nothing
                      , kind           = Function
                      , tags           = Nothing
                      , deprecated     = Nothing
                      , range          = range
                      , selectionRange = range
                      , children       = Nothing
                      }
                    ))
              meta.tydecls
  pure
    $ map snd
    $ sortBy (\(n1, _), (n2, _) => compare n1 n2)
    $ localDocSymbols ++ localTypeDecSymbols

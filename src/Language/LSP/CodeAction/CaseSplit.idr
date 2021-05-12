module Language.LSP.CodeAction.CaseSplit

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.List1
import Data.String
import Idris.IDEMode.CaseSplit
import Idris.REPL.Opts
import Idris.Syntax
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.Message
import Libraries.Data.List.Extra
import Libraries.Data.PosMap
import Server.Configuration
import Server.Log
import Server.Response
import Server.Utils
import System.File
import TTImp.Interactive.CaseSplit
import TTImp.TTImp

export
caseSplit : Ref LSPConf LSPConfiguration
         => Ref MD Metadata
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref ROpts REPLOpts
         => CodeActionParams -> Core (Maybe CodeAction)
caseSplit params = do
  let True = params.range.start.line == params.range.end.line
    | _ => do logString Debug "caseSplit: start and end line were different"
              pure Nothing

  [] <- searchCache params.range CaseSplit
    | action :: _ => do logString Debug "caseSplit: found cached action"
                        pure (Just action)

  let line = params.range.start.line
  let col = params.range.start.character

  Just (loc, name) <- gets MD (findInTreeLoc (cast params.range.start) (cast params.range.end) . nameLocMap)
    | Nothing => do logString Debug "caseSplit: couldn't find name at \{show (line, col)}"
                    pure Nothing

  let find = if col > 0 then within (line, col) else onLine line

  OK splits <- getSplits (anyAt find) name
     | SplitFail err => do
        logString Debug "caseSplit: found error when splitting: \{show err}"
        pure Nothing

  lines <- updateCase splits line col
  Just original <- getSourceLine (line + 1)
    | Nothing => do logString Debug $ "caseSplit: error while fetching the referenced line"
                    pure Nothing

  let docURI = params.textDocument.uri
  let rng = MkRange (MkPosition line 0)
                    (MkPosition line (cast (length original)))
  let edit = MkTextEdit rng (unlines lines)
  let workspaceEdit = MkWorkspaceEdit
        { changes           = Just (singleton docURI [edit])
        , documentChanges   = Nothing
        , changeAnnotations = Nothing
        }
  let action = MkCodeAction
        { title       = "Case split on ?\{show name}"
        , kind        = Just RefactorRewrite
        , diagnostics = Just []
        , isPreferred = Just False
        , disabled    = Nothing
        , edit        = Just workspaceEdit
        , command     = Nothing
        , data_       = Nothing
        }
  modify LSPConf (record { cachedActions $= insert (cast loc, CaseSplit, [action]) })
  pure (Just action)

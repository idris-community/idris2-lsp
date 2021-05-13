module Language.LSP.CodeAction.MakeWith

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.List
import Data.List1
import Data.String
import Idris.IDEMode.MakeClause
import Idris.REPL.Opts
import Idris.Syntax
import Idris.Resugar
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.Message
import Libraries.Data.List.Extra
import Libraries.Data.PosMap
import Parser.Unlit
import Server.Configuration
import Server.Log
import Server.Utils
import TTImp.TTImp

buildCodeAction : Name -> URI -> List TextEdit -> CodeAction
buildCodeAction name uri edits =
  let workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri edits)
                                      , documentChanges   = Nothing
                                      , changeAnnotations = Nothing
                                      }
   in MkCodeAction { title       = "Make with of \{show name}"
                   , kind        = Just RefactorRewrite
                   , diagnostics = Just []
                   , isPreferred = Just False
                   , disabled    = Nothing
                   , edit        = Just workspaceEdit
                   , command     = Nothing
                   , data_       = Nothing
                   }

export
makeWith : Ref LSPConf LSPConfiguration
        => Ref MD Metadata
        => Ref Ctxt Defs
        => Ref UST UState
        => Ref Syn SyntaxInfo
        => Ref ROpts REPLOpts
        => CodeActionParams -> Core (Maybe CodeAction)
makeWith params = do
  let True = params.range.start.line == params.range.end.line
    | _ => do logString Debug "makeWith: start and end line were different"
              pure Nothing

  [] <- searchCache params.range MakeWith
    | action :: _ => do logString Debug "makeWith: found cached action"
                        pure (Just action)

  nameLocs <- gets MD nameLocMap
  let line = params.range.start.line
  let col = params.range.start.character
  let Just (loc@(_, nstart, nend), name) = findPointInTreeLoc (line, col) nameLocs
    | Nothing => do logString Debug "makeWith: couldn't find name at \{show (line, col)}"
                    pure Nothing

  context <- gets Ctxt gamma
  [(_, _, Hole locs _, _)] <- lookupNameBy (\g => (definition g, type g)) name context
    | _ => do logString Debug $ "makeWith: \{show name} is not a metavariable"
              pure Nothing

  litStyle <- getLitStyle
  Just src <- getSourceLine (line + 1)
    | Nothing => do logString Debug $ "makeWith: error while fetching the referenced line"
                    pure Nothing
  let Right l = unlit litStyle src
    | Left err => do logString Debug $ "makeWith: invalid literate Idris"
                     pure Nothing
  let (markM, _) = isLitLine src
  let with_ = makeWith name l
  let range = MkRange (MkPosition line 0) (MkPosition line (cast (length src) - 1))
  let edit = MkTextEdit range with_
  let action = buildCodeAction name params.textDocument.uri [edit]

  modify LSPConf (record { cachedActions $= insert (cast loc, MakeWith, [action]) })
  pure $ Just action

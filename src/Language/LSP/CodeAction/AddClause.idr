module Language.LSP.CodeAction.AddClause

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.List
import Data.List1
import Data.String
import Idris.IDEMode.CaseSplit
import Idris.REPL.Opts
import Idris.Syntax
import Idris.Resugar
import Language.JSON
import Language.LSP.Message
import Libraries.Data.List.Extra
import Libraries.Data.PosMap
import Parser.Unlit
import Server.Configuration
import Server.Log
import Server.Utils
import TTImp.TTImp

buildCodeAction : URI -> List TextEdit -> CodeAction
buildCodeAction uri edits =
  let workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri edits)
                                      , documentChanges   = Nothing
                                      , changeAnnotations = Nothing
                                      }
   in MkCodeAction { title       = "Add clause"
                   , kind        = Just RefactorRewrite
                   , diagnostics = Just []
                   , isPreferred = Just False
                   , disabled    = Nothing
                   , edit        = Just workspaceEdit
                   , command     = Nothing
                   , data_       = Nothing
                   }

export
addClause : Ref LSPConf LSPConfiguration
         => Ref MD Metadata
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref ROpts REPLOpts
         => CodeActionParams -> Core (Maybe CodeAction)
addClause params = do
  let True = params.range.start.line == params.range.end.line
    | _ => do logString Debug "addClause: start and end line were different"
              pure Nothing

  let line = params.range.start.line
  Just clause <- getClause (line + 1) (UN "")
    | Nothing => do logString Debug "addClause: not defined here"
                    pure Nothing

  let range = MkRange (MkPosition (line + 1) 0) (MkPosition (line + 1) 0)
  let edit = MkTextEdit range (clause ++ "\n")

  pure $ Just $ buildCodeAction params.textDocument.uri [edit]

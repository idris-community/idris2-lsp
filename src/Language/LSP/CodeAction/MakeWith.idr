module Language.LSP.CodeAction.MakeWith

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Idris.IDEMode.MakeClause
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.CodeAction.Utils
import Language.LSP.Message
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

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (Other "refactor.rewrite.MakeWith" `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

export
makeWith : Ref LSPConf LSPConfiguration
        => Ref MD Metadata
        => Ref Ctxt Defs
        => Ref UST UState
        => Ref Syn SyntaxInfo
        => Ref ROpts REPLOpts
        => CodeActionParams -> Core (Maybe CodeAction)
makeWith params = do
  let True = isAllowed params
    | False => do logI MakeWith "Skipped"
                  pure Nothing
  logI MakeWith "Checking for \{show params.textDocument.uri} at \{show params.range}"
  withSingleLine MakeWith params (pure Nothing) $ \line => do
    withSingleCache MakeWith params MakeWith $ do

      nameLocs <- gets MD nameLocMap
      let col = params.range.start.character
      let Just (loc@(_, nstart, nend), name) = findPointInTreeLoc (line, col) nameLocs
        | Nothing => do logD MakeWith "No name found at \{show line}:\{show col}}"
                        pure Nothing
      logD MakeCase "Found name \{show name}"

      context <- gets Ctxt gamma
      [(_, _, Hole locs _, _)] <- lookupNameBy (\g => (definition g, type g)) name context
        | _ => do logD MakeWith "\{show name} is not a metavariable"
                  pure Nothing

      logD MakeCase "Found metavariable \{show name}"
      litStyle <- getLitStyle
      Just src <- getSourceLine (line + 1)
        | Nothing => do logE MakeWith "Error while fetching the referenced line"
                        pure Nothing
      let Right l = unlit litStyle src
        | Left err => do logE MakeWith $ "Invalid literate Idris"
                         pure Nothing
      let (markM, _) = isLitLine src
      let with_ = makeWith name l
      let range = MkRange (MkPosition line 0) (MkPosition line (cast (length src) - 1))
      let edit = MkTextEdit range with_
      let action = buildCodeAction name params.textDocument.uri [edit]

      pure $ Just (cast loc, action)

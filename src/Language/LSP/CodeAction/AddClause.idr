module Language.LSP.CodeAction.AddClause

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Idris.IDEMode.CaseSplit
import Idris.REPL.Opts
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction.Utils
import Language.LSP.Message
import Language.LSP.Utils
import Server.Configuration
import Server.Log
import Server.Utils

buildCodeAction : URI -> TextEdit -> CodeAction
buildCodeAction uri edit =
  MkCodeAction
    { title       = "Add clause"
    , kind        = Just RefactorRewrite
    , diagnostics = Nothing
    , isPreferred = Nothing
    , disabled    = Nothing
    , edit        = Just $ MkWorkspaceEdit
        { changes           = Just (singleton uri [edit])
        , documentChanges   = Nothing
        , changeAnnotations = Nothing
        }
    , command     = Nothing
    , data_       = Nothing
    }

export
addClauseKind : CodeActionKind
addClauseKind = Other "refactor.rewrite.AddClause"

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (addClauseKind `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

export
addClause : Ref LSPConf LSPConfiguration
         => Ref Ctxt Defs
         => Ref MD Metadata
         => Ref ROpts REPLOpts
         => Ref Syn SyntaxInfo
         => Ref UST UState
         => CodeActionParams -> Core (Maybe CodeAction)
addClause params = do
  let True = isAllowed params
    | False => logI AddClause "Skipped" >> pure Nothing
  logI AddClause "Checking for \{show params.textDocument.uri} at \{show params.range}"

  withSingleLine AddClause params (pure Nothing) $ \line => do
    Just clause <- getClause (line + 1) (UN $ Basic "")
      | Nothing => logD AddClause "No clause defined at line \{show line}" >> pure Nothing

    Just (loc, nidx, envlen, ty) <- findTyDeclAt (\p, n => onLine line p)
      | Nothing => logE AddClause "Cannot find declaration at line \{show line}" >> pure Nothing
    let newLine = endLine loc + 1

    let range = MkRange (MkPosition newLine 0) (MkPosition newLine 0)
    let edit = MkTextEdit range (clause ++ "\n")

    pure $ Just $ buildCodeAction params.textDocument.uri edit

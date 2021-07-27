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
addClause : Ref LSPConf LSPConfiguration
         => Ref Ctxt Defs
         => Ref MD Metadata
         => Ref ROpts REPLOpts
         => Ref Syn SyntaxInfo
         => Ref UST UState
         => CodeActionParams -> Core (Maybe CodeAction)
addClause params = do
  logI AddClause "Checking for \{show params.textDocument.uri} at \{show params.range}"
  withSingleLine AddClause params (pure Nothing) $ \line => do

    Just clause <- getClause (line + 1) (UN "")
      | Nothing => do logD AddClause "No clause defined at line \{show line}"
                      pure Nothing

    -- FIXME: check where the declaration ends instead of putting it one line under the cursor
    let range = MkRange (MkPosition (line + 1) 0) (MkPosition (line + 1) 0)
    let edit = MkTextEdit range (clause ++ "\n")

    pure $ Just $ buildCodeAction params.textDocument.uri edit

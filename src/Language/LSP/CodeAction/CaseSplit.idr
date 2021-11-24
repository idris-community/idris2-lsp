module Language.LSP.CodeAction.CaseSplit

import Core.Context
import Core.Core
import Core.Metadata
import Core.UnifyState
import Data.String
import Idris.IDEMode.CaseSplit
import Idris.REPL.Opts
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.CodeAction.Utils
import Language.LSP.Message
import Server.Configuration
import Server.Log
import Server.Utils
import TTImp.Interactive.CaseSplit

buildCodeAction : URI -> Name -> TextEdit -> CodeAction
buildCodeAction uri name edit =
  MkCodeAction
    { title       = "Case split on ?\{show name}"
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
caseSplitKind : CodeActionKind
caseSplitKind = Other "refactor.rewrite.CaseSplit"

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (caseSplitKind `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

export
caseSplit : Ref LSPConf LSPConfiguration
         => Ref MD Metadata
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref ROpts REPLOpts
         => CodeActionParams -> Core (Maybe CodeAction)
caseSplit params = do
  let True = isAllowed params
    | False => logI CaseSplit "Skipped" >> pure Nothing
  logI CaseSplit "Checking for \{show params.textDocument.uri} at \{show params.range}"

  withSingleLine CaseSplit params (pure Nothing) $ \line => do
    withSingleCache CaseSplit params CaseSplit $ do

      let col = params.range.start.character
      Just (loc, name) <- gets MD (findInTreeLoc (cast params.range.start) (cast params.range.end) . nameLocMap)
        | Nothing => logD CaseSplit "No name found at \{show line}:\{show col}}" >> pure Nothing

      logD CaseSplit "Found name \{show name}"
      let find = if col > 0 then within (line, col) else onLine line
      OK splits <- getSplits (anyAt find) name
        | SplitFail err => logD CaseSplit "Error while splitting: \{show err}" >> pure Nothing

      lines <- updateCase splits line col
      Just original <- getSourceLine (line + 1)
        | Nothing => do logE CaseSplit "Cannot fetch referenced line even if compiler command succeded"
                        pure Nothing

      let rng = MkRange (MkPosition line 0) (MkPosition line (cast (length original)))
      let edit = MkTextEdit rng (unlines lines)
      pure $ Just (cast loc, buildCodeAction params.textDocument.uri name edit)

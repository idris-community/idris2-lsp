module Language.LSP.CodeAction.MakeCase

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.String
import Idris.IDEMode.CaseSplit
import Idris.IDEMode.MakeClause
import Idris.REPL.Opts
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.CodeAction.Utils
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Configuration
import Server.Log
import Server.Utils
import TTImp.Interactive.CaseSplit

currentLineRange : Int -> String -> Range
currentLineRange i origLine =
  MkRange (MkPosition i 0)
          (MkPosition i (cast $ Prelude.String.length origLine))

isHole : Defs -> Name -> Core Bool
isHole defs n
    = do Just def <- lookupCtxtExact n (gamma defs)
              | Nothing => do pure False
         case definition def of
              Hole _ _ => pure True
              _ => pure False

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (Other "refactor.rewrite.MakeCase" `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

export
handleMakeCase : Ref LSPConf LSPConfiguration
         => Ref MD Metadata
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref ROpts REPLOpts
         => CodeActionParams -> Core (Maybe CodeAction)
handleMakeCase params = do
  let True = isAllowed params
    | False => do logI MakeCase "Skipped"
                  pure Nothing
  logI MakeCase "Checking for \{show params.textDocument.uri} at \{show params.range}"
  withSingleLine MakeCase params (pure Nothing) $ \line => do
    withSingleCache MakeCase params MakeCase $ do

      meta <- get MD
      let col = params.range.start.character
      let Just (loc, name) = findPointInTreeLoc (line, col) (nameLocMap meta)
        | Nothing => do logD MakeCase "No name found at \{show line}:\{show col}}"
                        pure Nothing

      logD MakeCase "Found name \{show name}"
      defs <- get Ctxt
      True <- isHole defs name -- should only work on holes
        | _ => do logD MakeCase "\{show name} is not a metavariable"
                  pure Nothing

      Just original <- getSourceLine (line + 1)
        | Nothing => do logE MakeCase "Error while fetching the referenced line"
                        pure Nothing
      let name' = dropAllNS name
      let newText = makeCase False name' original -- TODO sort out brackets arg
      let rng = currentLineRange line original
      let edit = MkTextEdit rng newText

      let docURI = params.textDocument.uri
      let workspaceEdit = MkWorkspaceEdit
            { changes           = Just (singleton docURI [edit])
            , documentChanges   = Nothing
            , changeAnnotations = Nothing
            }
      let codeAction = MkCodeAction
            { title       = "Make case for hole ?\{show name'}"
            , kind        = Just RefactorRewrite
            , diagnostics = Just []
            , isPreferred = Just False -- not a quickfix
            , disabled    = Nothing
            , edit        = Just workspaceEdit
            , command     = Nothing
            , data_       = Nothing
            }
      pure $ Just (cast loc, codeAction)

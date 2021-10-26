module Language.LSP.CodeAction.MakeLemma

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.String
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.CodeAction.Utils
import Language.LSP.Message
import Libraries.Data.List.Extra
import Libraries.Data.PosMap
import Parser.Unlit
import Server.Configuration
import Server.Log
import Server.Utils
import TTImp.Interactive.MakeLemma
import TTImp.TTImp
import TTImp.TTImp.Functor

buildCodeAction : Name -> URI -> List TextEdit -> CodeAction
buildCodeAction name uri edits =
  let workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri edits)
                                      , documentChanges   = Nothing
                                      , changeAnnotations = Nothing
                                      }
   in MkCodeAction { title       = "Make lemma of \{show name}"
                   , kind        = Just RefactorExtract
                   , diagnostics = Just []
                   , isPreferred = Just False
                   , disabled    = Nothing
                   , edit        = Just workspaceEdit
                   , command     = Nothing
                   , data_       = Nothing
                   }

findBlankLine : List String -> Int -> Int
findBlankLine [] acc = acc
findBlankLine (x :: xs) acc = if trim x == "" then acc else findBlankLine xs (acc - 1)

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (Other "refactor.rewrite.MakeLemma" `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

export
makeLemma : Ref LSPConf LSPConfiguration
         => Ref MD Metadata
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref ROpts REPLOpts
         => CodeActionParams -> Core (Maybe CodeAction)
makeLemma params = do
  let True = isAllowed params
    | False => do logI MakeLemma "Skipped"
                  pure Nothing
  logI MakeLemma "Checking for \{show params.textDocument.uri} at \{show params.range}"
  withSingleLine MakeLemma params (pure Nothing) $ \line => do
    withSingleCache MakeLemma params MakeLemma $ do

      nameLocs <- gets MD nameLocMap
      let col = params.range.start.character
      let Just (loc@(_, nstart, nend), name) = findPointInTreeLoc (line, col) nameLocs
        | Nothing => do logD MakeLemma "No name found at \{show line}:\{show col}}"
                        pure Nothing
      logD MakeLemma "Found name \{show name}"

      context <- gets Ctxt gamma
      toBeBracketed <- gets Syn bracketholes
      let toBrack = elemBy (\x, y => dropNS x == dropNS y) name toBeBracketed

      [(n, nidx, Hole locs _, ty)] <- lookupNameBy (\g => (definition g, type g)) name context
        | _ => do logD MakeLemma $ "\{show name} is not a metavariable"
                  pure Nothing
      logD MakeLemma "Found metavariable \{show name}"

      (lty, lapp) <- makeLemma replFC name locs ty
      lemmaTy <- pterm $ map defaultKindedName lty
      papp <- pterm $ map defaultKindedName lapp
      let lemmaApp = show $ the IPTerm $ if toBrack then addBracket replFC papp else papp

      src <- lines <$> getSource
      let Just srcLine = elemAt src (integerToNat (cast line))
        | Nothing => do logE MakeLemma $ "Error while fetching the referenced line"
                        pure Nothing
      let (markM, _) = isLitLine srcLine
      -- TODO: currently it has the same behaviour of the compiler, it puts the new
      --       definition in the first blank line above the hole. We want to put it
      --       exactly above the clause or declaration that uses the hole, however
      --       this information is needed within the compiler, so waiting future PRs.
      let blank = findBlankLine (reverse $ take (integerToNat (cast line)) src) line

      let appEdit = MkTextEdit (MkRange (uncurry MkPosition nstart) (uncurry MkPosition nend)) lemmaApp
      let tyEdit = MkTextEdit (MkRange (MkPosition blank 0) (MkPosition blank 0))
                              (relit markM "\{show $ dropNS name} : \{show lemmaTy}\n\n")
      let action = buildCodeAction name params.textDocument.uri [tyEdit, appEdit]
      pure $ Just (cast loc, action)

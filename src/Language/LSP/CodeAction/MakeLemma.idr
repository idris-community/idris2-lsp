module Language.LSP.CodeAction.MakeLemma

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.List
import Data.List1
import Data.String
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
import TTImp.Interactive.MakeLemma

buildCodeAction : Name -> URI -> List TextEdit -> CodeAction
buildCodeAction name uri edits =
  let workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri edits)
                                      , documentChanges   = Nothing
                                      , changeAnnotations = Nothing
                                      }
   in MkCodeAction { title       = "Make lemma of \{show name}"
                   , kind        = Just (Other "refactor.rewrite.auto")
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

export
makeLemma : Ref LSPConf LSPConfiguration
         => Ref MD Metadata
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref ROpts REPLOpts
         => CodeActionParams -> Core (Maybe CodeAction)
makeLemma params = do
  let True = params.range.start.line == params.range.end.line
    | _ => do logString Debug "makeLemma: start and end line were different"
              pure Nothing

  [] <- searchCache params.range MakeLemma
    | action :: _ => do logString Debug "makeLemma: found cached action"
                        pure (Just action)

  nameLocs <- gets MD nameLocMap
  let line = params.range.start.line
  let col = params.range.start.character
  let Just (loc@(_, nstart, nend), name) = findPointInTreeLoc (line, col) nameLocs
    | Nothing => do logString Debug "makeLemma: couldn't find name at \{show (line, col)}"
                    pure Nothing

  context <- gets Ctxt gamma
  toBeBracketed <- gets Syn bracketholes
  let toBrack = elemBy (\x, y => dropNS x == dropNS y) name toBeBracketed

  [(n, nidx, Hole locs _, ty)] <- lookupNameBy (\g => (definition g, type g)) name context
    | _ => do logString Debug $ "makeLemma: \{show name} is not a metavariable"
              pure Nothing

  (lty, lapp) <- makeLemma replFC name locs ty
  lemmaTy <- pterm lty
  papp <- pterm lapp
  let lemmaApp = show $ the PTerm $ if toBrack then addBracket replFC papp else papp

  src <- forget . lines <$> getSource
  let Just srcLine = elemAt src (integerToNat (cast line))
    | Nothing => do logString Debug $ "makeLemma: error while fetching the referenced line"
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
  modify LSPConf (record { cachedActions $= insert (cast loc, MakeLemma, [action]) })
  pure $ Just action

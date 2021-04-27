module Language.LSP.CodeAction.ExprSearch

import Core.CaseTree
import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.List
import Data.String
import Idris.REPL.Opts
import Idris.Syntax
import Idris.Resugar
import Language.JSON
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Configuration
import Server.Utils
import TTImp.TTImp
import TTImp.Interactive.ExprSearch

dropLams : Nat -> RawImp -> RawImp
dropLams Z tm = tm
dropLams (S k) (ILam _ _ _ _ _ sc) = dropLams k sc
dropLams _ tm = tm

dropLamsTm : {vars : _} -> Nat -> Env Term vars -> Term vars -> (vars' ** (Env Term vars', Term vars'))
dropLamsTm Z env tm = (_ ** (env, tm))
dropLamsTm (S k) env (Bind _ _ b sc) = dropLamsTm k (b :: env) sc
dropLamsTm _ env tm = (_ ** (env, tm))

nextProofSearch : Ref Ctxt Defs
               => Ref UST UState
               => Ref ROpts REPLOpts
               => Core (Maybe (Name, RawImp))
nextProofSearch = do
  opts <- get ROpts
  let Just (n, res) = psResult opts
    | Nothing => pure Nothing
  Just (res, next) <- nextResult res
    | Nothing => do put ROpts (record { psResult = Nothing } opts)
                    pure Nothing
  put ROpts (record { psResult = Just (n, next) } opts)
  pure (Just (n, res))

fueledRepeat : Nat -> List a -> Core (Maybe a) -> Core (List a)
fueledRepeat Z acc f = pure (reverse acc)
fueledRepeat (S k) acc f = do Just res <- f
                                | Nothing => pure (reverse acc)
                              fueledRepeat k (res :: acc) f

buildCodeAction : Name -> URI -> Range -> String -> CodeAction
buildCodeAction name uri range str =
  let textEdit = MkTextEdit range str
      workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri [textEdit])
                                      , documentChanges   = Nothing
                                      , changeAnnotations = Nothing
                                      }
   in MkCodeAction { title       = "Expression search on \{show name}"
                   , kind        = Just (Other "refactor.rewrite.auto")
                   , diagnostics = Just []
                   , isPreferred = Just False
                   , disabled    = Nothing
                   , edit        = Just workspaceEdit
                   , command     = Nothing
                   , data_       = Nothing
                   }
export
exprSearch : Ref LSPConf LSPConfiguration
          => Ref MD Metadata
          => Ref Ctxt Defs
          => Ref UST UState
          => Ref Syn SyntaxInfo
          => Ref ROpts REPLOpts
          => CodeActionParams -> Core (List CodeAction)
exprSearch params = do
  let True = params.range.start.line == params.range.end.line
    | _ => pure []

  fuel <- gets LSPConf searchLimit
  nameLocs <- gets MD nameLocMap
  let line = params.range.start.line
  let col = params.range.start.character
  let Just ((_, nstart, nend), name) = findInTreeLoc (line, col) nameLocs
    | Nothing => pure []

  context <- gets Ctxt gamma
  toBeBracketed <- gets Syn bracketholes
  let toBrack = elemBy (\x, y => dropNS x == dropNS y) name toBeBracketed
  solutions <- case !(lookupDefName name context) of
                    [(n, nidx, Hole locs _)] => do
                      let searchtm = exprSearch replFC name []
                      ropts <- get ROpts
                      put ROpts (record { psResult = Just (name, searchtm) } ropts)
                      fueledRepeat fuel [] (do Just (_, restm) <- nextProofSearch
                                                 | Nothing => pure Nothing
                                               let tm' = dropLams locs restm
                                               itm <- pterm tm'
                                               let itm' : PTerm = if toBrack then addBracket replFC itm else itm
                                               pure $ Just (show itm'))
                    [(n, nidx, PMDef pi [] (STerm _ tm) _ _)] => case holeInfo pi of
                      NotHole => pure []
                      SolvedHole locs => do
                        let (_ ** (env, tm')) = dropLamsTm locs [] tm
                        itm <- resugar env tm'
                        let itm' : PTerm = if toBrack then addBracket replFC itm else itm
                        pure [show itm']
                    _ => pure []

  let range = MkRange (uncurry MkPosition nstart) (uncurry MkPosition nend)
  pure $ map (buildCodeAction name params.textDocument.uri range) solutions

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
import Language.LSP.CodeAction
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Configuration
import Server.Log
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

buildCodeAction : Name -> URI -> Range -> String -> CodeAction
buildCodeAction name uri range str =
  let textEdit = MkTextEdit range str
      workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri [textEdit])
                                      , documentChanges   = Nothing
                                      , changeAnnotations = Nothing
                                      }
   in MkCodeAction { title       = "Expression search on \{show $ dropAllNS name} as ~ \{strSubstr 0 50 str} ..."
                   , kind        = Just RefactorRewrite
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

  [] <- searchCache params.range ExprSearch
    | actions => do logString Debug "exprSearch: found cached action"
                    pure actions

  fuel <- gets LSPConf searchLimit
  nameLocs <- gets MD nameLocMap
  let line = params.range.start.line
  let col = params.range.start.character
  let Just (loc@(_, nstart, nend), name) = findPointInTreeLoc (line, col) nameLocs
    | Nothing => pure []

  context <- gets Ctxt gamma
  toBrack <- gets Syn (elemBy (\x, y => dropNS x == dropNS y) name . bracketholes)
  let bracket = the (PTerm -> PTerm) $ if toBrack then addBracket replFC else id
  solutions <-
    case !(lookupDefName name context) of
         [(n, nidx, Hole locs _)] =>
           catch (do searchtms <- exprSearchN replFC fuel name []
                     traverse (map (show . bracket) . pterm . dropLams locs) searchtms)
                 (\case Timeout _ => pure []
                        err => do logString Debug "exprSearch: unexpected error while searching"
                                  throw err)
         [(n, nidx, PMDef pi [] (STerm _ tm) _ _)] => case holeInfo pi of
           NotHole => pure []
           SolvedHole locs => do
             let (_ ** (env, tm')) = dropLamsTm locs [] tm
             itm <- resugar env tm'
             pure [show $ bracket itm]
         _ => pure []

  let range = MkRange (uncurry MkPosition nstart) (uncurry MkPosition nend)
  let actions = map (buildCodeAction name params.textDocument.uri range) solutions
  modify LSPConf (record { cachedActions $= insert (cast loc, ExprSearch, actions) })
  pure actions

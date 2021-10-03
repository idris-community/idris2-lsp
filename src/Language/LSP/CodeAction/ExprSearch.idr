module Language.LSP.CodeAction.ExprSearch

import Core.Case.CaseTree
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
import Server.Configuration
import Server.Log
import Server.Utils
import TTImp.Interactive.ExprSearch
import TTImp.TTImp
import TTImp.TTImp.Functor

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
  MkCodeAction
    { title       = "Expression search on \{show $ dropAllNS name} as ~ \{strSubstr 0 50 str} ..."
    , kind        = Just RefactorRewrite
    , diagnostics = Just []
    , isPreferred = Just False
    , disabled    = Nothing
    , edit        = Just $ MkWorkspaceEdit
        { changes           = Just (singleton uri [MkTextEdit range str])
        , documentChanges   = Nothing
        , changeAnnotations = Nothing
        }
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
  logI ExprSearch "Checking for \{show params.textDocument.uri} at \{show params.range}"
  withSingleLine ExprSearch params (pure []) $ \line => do
    withMultipleCache ExprSearch params ExprSearch $ do

      fuel <- gets LSPConf searchLimit
      nameLocs <- gets MD nameLocMap
      let col = params.range.start.character
      let Just (loc@(_, nstart, nend), name) = findPointInTreeLoc (line, col) nameLocs
        | Nothing => do logD ExprSearch "No name found at \{show line}:\{show col}}"
                        pure []
      logD ExprSearch "Found name \{show name}"

      defs <- get Ctxt
      let context = defs.gamma
      toBrack <- gets Syn (elemBy (\x, y => dropNS x == dropNS y) name . bracketholes)
      let bracket = the (IPTerm -> IPTerm) $ if toBrack then addBracket replFC else id
      solutions <-
        case !(lookupDefName name context) of
             [(n, nidx, Hole locs _)] =>
               catch (do searchtms <- exprSearchN replFC fuel name []
                         traverse (map (show . bracket) . pterm . map defaultKindedName . dropLams locs) searchtms)
                     (\case Timeout _ => do logI ExprSearch "Timed out"
                                            pure []
                            err => do logC ExprSearch "Unexpected error while searching"
                                      throw err)
             [(n, nidx, PMDef pi [] (STerm _ tm) _ _)] => case holeInfo pi of
               NotHole => do logD ExprSearch "\{show name} is not a metavariable"
                             pure []
               SolvedHole locs => do
                 let (_ ** (env, tm')) = dropLamsTm locs [] !(normaliseHoles defs [] tm)
                 itm <- resugar env tm'
                 pure [show $ bracket itm]
             _ => do logD ExprSearch "\{show name} is not a metavariable"
                     pure []

      let range = MkRange (uncurry MkPosition nstart) (uncurry MkPosition nend)
      let actions = buildCodeAction name params.textDocument.uri range <$> solutions
      pure [(cast loc, actions)]

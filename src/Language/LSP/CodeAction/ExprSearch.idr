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
import Language.LSP.Message.Derive
import Language.Reflection
import Parser.Rule.Source
import Parser.Source
import Server.Configuration
import Server.Log
import Server.Utils
import TTImp.Interactive.ExprSearch
import TTImp.TTImp
import TTImp.TTImp.Functor
import TTImp.Utils

%language ElabReflection
%hide TT.Name

export
record ExprSearchWithHintsParams where
  constructor MkExprSearchWithHintsParams
  codeAction : CodeActionParams
  hints : List String

%runElab deriveJSON defaultOpts `{ExprSearchWithHintsParams}

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
exprSearchKind : CodeActionKind
exprSearchKind = Other "refactor.rewrite.ExprSearch"

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (exprSearchKind `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

exprSearch' : Ref LSPConf LSPConfiguration
           => Ref MD Metadata
           => Ref Ctxt Defs
           => Ref UST UState
           => Ref Syn SyntaxInfo
           => Ref ROpts REPLOpts
           => CodeActionParams -> List Name -> Core (List CodeAction)
exprSearch' params hints = do
  let True = isAllowed params
    | False => logI ExprSearch "Skipped" >> pure []
  logI ExprSearch "Checking for \{show params.textDocument.uri} at \{show params.range}"

  withSingleLine ExprSearch params (pure []) $ \line => do
    fuel <- gets LSPConf searchLimit
    nameLocs <- gets MD nameLocMap
    let col = params.range.start.character
    let Just (loc@(_, nstart, nend), name) = findPointInTreeLoc (line, col) nameLocs
      | Nothing => logD ExprSearch "No name found at \{show line}:\{show col}}" >> pure []
    logD ExprSearch "Found name \{show name}"

    defs <- get Ctxt
    toBrack <- gets Syn (elemBy ((==) `on` dropNS) name . bracketholes)
    let showPTerm : IPTerm -> String = if toBrack then show . addBracket replFC else show
    names <- lookupDefName name defs.gamma
    solutions <- case names of
      [(n, nidx, Hole locs _)] =>
        catch (do searchtms <- exprSearchN replFC fuel name hints
                  traverse (map showPTerm . pterm . map defaultKindedName . dropLams locs) searchtms)
              (\case Timeout _ => logI ExprSearch "Timed out" >> pure []
                     err => logC ExprSearch "Unexpected error while searching" >> throw err)
      [(n, nidx, PMDef pi [] (STerm _ tm) _ _)] => case holeInfo pi of
        NotHole => logD ExprSearch "\{show name} is not a metavariable" >> pure []
        SolvedHole locs => do
          (_ ** (env, tm')) <- dropLamsTm locs [] <$> normaliseHoles defs [] tm
          itm <- resugar env tm'
          pure [showPTerm itm]
      _ => logD ExprSearch "\{show name} is not a metavariable" >> pure []

    let range = MkRange (uncurry MkPosition nstart) (uncurry MkPosition nend)
    let actions = buildCodeAction name params.textDocument.uri range <$> solutions
    pure actions

export
exprSearch : Ref LSPConf LSPConfiguration
          => Ref MD Metadata
          => Ref Ctxt Defs
          => Ref UST UState
          => Ref Syn SyntaxInfo
          => Ref ROpts REPLOpts
          => CodeActionParams -> Core (List CodeAction)
exprSearch params = exprSearch' params []

export
exprSearchWithHints : Ref LSPConf LSPConfiguration
                   => Ref MD Metadata
                   => Ref Ctxt Defs
                   => Ref UST UState
                   => Ref Syn SyntaxInfo
                   => Ref ROpts REPLOpts
                   => ExprSearchWithHintsParams -> Core (List CodeAction)
exprSearchWithHints params = do
  defs <- get Ctxt
  hs <- for params.hints $ \str => do
    let Right (_, _, n) = runParser (Virtual Interactive) Nothing str name
      | _ => pure []
    ns <- lookupCtxtName n defs.gamma
    pure $ fst <$> ns
  exprSearch' params.codeAction (concat hs)

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
import Language.LSP.Utils
import Libraries.Data.List.Extra
import Libraries.Data.PosMap
import Parser.Unlit
import Server.Configuration
import Server.Log
import Server.Utils
import TTImp.Interactive.MakeLemma
import TTImp.TTImp
import TTImp.TTImp.Functor

buildCodeAction : Name -> URI -> TextEdit -> TextEdit -> CodeAction
buildCodeAction name uri tyEdit appEdit =
  MkCodeAction
    { title       = "Make lemma for hole ?\{show name}"
    , kind        = Just RefactorExtract
    , diagnostics = Nothing
    , isPreferred = Nothing
    , disabled    = Nothing
    , edit        = Just $ MkWorkspaceEdit
        { changes           = Just (singleton uri [tyEdit, appEdit])
        , documentChanges   = Nothing
        , changeAnnotations = Nothing
        }
    , command     = Nothing
    , data_       = Nothing
    }

findBlankLine : List String -> Int -> Int
findBlankLine [] acc = acc
findBlankLine (x :: xs) acc = if trim x == "" then acc else findBlankLine xs (acc - 1)

export
makeLemmaKind : CodeActionKind
makeLemmaKind = Other "refactor.extract.MakeLemma"

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (makeLemmaKind `elem` filter) || (RefactorExtract `elem` filter)) params.context.only

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
    | False => logI MakeLemma "Skipped" >> pure Nothing
  logI MakeLemma "Checking for \{show params.textDocument.uri} at \{show params.range}"

  withSingleLine MakeLemma params (pure Nothing) $ \line => do
    withSingleCache MakeLemma params MakeLemma $ do

      nameLocs <- gets MD nameLocMap
      let col = params.range.start.character
      let Just (loc@(_, nstart, nend), name) = findPointInTreeLoc (line, col) nameLocs
        | Nothing => logD MakeLemma "No name found at \{show line}:\{show col}}" >> pure Nothing
      logD MakeLemma "Found name \{show name}"

      context <- gets Ctxt gamma
      toBrack <- gets Syn (elemBy ((==) `on` dropNS) name . bracketholes)
      let showPTerm : IPTerm -> String = if toBrack then show . addBracket replFC else show
      [(n, nidx, Hole locs _, ty)] <- lookupNameBy (\g => (definition g, type g)) name context
        | _ => logD MakeLemma "\{show name} is not a metavariable" >> pure Nothing
      logD MakeLemma "Found metavariable \{show name}"

      (lty, lapp) <- makeLemma replFC name locs ty
      lemmaTy <- pterm $ map defaultKindedName lty
      papp <- pterm $ map defaultKindedName lapp
      let lemmaApp = showPTerm papp

      src <- lines <$> getSource
      let Just srcLine = elemAt src (integerToNat (cast line))
        | Nothing => logE MakeLemma "Error while fetching the referenced line" >> pure Nothing
      let (markM, _) = isLitLine srcLine
      -- TODO: currently it has the same behaviour of the compiler, it puts the new
      --       definition in the first blank line above the hole. We want to put it
      --       exactly above the clause or declaration that uses the hole, however
      --       this information is needed within the compiler, so waiting future PRs.
      let blank = findBlankLine (reverse $ take (integerToNat (cast line)) src) line

      let appEdit = MkTextEdit (MkRange (uncurry MkPosition nstart) (uncurry MkPosition nend)) lemmaApp
      let tyEdit = MkTextEdit (MkRange (MkPosition blank 0) (MkPosition blank 0))
                              (relit markM "\{show $ dropNS name} : \{show lemmaTy}\n\n")
      let action = buildCodeAction name params.textDocument.uri tyEdit appEdit
      pure $ Just (cast loc, action)

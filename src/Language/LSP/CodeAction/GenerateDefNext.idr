module Language.LSP.CodeAction.GenerateDefNext

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
import Parser.Unlit
import Server.Configuration
import Server.Log
import Server.Utils
import Libraries.Data.Tap
import TTImp.Interactive.GenerateDef
import TTImp.Interactive.ExprSearch
import TTImp.TTImp
import TTImp.TTImp.Functor

printClause : Ref Ctxt Defs
           => Ref Syn SyntaxInfo
           => Maybe String -> Nat -> ImpClause -> Core String
printClause l i (PatClause _ lhsraw rhsraw) = do
  lhs <- pterm $ map defaultKindedName lhsraw
  rhs <- pterm $ map defaultKindedName rhsraw
  pure $ relit l "\{pack (replicate i ' ')}\{show lhs} = \{show rhs}"
printClause l i (WithClause _ lhsraw rig wvraw prf flags csraw) = do
  lhs <- pterm $ map defaultKindedName lhsraw
  wval <- pterm $ map defaultKindedName wvraw
  cs <- traverse (printClause l (i + 2)) csraw
  pure (relit l ((pack (replicate i ' ')
         ++ show lhs
         ++ " with \{showCount rig}(" ++ show wval ++ ")"
         ++ maybe "" (\ nm => " proof " ++ show nm) prf
         ++ "\n"))
         ++ showSep "\n" cs)
printClause l i (ImpossibleClause _ lhsraw) = do
  do lhs <- pterm $ map defaultKindedName lhsraw
     pure $ relit l "\{pack (replicate i ' ')}\{show lhs} impossible"

number : Nat -> List a -> List (Nat, a)
number n [] = []
number n (x :: xs) = (n, x) :: number (S n) xs

generateDefNextKind : CodeActionKind
generateDefNextKind = Other "refactor.rewrite.GenerateDefNext"

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (generateDefNextKind `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

buildCodeAction : URI -> TextEdit -> CodeAction
buildCodeAction uri edit =
  MkCodeAction
    { title       = "Generate next definition"
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

nextGenDef : {auto c : Ref Ctxt Defs} ->
             {auto u : Ref UST UState} ->
             {auto o : Ref ROpts REPLOpts} ->
             (reject : Nat) ->
             Core (Maybe (Int, (FC, List ImpClause)))
nextGenDef reject
    = do opts <- get ROpts
         let Just (line, res) = gdResult opts
              | Nothing => pure Nothing
         Just (res, next) <- nextResult res
              | Nothing =>
                    do put ROpts ({ gdResult := Nothing } opts)
                       pure Nothing
         put ROpts ({ gdResult := Just (line, next) } opts)
         case reject of
              Z => pure (Just (line, res))
              S k => nextGenDef k

export
generateDefNext : Ref LSPConf LSPConfiguration
               => Ref MD Metadata
               => Ref Ctxt Defs
               => Ref UST UState
               => Ref Syn SyntaxInfo
               => Ref ROpts REPLOpts
               => CodeActionParams -> Core (List CodeAction)
generateDefNext params = do
  let True = isAllowed params
    | False => logI GenerateDefNext "Skipped" >> pure []
  logI GenerateDefNext "Checking for \{show params.textDocument.uri} at \{show params.range}"

  withSingleLine GenerateDefNext params (pure []) $ \line => do
    withMultipleCache GenerateDefNext params GenerateDefNext $ do
      defs <- branch
      Just (loc, n, _, _) <- findTyDeclAt (\p, n => onLine line p)
        | Nothing => logD GenerateDef "No name found at line \{show line}" >> pure []
      logD CaseSplit "Found type declaration \{show n}"

      previousResults <- gdResult <$> get ROpts
      let staleDefs = case previousResults of
                           Nothing => True
                           Just (l, _) => l /= line
      when staleDefs $ do
        fuel <- gets LSPConf searchLimit
        results <- lookupDefExact n defs.gamma
        case results of
          Just None => do
             catch (do searchdefs <- makeDefN (\p, n => onLine line p) fuel n -- makeDefSort (\p, n => onLine line p) fuel mostUsed n
                       logD GenerateDefNext "got \{show $ length searchdefs} results"
                       let sd' : Search (FC, List ImpClause) = foldr (\n,acc => n :: pure acc) Nil searchdefs
                       update ROpts { gdResult := Just (line, pure sd') }
--                        Just _ <- nextGenDef 0
--                          | Nothing => pure ()
--                        logD GenerateDefNext "skipped one result"
--                        Just _ <- nextGenDef 0
--                          | Nothing => pure ()
--                        logD GenerateDefNext "got second result"
--                        Just _ <- nextGenDef 0
--                          | Nothing => pure ()
--                        logD GenerateDefNext "got third result"
                       pure ())
                   (\case Timeout _ => logI GenerateDefNext "Timed out" >> pure ()
                          err => logC GenerateDefNext "Unexpected error while searching" >> throw err)
          Just _ => logD GenerateDefNext "There is already a definition for \{show n}" >> pure ()
          Nothing => logD GenerateDefNext "Couldn't find type declaration at line \{show line}" >> pure ()

      Just (line, (fc, cs)) <- nextGenDef 0
        | Nothing => logD GenerateDefNext "No more results" >> pure []
      let l : Nat = integerToNat $ cast $ startCol (toNonEmptyFC fc)
      Just srcLine <- getSourceLine line
        | Nothing => logE GenerateDefNext "Source line \{show line} not found" >> pure []
      let (markM, srcLineUnlit) = isLitLine srcLine
      lines <- traverse (printClause markM l) cs

      put Ctxt defs

      let docURI = params.textDocument.uri
      let newLine = endLine loc + 1
      let rng = MkRange (MkPosition newLine 0) (MkPosition newLine 0) -- insert

      action <- do
        let edit = MkTextEdit rng (String.unlines lines)
        pure $ buildCodeAction docURI edit
      -- TODO: retrieve the line length efficiently
      pure [(MkRange (MkPosition line 0) (MkPosition line 1000), [action])]

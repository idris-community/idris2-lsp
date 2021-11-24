module Language.LSP.CodeAction.GenerateDef

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
import Parser.Unlit
import Server.Configuration
import Server.Log
import Server.Utils
import TTImp.Interactive.GenerateDef
import TTImp.TTImp
import TTImp.TTImp.Functor

printClause : Ref Ctxt Defs
           => Ref Syn SyntaxInfo
           => Maybe String -> Nat -> ImpClause -> Core String
printClause l i (PatClause _ lhsraw rhsraw) = do
  lhs <- pterm $ map defaultKindedName lhsraw
  rhs <- pterm $ map defaultKindedName rhsraw
  pure $ relit l "\{pack (replicate i ' ')}\{show lhs} = \{show rhs}"
printClause l i (WithClause _ lhsraw wvraw prf flags csraw) = do
  lhs <- pterm $ map defaultKindedName lhsraw
  wval <- pterm $ map defaultKindedName wvraw
  cs <- traverse (printClause l (i + 2)) csraw
  pure (relit l ((pack (replicate i ' ')
         ++ show lhs
         ++ " with (" ++ show wval ++ ")"
         ++ maybe "" (\ nm => " proof " ++ show nm) prf
         ++ "\n"))
         ++ showSep "\n" cs)
printClause l i (ImpossibleClause _ lhsraw) = do
  do lhs <- pterm $ map defaultKindedName lhsraw
     pure $ relit l "\{pack (replicate i ' ')}\{show lhs} impossible"

number : Nat -> List a -> List (Nat, a)
number n [] = []
number n (x :: xs) = (n, x) :: number (S n) xs

generateDefKind : CodeActionKind
generateDefKind = Other "refactor.rewrite.GenerateDef"

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (generateDefKind `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

buildCodeAction : URI -> Nat -> String -> TextEdit -> CodeAction
buildCodeAction uri i lastLine edit =
  MkCodeAction
    { title       = "Generate definition #\{show i} as ~ \{strSubstr 0 50 lastLine} ..."
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
generateDef : Ref LSPConf LSPConfiguration
           => Ref MD Metadata
           => Ref Ctxt Defs
           => Ref UST UState
           => Ref Syn SyntaxInfo
           => Ref ROpts REPLOpts
           => CodeActionParams -> Core (List CodeAction)
generateDef params = do
  let True = isAllowed params
    | False => logI GenerateDef "Skipped" >> pure []
  logI GenerateDef "Checking for \{show params.textDocument.uri} at \{show params.range}"

  withSingleLine GenerateDef params (pure []) $ \line => do
    withMultipleCache GenerateDef params GenerateDef $ do
      defs <- branch
      Just (loc, n, _, _) <- findTyDeclAt (\p, n => onLine line p)
        | Nothing => logD GenerateDef "No name found at line \{show line}" >> pure []
      logD CaseSplit "Found type declaration \{show n}"

      fuel <- gets LSPConf searchLimit
      results <- lookupDefExact n defs.gamma
      solutions <- case results of
        Just None => do
           catch (do searchdefs@((fc, _) :: _) <- makeDefN (\p, n => onLine line p) fuel n
                       | _ => pure []
                     let l : Nat = integerToNat $ cast $ startCol (toNonEmptyFC fc)
                     Just srcLine <- getSourceLine (line + 1)
                       | Nothing => logE GenerateDef "Source line \{show line} not found" >> pure []
                     let (markM, srcLineUnlit) = isLitLine srcLine
                     traverse (\(_, cs) => traverse (printClause markM l) cs) searchdefs)
                 (\case Timeout _ => logI GenerateDef "Timed out" >> pure []
                        err => logC GenerateDef "Unexpected error while searching" >> throw err)
        Just _ => logD GenerateDef "There is already a definition for \{show n}" >> pure []
        Nothing => logD GenerateDef "Couldn't find type declaration at line \{show line}" >> pure []

      put Ctxt defs

      let docURI = params.textDocument.uri
      let newLine = endLine loc + 1
      let rng = MkRange (MkPosition newLine 0) (MkPosition newLine 0) -- insert

      actions <- for (number 1 solutions) $ \(i,funcDef) => do
        let lastLine = fromMaybe "" (last' funcDef)
        let edit = MkTextEdit rng (String.unlines funcDef)
        pure $ buildCodeAction docURI i lastLine edit
      -- TODO: retrieve the line length efficiently
      pure [(MkRange (MkPosition line 0) (MkPosition line 1000), actions)]

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
  pure (relit l (pack (replicate i ' ') ++ show lhs ++ " = " ++ show rhs))
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
     pure (relit l (pack (replicate i ' ') ++ show lhs ++ " impossible"))

number : Nat -> List a -> List (Nat, a)
number n [] = []
number n (x :: xs) = (n,x) :: number (S n) xs

export
generateDef : Ref LSPConf LSPConfiguration
            => Ref MD Metadata
            => Ref Ctxt Defs
            => Ref UST UState
            => Ref Syn SyntaxInfo
            => Ref ROpts REPLOpts
            => CodeActionParams -> Core (List CodeAction)
generateDef params = do
  logI GenerateDef "Checking for \{show params.textDocument.uri} at \{show params.range}"
  withSingleLine GenerateDef params (pure []) $ \line => do
    withMultipleCache GenerateDef params GenerateDef $ do

      defs <- branch
      Just (_, n, _, _) <- findTyDeclAt (\p, n => onLine line p)
        | Nothing => do logD GenerateDef "No name found at line \{show line}"
                        pure []
      logD CaseSplit "Found type declaration \{show n}"

      fuel <- gets LSPConf searchLimit
      solutions <- case !(lookupDefExact n defs.gamma) of
        Just None => do
           catch (do searchdefs@((fc, _) :: _) <- makeDefN (\p, n => onLine line p) fuel n
                       | _ => pure []
                     let l : Nat = integerToNat $ cast $ startCol (toNonEmptyFC fc)
                     Just srcLine <- getSourceLine (line + 1)
                       | Nothing => do logE GenerateDef "Source line \{show line} not found"
                                       pure []
                     let (markM, srcLineUnlit) = isLitLine srcLine
                     for searchdefs $ \(_, cs) => do
                       traverse (printClause markM l) cs)
                 (\case Timeout _ => do logI GenerateDef "Timed out"
                                        pure []
                        err => do logC GenerateDef "Unexpected error while searching"
                                  throw err)
        Just _ => do logD GenerateDef "There is already a definition for \{show n}"
                     pure []
        Nothing => do logD GenerateDef "Couldn't find type declaration at line \{show line}"
                      pure []

      put Ctxt defs

      let docURI = params.textDocument.uri
      let rng = MkRange (MkPosition (line + 1) 0) (MkPosition (line + 1) 0) -- insert

      actions <- for (number 1 solutions) $ \(i,funcDef) => do
        let lastLine = fromMaybe "" (last' funcDef)
        let edit = MkTextEdit rng (String.unlines funcDef ++ "\n") -- extra newline
        let workspaceEdit = MkWorkspaceEdit
              { changes           = Just (singleton docURI [edit])
              , documentChanges   = Nothing
              , changeAnnotations = Nothing
              }
        pure $ MkCodeAction
          { title = "Generate definition #\{show i} as ~ \{strSubstr 0 50 lastLine} ..."
          , kind        = Just RefactorRewrite
          , diagnostics = Just []
          , isPreferred = Just False
          , disabled    = Nothing
          , edit        = Just workspaceEdit
          , command     = Nothing
          , data_       = Nothing
          }
      -- TODO: retrieve the line length efficiently
      pure [(MkRange (MkPosition line 0) (MkPosition line 1000), actions)]

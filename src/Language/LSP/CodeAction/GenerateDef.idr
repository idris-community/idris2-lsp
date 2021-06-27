module Language.LSP.CodeAction.GenerateDef

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.List
import Data.List1
import Data.String
import Idris.IDEMode.CaseSplit
import Idris.Pretty
import Idris.REPL
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.Message
import Libraries.Data.List.Extra
import Libraries.Data.PosMap
import Libraries.Text.PrettyPrint.Prettyprinter
import Server.Configuration
import Server.Response
import Server.Utils
import System.File
import Server.Log
import System.Clock
import TTImp.Interactive.GenerateDef
import TTImp.TTImp
import TTImp.Interactive.ExprSearch
import Parser.Unlit

printClause : Ref Ctxt Defs
           => Ref Syn SyntaxInfo
           => Maybe String -> Nat -> ImpClause -> Core String
printClause l i (PatClause _ lhsraw rhsraw) = do
  lhs <- pterm lhsraw
  rhs <- pterm rhsraw
  pure (relit l (pack (replicate i ' ') ++ show lhs ++ " = " ++ show rhs))
printClause l i (WithClause _ lhsraw wvraw prf flags csraw) = do
  lhs <- pterm lhsraw
  wval <- pterm wvraw
  cs <- traverse (printClause l (i + 2)) csraw
  pure (relit l ((pack (replicate i ' ')
         ++ show lhs
         ++ " with (" ++ show wval ++ ")"
         ++ maybe "" (\ nm => " proof " ++ show nm) prf
         ++ "\n"))
         ++ showSep "\n" cs)
printClause l i (ImpossibleClause _ lhsraw) = do
  do lhs <- pterm lhsraw
     pure (relit l (pack (replicate i ' ') ++ show lhs ++ " impossible"))

number : Nat -> List a -> List (Nat, a)
number n [] = []
number n (x :: xs) = (n,x) :: number (S n) xs

pred : Nat -> Nat
pred Z     = Z
pred (S k) = k

-- This is needed as PosMap is not smart enough yet to track down
-- the non applied functions in the PosMap. This requires a change
-- in the compiler, for that reason the generateDef functionality
-- needs to have a backup strategy for names. If the PosMap
-- creation of the compiler changes probably this functions
-- will be rendered as reduntant one.
guessName : Ref MD Metadata
         => (Int, Int) -> Core (Maybe Name)
guessName (line, col) = do
  meta <- get MD
  let Nothing = findPointInTree (line, col) (nameLocMap meta)
      | Just name => pure (Just name)
  Nothing <- findTypeAt $ anyAt $ within (line, col)
      | Just (name, _, _) => pure (Just name)
  pure Nothing

export
generateDef : Ref LSPConf LSPConfiguration
            => Ref MD Metadata
            => Ref Ctxt Defs
            => Ref UST UState
            => Ref Syn SyntaxInfo
            => Ref ROpts REPLOpts
            => CodeActionParams -> Core (List CodeAction)
generateDef params = do
  defs <- branch
  let True = params.range.start.line == params.range.end.line
      | _ => do
        logString Debug "generateDef: start and end lines were different."
        pure []

  [] <- searchCache params.range GenerateDef
    | actions => do logString Debug "generateDef: found cached action"
                    pure actions

  let line = params.range.start.line
  let col = params.range.start.character

  defs <- get Ctxt
  Just (_, n, _, _) <- findTyDeclAt (\p, n => onLine line p)
    | Nothing => do logString Debug "generateDef: couldn't find type declaration at line \{show line}"
                    pure []

  fuel <- gets LSPConf searchLimit
  solutions <- case !(lookupDefExact n defs.gamma) of
    Just None => do
       catch (do searchdefs@((fc, _) :: _) <- makeDefN (\p, n => onLine line p) fuel n
                   | _ => pure []
                 let l : Nat = integerToNat $ cast $ startCol (toNonEmptyFC fc)
                 Just srcLine <- getSourceLine (line + 1)
                   | Nothing => do logString Error "generateDef: source line \{show line} not found"
                                   pure []
                 let (markM, srcLineUnlit) = isLitLine srcLine
                 for searchdefs $ \(_, cs) => do
                   traverse (printClause markM l) cs)
             (\case Timeout _ => pure []
                    err => do logString Debug "generateDef: unexpected error while searching"
                              throw err)
    Just _ => do logString Debug "generateDef: there is already a definition for \{show n}"
                 pure []
    Nothing => do logString Debug "generateDef: couldn't find type declaration at line \{show line}"
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
  modify LSPConf (record { cachedActions $= insert (MkRange (MkPosition line 0) (MkPosition line 1000), GenerateDef, actions) })
  pure actions

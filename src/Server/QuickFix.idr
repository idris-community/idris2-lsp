module Server.QuickFix

import Core.Context
import Core.Core
import Core.Env
import Core.FC
import Core.Metadata
import Data.List
import Data.List1
import Data.String
import Idris.Pretty
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Language.LSP.Message
import Language.JSON
import Server.Configuration
import Server.Diagnostics
import Server.Utils
import Server.Log

pShowMN : {vars : _} -> Term vars -> Env Term vars -> Doc IdrisAnn -> Doc IdrisAnn
pShowMN (Local _ _ _ p) env acc =
  case dropAllNS (nameAt p) of
       MN _ _ => acc <++> parens ("implicitly bound at" <++> pretty0 (getBinderLoc p env))
       _      => acc
pShowMN _ _ acc = acc

pshow : {vars : _}
     -> Ref Ctxt Defs
     => Ref Syn SyntaxInfo
     => Env Term vars
     -> Term vars
     -> Core String
pshow env tm = do
  defs <- get Ctxt
  ntm <- normaliseHoles defs env tm
  itm <- resugar env ntm
  pure $ show (pShowMN ntm env $ prettyTerm itm)

addMissingCases : Ref Ctxt Defs
               => Ref Syn SyntaxInfo
               => String -> List ClosedTerm -> Nat -> Core String
addMissingCases n [] k = pure ""
addMissingCases n (x :: xs) k =
  [| pure "\{!(pshow Env.empty x)} = ?\{n}_missing_case_\{show k}\n" ++ addMissingCases n xs (S k) |]

findBlankLine : List String -> Int -> Int
findBlankLine [] acc = acc
findBlankLine (x :: xs) acc = if trim x == "" then acc else findBlankLine xs (acc + 1)

buildQuickfix : URI -> String -> Diagnostic -> TextEdit -> CodeAction
buildQuickfix uri msg diagnostic edit = MkCodeAction
  { title       = "QuickFix: \{msg}"
  , kind        = Just QuickFix
  , diagnostics = Just [diagnostic]
  , isPreferred = Just True
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
findQuickfix : Ref LSPConf LSPConfiguration
            => Ref Ctxt Defs
            => Ref Syn SyntaxInfo
            => Ref ROpts REPLOpts
            => Ref MD Metadata
            => (caps : Maybe PublishDiagnosticsClientCapabilities)
            -> (uri : URI)
            -> Error
            -> Core ()
findQuickfix caps uri (WhenUnifying fc _ _ x y err) = findQuickfix caps uri err
findQuickfix caps uri (InType _ _ err) = findQuickfix caps uri err
findQuickfix caps uri (InCon _ err) = findQuickfix caps uri err
findQuickfix caps uri (InLHS _ _ err) = findQuickfix caps uri err
findQuickfix caps uri (InRHS _ _ err) = findQuickfix caps uri err
findQuickfix caps uri err@(PatternVariableUnifies fc fct env n tm) =
  whenJust (isNonEmptyFC fct) $ \fc => do
    diagnostic <- errorToDiagnostic caps uri err
    let textEdit = MkTextEdit (cast fc) (nameRoot n)
    let codeAction = buildQuickfix uri "Unify pattern names" diagnostic textEdit
    update LSPConf ({ quickfixes $= (codeAction ::) })
findQuickfix caps uri err@(ValidCase fc _ (Left tm)) =
  whenJust (isNonEmptyFC fc) $ \fc => do
    Just (f, args) <- (Utils.uncons' <=< init' <=< map words) <$> (getSourceLine (startLine fc + 1))
      | Nothing => logE QuickFix "Error while fetching source line" >> pure ()
    let line = unwords $ f :: args ++ ["=", "?\{f}_rhs_not_impossible"]
    diagnostic <- errorToDiagnostic caps uri err
    let textEdit = MkTextEdit (cast fc) line
    let codeAction = buildQuickfix uri "Remove impossible keyword" diagnostic textEdit
    update LSPConf ({ quickfixes $= (codeAction ::) })
findQuickfix caps uri err@(NotCovering fc n (MissingCases cs)) =
  whenJust (isNonEmptyFC fc) $ \fc => do
    cases <- addMissingCases !(prettyName n) cs 1
    let startline = startLine fc
    let endline = endLine fc
    src <- String.lines <$> getSource
    let blank = findBlankLine (drop (integerToNat (cast endline)) src) endline
    diagnostic <- errorToDiagnostic caps uri err
    let range = MkRange (MkPosition blank 0) (MkPosition blank 0)
    let textEdit = MkTextEdit range cases
    let missingCodeAction = buildQuickfix uri "Add missing cases" diagnostic textEdit
    let range = MkRange (MkPosition startline 0) (MkPosition startline 0)
    let textEdit = MkTextEdit range "partial\n"
    let partialCodeAction = buildQuickfix uri "Add partial annotation" diagnostic textEdit
    update LSPConf ({ quickfixes $= ([missingCodeAction, partialCodeAction] ++) })
findQuickfix caps uri err@(NotCovering fc n _) =
  whenJust (isNonEmptyFC fc) $ \fc => do
    diagnostic <- errorToDiagnostic caps uri err
    let startline = startLine fc
    let range = MkRange (MkPosition startline 0) (MkPosition startline 0)
    let textEdit = MkTextEdit range "partial\n"
    let codeAction = buildQuickfix uri "Add partial annotation" diagnostic textEdit
    update LSPConf ({ quickfixes $= (codeAction ::) })
findQuickfix caps uri err@(NotTotal fc n _) =
  whenJust (isNonEmptyFC fc) $ \fc => do
    diagnostic <- errorToDiagnostic caps uri err
    let startline = startLine fc
    let range = MkRange (MkPosition startline 0) (MkPosition startline 0)
    let textEdit = MkTextEdit range "covering\n"
    let codeAction = buildQuickfix uri "Add covering annotation" diagnostic textEdit
    update LSPConf ({ quickfixes $= (codeAction ::) })
findQuickfix caps uri (MaybeMisspelling err ns) =
  whenJust (isNonEmptyFC =<< getErrorLoc err) $ \fc => do
    diagnostic <- errorToDiagnostic caps uri err
    let range = cast fc
    let codeActions = map (\n => buildQuickfix uri "Replace with \{show n}" diagnostic (MkTextEdit range n)) . forget $ ns
    update LSPConf ({ quickfixes $= (codeActions ++) })
findQuickfix _ _ _ = pure ()

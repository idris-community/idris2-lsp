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

pShowMN : {vars : _} -> Term vars -> Env t vars -> Doc IdrisAnn -> Doc IdrisAnn
pShowMN (Local _ _ _ p) env acc =
  case dropAllNS (nameAt p) of
       MN _ _ => acc <++> parens ("implicitly bound at" <++> pretty (getBinderLoc p env))
       _      => acc
pShowMN _ _ acc = acc

pshow : {vars : _}
     -> Ref Ctxt Defs
     => Ref Syn SyntaxInfo
     => Env Term vars
     -> Term vars
     -> Core String
pshow env tm
    = do defs <- get Ctxt
         ntm <- normaliseHoles defs env tm
         itm <- resugar env ntm
         pure $ show (pShowMN ntm env $ prettyTerm itm)

addMissingCases : Ref Ctxt Defs
               => Ref Syn SyntaxInfo
               => String -> List ClosedTerm -> Nat -> Core String
addMissingCases n [] k = pure ""
addMissingCases n (x :: xs) k =
  [| pure "\{!(pshow [] x)} = ?\{n}_missing_case_\{show k}\n" ++ addMissingCases n xs (S k) |]

findBlankLine : List String -> Int -> Int
findBlankLine [] acc = acc
findBlankLine (x :: xs) acc = if trim x == "" then acc else findBlankLine xs (acc + 1)

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
findQuickfix caps uri (WhenUnifying fc _ x y err) = findQuickfix caps uri err
findQuickfix caps uri (InType _ _ err) = findQuickfix caps uri err
findQuickfix caps uri (InCon _ _ err) = findQuickfix caps uri err
findQuickfix caps uri (InLHS _ _ err) = findQuickfix caps uri err
findQuickfix caps uri (InRHS _ _ err) = findQuickfix caps uri err
findQuickfix caps uri (MaybeMisspelling err _) = findQuickfix caps uri err
findQuickfix caps uri err@(PatternVariableUnifies fc env n tm) = do
  whenJust (isNonEmptyFC (getLoc tm)) $ \fc => do
    diagnostic <- toDiagnostic caps uri err
    let textEdit = MkTextEdit (cast fc) (nameRoot n)
    let workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri [textEdit])
                                        , documentChanges   = Nothing
                                        , changeAnnotations = Nothing
                                        }
    let codeAction = MkCodeAction { title       = "QuickFix: unify pattern names"
                                  , kind        = Just QuickFix
                                  , diagnostics = Just [diagnostic]
                                  , isPreferred = Just True
                                  , disabled    = Nothing
                                  , edit        = Just workspaceEdit
                                  , command     = Nothing
                                  , data_       = Nothing
                                  }
    modify LSPConf (record { quickfixes $= (codeAction ::) })
findQuickfix caps uri err@(ValidCase fc _ (Left tm)) =
  whenJust (isNonEmptyFC fc) $ \fc => do
    Just (f, args) <- (uncons' <=< init' <=< map words) <$> (getSourceLine (startLine fc + 1))
      | Nothing => do logString Debug "findQuickfix: error while fetching source line"
                      pure ()
    let line = unwords $ f :: args ++ ["=", "?\{f}_rhs_not_impossible"]
    diagnostic <- toDiagnostic caps uri err
    let textEdit = MkTextEdit (cast fc) line
    let workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri [textEdit])
                                        , documentChanges   = Nothing
                                        , changeAnnotations = Nothing
                                        }
    let codeAction = MkCodeAction { title       = "QuickFix: remove impossible keyword"
                                  , kind        = Just QuickFix
                                  , diagnostics = Just [diagnostic]
                                  , isPreferred = Just True
                                  , disabled    = Nothing
                                  , edit        = Just workspaceEdit
                                  , command     = Nothing
                                  , data_       = Nothing
                                  }
    modify LSPConf (record { quickfixes $= (codeAction ::) })
findQuickfix caps uri err@(NotCovering fc n (MissingCases cs)) = do
  whenJust (isNonEmptyFC fc) $ \fc => do
    cases <- addMissingCases !(prettyName n) cs 1
    let startline = startLine fc
    let endline = endLine fc
    src <- forget . String.lines <$> getSource
    let blank = findBlankLine (drop (integerToNat (cast endline)) src) endline
    diagnostic <- toDiagnostic caps uri err
    let range = MkRange (MkPosition blank 0) (MkPosition blank 0)
    let textEdit = MkTextEdit range cases
    let workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri [textEdit])
                                        , documentChanges   = Nothing
                                        , changeAnnotations = Nothing
                                        }
    let missingCodeAction = MkCodeAction { title       = "QuickFix: add missing cases"
                                         , kind        = Just QuickFix
                                         , diagnostics = Just [diagnostic]
                                         , isPreferred = Just True
                                         , disabled    = Nothing
                                         , edit        = Just workspaceEdit
                                         , command     = Nothing
                                         , data_       = Nothing
                                         }
    let range = MkRange (MkPosition startline 0) (MkPosition startline 0)
    let textEdit = MkTextEdit range "partial\n"
    let workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri [textEdit])
                                        , documentChanges   = Nothing
                                        , changeAnnotations = Nothing
                                        }
    let partialCodeAction = MkCodeAction { title       = "QuickFix: add partial annotation"
                                         , kind        = Just QuickFix
                                         , diagnostics = Just [diagnostic]
                                         , isPreferred = Just False
                                         , disabled    = Nothing
                                         , edit        = Just workspaceEdit
                                         , command     = Nothing
                                         , data_       = Nothing
                                         }
    modify LSPConf (record { quickfixes $= (\qf => missingCodeAction :: partialCodeAction :: qf) })
findQuickfix caps uri err@(NotCovering fc n _) = do
  whenJust (isNonEmptyFC fc) $ \fc => do
    diagnostic <- toDiagnostic caps uri err
    let startline = startLine fc
    let range = MkRange (MkPosition startline 0) (MkPosition startline 0)
    let textEdit = MkTextEdit range "partial\n"
    let workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri [textEdit])
                                        , documentChanges   = Nothing
                                        , changeAnnotations = Nothing
                                        }
    let codeAction = MkCodeAction { title       = "QuickFix: add partial annotation"
                                  , kind        = Just QuickFix
                                  , diagnostics = Just [diagnostic]
                                  , isPreferred = Just False
                                  , disabled    = Nothing
                                  , edit        = Just workspaceEdit
                                  , command     = Nothing
                                  , data_       = Nothing
                                  }
    modify LSPConf (record { quickfixes $= (codeAction ::) })
findQuickfix caps uri err@(NotTotal fc n _) = do
  whenJust (isNonEmptyFC fc) $ \fc => do
    diagnostic <- toDiagnostic caps uri err
    let startline = startLine fc
    let range = MkRange (MkPosition startline 0) (MkPosition startline 0)
    let textEdit = MkTextEdit range "covering\n"
    let workspaceEdit = MkWorkspaceEdit { changes           = Just (singleton uri [textEdit])
                                        , documentChanges   = Nothing
                                        , changeAnnotations = Nothing
                                        }
    let codeAction = MkCodeAction { title       = "QuickFix: add covering annotation"
                                  , kind        = Just QuickFix
                                  , diagnostics = Just [diagnostic]
                                  , isPreferred = Just False
                                  , disabled    = Nothing
                                  , edit        = Just workspaceEdit
                                  , command     = Nothing
                                  , data_       = Nothing
                                  }
    modify LSPConf (record { quickfixes $= (codeAction ::) })
findQuickfix _ _ _ = pure ()


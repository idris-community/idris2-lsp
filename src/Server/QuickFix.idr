module Server.QuickFix

import Core.Context
import Core.Core
import Core.Env
import Core.FC
import Core.Metadata
import Data.List
import Data.String
import Idris.REPL.Opts
import Idris.Syntax
import Language.LSP.Message
import Language.JSON
import Server.Configuration
import Server.Diagnostics
import Server.Utils
import Server.Log

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
    let line = unwords $ f :: args ++ ["=", "\{f}_rhs_not_impossible"]
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
-- findQuickfix caps uri (NotCovering fc x y) = ?findQuickfix_rhs_12
-- findQuickfix caps uri (NotTotal fc x y) = ?findQuickfix_rhs_13
-- findQuickfix caps uri (AmbiguousName fc xs) = ?findQuickfix_rhs_18
-- findQuickfix caps uri (AmbiguousElab fc x xs) = ?findQuickfix_rhs_19
-- findQuickfix caps uri (MaybeMisspelling x xs) = ?findQuickfix_rhs_66
findQuickfix _ _ _ = pure ()


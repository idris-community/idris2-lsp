module Language.LSP.CodeAction.Intro

import Core.Context
import Core.Core
import Core.Metadata
import Core.Name
import Core.UnifyState
import Data.String
import Idris.Pretty
import Idris.REPL
import Idris.REPL.Common
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.CodeAction.Utils
import Language.LSP.Message
import Server.Configuration
import Server.Log
import Server.Utils

export
introKind : CodeActionKind
introKind = Other "refactor.rewrite.Intro"

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (introKind `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

buildCodeAction : Name -> URI -> Range -> String -> CodeAction
buildCodeAction name uri range str =
  MkCodeAction
    { title       = "Intro \{strSubstr 0 50 str} over hole \{show $ dropAllNS name}"
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
intro : Ref LSPConf LSPConfiguration
     => Ref MD Metadata
     => Ref Ctxt Defs
     => Ref UST UState
     => Ref Syn SyntaxInfo
     => Ref ROpts REPLOpts
     => CodeActionParams -> Core (List CodeAction)
intro params = do
  let True = isAllowed params
    | False => logI Intro "Skipped" >> pure []
  logI Intro "Checking for \{show params.textDocument.uri} at \{show params.range}"
  withSingleLine Intro params (pure []) $ \line => do
    nameLocs <- gets MD nameLocMap
    let col = params.range.start.character
    let Just (loc@(_, nstart, nend), holename) = findPointInTreeLoc (line, col) nameLocs
      | Nothing => logD Intro "No name found at \{show line}:\{show col}}" >> pure []
    logI Intro "Found name \{show holename}"
    Right (Edited (MadeIntro is)) <- catch (Right <$> process (Editing (Intro False line holename)))
                                           (pure . Left)
      | Right (Edited (EditError err)) => logI Intro "Error while introing \{show holename}: \{!(render err)}" >> pure []
      | Left err => logW Intro "Unexpected error while refining \{show holename}: \{show err}" >> pure []
      | _ => logW Intro "Unexpected error while refining \{show holename}" >> pure []
    let range = MkRange (uncurry MkPosition nstart) (uncurry MkPosition nend)
    let actions = map (buildCodeAction holename params.textDocument.uri range) (forget is)
    pure actions

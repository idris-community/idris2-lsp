module Language.LSP.CodeAction.RefineHole

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.String
import Idris.Parser
import Idris.Pretty
import Idris.REPL
import Idris.REPL.Common
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.CodeAction.Utils
import Language.LSP.Message
import Language.LSP.Message.Derive
import Language.Reflection
import Libraries.Data.NameMap
import Parser.Source
import Parser.Rule.Source
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
record RefineHoleParams where
  constructor MkRefineHoleParams
  codeAction : CodeActionParams
  hint : String

%runElab deriveJSON defaultOpts `{RefineHoleParams}

export
refineHoleKind : CodeActionKind
refineHoleKind = Other "refactor.rewrite.RefineHole"

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (refineHoleKind `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

buildCodeAction : Name -> URI -> Range -> String -> CodeAction
buildCodeAction name uri range str =
  MkCodeAction
    { title       = "Refine hole on \{show $ dropAllNS name}"
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
refineHole : Ref LSPConf LSPConfiguration
          => Ref MD Metadata
          => Ref Ctxt Defs
          => Ref UST UState
          => Ref Syn SyntaxInfo
          => Ref ROpts REPLOpts
          => RefineHoleParams -> Core (List CodeAction)
refineHole params = do
  let True = isAllowed params.codeAction
    | False => logI RefineHole "Skipped" >> pure []
  logI RefineHole "Checking for \{show params.codeAction.textDocument.uri} at \{show params.codeAction.range}" 
  withSingleLine RefineHole params.codeAction (pure []) $ \line => do
    nameLocs <- gets MD nameLocMap 
    let col = params.codeAction.range.start.character
    let Just (loc@(_, nstart, nend), holename) = findPointInTreeLoc (line, col) nameLocs
      | Nothing => logD RefineHole "No name found at \{show line}:\{show col}}" >> pure []
    logI RefineHole "Found name \{show holename}"
    let Right (_, _, hinttm) = runParser (Virtual Interactive) Nothing params.hint aPTerm
      | _ => logD RefineHole "Suggested expression is not parsable" >> pure []
    Right (Edited (DisplayEdit res)) <- catch (Right <$> process (Editing (Refine False line holename hinttm)))
                                              (pure . Left)
      | Right (Edited (EditError err)) => logI RefineHole "Error while refining \{show holename}: \{!(render err)}" >> pure []
      | Left err => logW RefineHole "Unexpected error while refining \{show holename}: \{show err}" >> pure []
      | _ => logW RefineHole "Unexpected error while refining \{show holename}" >> pure []
    let range = MkRange (uncurry MkPosition nstart) (uncurry MkPosition nend)
    action <- buildCodeAction holename params.codeAction.textDocument.uri range <$> render res
    pure (singleton action)

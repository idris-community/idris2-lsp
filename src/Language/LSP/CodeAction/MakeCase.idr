module Language.LSP.CodeAction.MakeCase

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.String
import Idris.IDEMode.CaseSplit
import Idris.IDEMode.MakeClause
import Idris.REPL.Opts
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.CodeAction.Utils
import Language.LSP.Message
import Language.LSP.Utils
import Libraries.Data.PosMap
import Parser.Unlit
import Server.Configuration
import Server.Log
import Server.Utils
import TTImp.Interactive.CaseSplit

currentLineRange : Int -> String -> Range
currentLineRange i origLine = MkRange (MkPosition i 0) (MkPosition i (cast $ length origLine))

isHole : Defs -> Name -> Core Bool
isHole defs n = do
  Just def <- lookupCtxtExact n (gamma defs)
    | Nothing => pure False
  pure $ case definition def of
    Hole {} => True
    _ => False

export
makeCaseKind : CodeActionKind
makeCaseKind = Other "refactor.rewrite.MakeCase"

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (makeCaseKind `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

buildCodeAction : URI -> Name -> TextEdit -> CodeAction
buildCodeAction uri name edit =
  MkCodeAction
    { title       = "Make case for hole ?\{show name}"
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
makeCase : Ref LSPConf LSPConfiguration
        => Ref MD Metadata
        => Ref Ctxt Defs
        => Ref UST UState
        => Ref Syn SyntaxInfo
        => Ref ROpts REPLOpts
        => CodeActionParams -> Core (Maybe CodeAction)
makeCase params = do
  let True = isAllowed params
    | False => logI MakeCase "Skipped" >> pure Nothing
  logI MakeCase "Checking for \{show params.textDocument.uri} at \{show params.range}"

  withSingleLine MakeCase params (pure Nothing) $ \line => do
    withSingleCache MakeCase params MakeCase $ do
      nameLocs <- gets MD nameLocMap
      let col = params.range.start.character
      let Just (loc, name) = findPointInTreeLoc (line, col) nameLocs
        | Nothing => logD MakeCase "No name found at \{show line}:\{show col}}" >> pure Nothing
      logD MakeCase "Found name \{show name}"

      defs <- get Ctxt
      True <- isHole defs name -- should only work on holes
        | _ => logD MakeCase "\{show name} is not a metavariable" >> pure Nothing

      toBrack <- gets Syn (elemBy ((==) `on` dropNS) name . bracketholes)
      Just original <- getSourceLine (line + 1)
        | Nothing => logE MakeCase "Error while fetching the referenced line" >> pure Nothing
      litStyle <- getLitStyle
      let Right l = unlit litStyle original
         | Left err => logE MakeCase "Invalid literate Idris" >> pure Nothing
      let (markM, _) = isLitLine original
      let name' = dropAllNS name
      let caseText = makeCase toBrack (dropAllNS name') original
      let newText = maybe caseText (const (unlines $ map (relit markM) $ lines caseText)) markM
      let edit = MkTextEdit (currentLineRange line original) newText

      pure $ Just (cast loc, buildCodeAction params.textDocument.uri name' edit)

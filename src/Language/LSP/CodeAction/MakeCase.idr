module Language.LSP.CodeAction.MakeCase

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.List
import Data.List1
import Data.String
import Idris.IDEMode.CaseSplit
import Idris.IDEMode.MakeClause
import Idris.REPL.Opts
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.Message
import Libraries.Data.List.Extra
import Libraries.Data.PosMap
import Server.Configuration
import Server.Response
import Server.Utils
import Server.Log
import System.File
import TTImp.Interactive.CaseSplit
import TTImp.TTImp

currentLineRange : Int -> String -> Range
currentLineRange i origLine =
  MkRange (MkPosition i 0)
          (MkPosition i (cast $ Prelude.String.length origLine))

export
handleMakeCase : Ref LSPConf LSPConfiguration
         => Ref MD Metadata
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref ROpts REPLOpts
         => CodeActionParams -> Core (Maybe CodeAction)
handleMakeCase params = do
  let True = params.range.start.line == params.range.end.line
    | _ => pure Nothing

  [] <- searchCache params.range MakeCase
    | action :: _ => do logString Debug "makeCase: found cached action"
                        pure (Just action)

  meta <- get MD
  let line = params.range.start.line
  let col = params.range.start.character
  let Just (loc, name) = findPointInTreeLoc (line, col) (nameLocMap meta)
    | Nothing =>
        do logString Debug $
             "makeCase: couldn't find name in tree for position (\{show line},\{show col})"
           pure Nothing

  defs <- get Ctxt
  let True = !(isHole defs name) -- should only work on holes
    | _ => do logString Debug $ "makeCase: \(show name) is not a hole"
              pure Nothing

  Just original <- getSourceLine (line + 1)
    | Nothing => do logString Debug $ "caseSplit: error while fetching the referenced line"
                    pure Nothing
  let name' = dropAllNS name
  let newText = makeCase False name' original -- TODO sort out brackets arg
  let rng = currentLineRange line original
  let edit = MkTextEdit rng newText

  let docURI = params.textDocument.uri
  let workspaceEdit = MkWorkspaceEdit
        { changes           = Just (singleton docURI [edit])
        , documentChanges   = Nothing
        , changeAnnotations = Nothing
        }
  let codeAction = MkCodeAction
        { title       = "Make case for hole ?\{show name'}"
        , kind        = Just RefactorRewrite
        , diagnostics = Just []
        , isPreferred = Just False -- not a quickfix
        , disabled    = Nothing
        , edit        = Just workspaceEdit
        , command     = Nothing
        , data_       = Nothing
        }
  modify LSPConf (record { cachedActions $= insert (cast loc, MakeCase, [codeAction]) })
  pure $ Just codeAction
  where
    isHole : Defs -> Name -> Core Bool
    isHole defs n
        = do Just def <- lookupCtxtExact n (gamma defs)
                  | Nothing => do pure False
             case definition def of
                  Hole _ _ => pure True
                  _ => pure False

module Language.LSP.CodeAction.CaseSplit

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.List1
import Data.String
import Idris.IDEMode.CaseSplit
import Idris.REPL.Opts
import Idris.Syntax
import Idris.Syntax
import Language.JSON
import Language.LSP.Message
import Libraries.Data.List.Extra
import Libraries.Data.PosMap
import Server.Configuration
import Server.Response
import Server.Utils
import System.File
import TTImp.Interactive.CaseSplit
import TTImp.TTImp

-- fork of updateCase in Idris.IDEMode.CaseSplit.updateCase
originalLine : Ref Ctxt Defs
            => Ref Syn SyntaxInfo
            => Ref ROpts REPLOpts
            => List ClauseUpdate -> Int -> Int
            -> Core String
originalLine splits line col = do
  Just f <- gets ROpts mainfile
    | Nothing => throw (InternalError "No file loaded")
  Right file <- coreLift $ readFile f
    | Left err => throw (FileErr f err)
  let Just l = elemAt (forget $ lines file) (integerToNat (cast line))
    | Nothing => throw (InternalError "File too short!")
  pure l

export
caseSplit : Ref LSPConf LSPConfiguration
         => Ref MD Metadata
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref ROpts REPLOpts
         => OneOf [Int, String, Null] -> CodeActionParams -> Core (Maybe CodeAction)
caseSplit msgId params = do
  let True = params.range.start.line == params.range.end.line
      | _ => pure Nothing

  meta <- get MD
  let line = params.range.start.line
  let col = params.range.start.character
  let Just name = findInTree (line, col) (nameLocMap meta)
    | Nothing => pure Nothing

  let find = if col > 0
                then within (line, col)
                else onLine line

  OK splits <- getSplits (anyAt find) name
     | SplitFail err => pure Nothing

  lines <- updateCase splits line col
  original <- originalLine splits line col

  let docURI = params.textDocument.uri
  let rng = MkRange (MkPosition line 0)
                    (MkPosition line (cast (length original)))
  let edit = MkTextEdit rng (unlines lines)
  let workspaceEdit = MkWorkspaceEdit
        { changes           = Just (singleton docURI [edit])
        , documentChanges   = Nothing
        , changeAnnotations = Nothing
        }
  pure $ Just $ MkCodeAction
    { title       = "Case split on \{show name}"
    , kind        = Just $ Other "case-split"
    , diagnostics = Just []
    , isPreferred = Just False
    , disabled    = Nothing
    , edit        = Just workspaceEdit
    , command     = Nothing
    , data_       = Nothing
    }

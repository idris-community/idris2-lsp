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
originalLine
  : {auto c : Ref Ctxt Defs} ->
    {auto s : Ref Syn SyntaxInfo} ->
    {auto o : Ref ROpts REPLOpts} ->
    List ClauseUpdate -> Int -> Int ->
    Core String
originalLine splits line col
    = do opts <- get ROpts
         case mainfile opts of
              Nothing => throw (InternalError "No file loaded")
              Just f =>
                do Right file <- coreLift $ readFile f
                       | Left err => throw (FileErr f err)
                   let thisline = elemAt (forget $ lines file) (integerToNat (cast line))
                   case thisline of
                        Nothing => throw (InternalError "File too short!")
                        Just l => pure l
  where
    getValid : ClauseUpdate -> Maybe (List (Name, RawImp))
    getValid (Valid _ ups) = Just ups
    getValid _ = Nothing

    getBad : ClauseUpdate -> Maybe RawImp
    getBad (Impossible lhs) = Just lhs
    getBad _ = Nothing

export
caseSplit : Ref LSPConf LSPConfiguration
         => Ref MD Metadata
         => Ref Ctxt Defs
         => Ref UST UState
         => Ref Syn SyntaxInfo
         => Ref ROpts REPLOpts
         => Int .+. String .+. Null -> CodeActionParams -> Core ()
caseSplit msgId params = do
  let True = params.range.start.line == params.range.end.line
      | _ =>
        sendResponseMessage
          TextDocumentCodeAction
          (Failure msgId (MkResponseError InvalidParams "Multiple lines were selected." JNull))

  meta <- get MD
  let line = params.range.start.line
  let col = params.range.start.character
  let Just name = findInTree (line, col) (nameLocMap meta)
      | Nothing =>
          sendResponseMessage
            TextDocumentCodeAction
            (Failure msgId (MkResponseError InternalError "Can't find name" JNull))

  let find = if col > 0
                then within (line, col)
                else onLine line

  OK splits <- getSplits (anyAt find) name
     | SplitFail err => do
        sendResponseMessage
          TextDocumentCodeAction
          (Failure msgId (MkResponseError InternalError "Can't split: \{show err}" JNull))

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
  let caseSplitCodeAction = MkCodeAction
        { title       = "Case split on \{show name}"
        , kind        = Just $ Other "case-split"
        , diagnostics = Just []
        , isPreferred = Just False
        , disabled    = Nothing
        , edit        = Just workspaceEdit
        , command     = Nothing
        , data_       = Nothing
        }
  let response = Success msgId (Left [Right caseSplitCodeAction])
  sendResponseMessage TextDocumentCodeAction response

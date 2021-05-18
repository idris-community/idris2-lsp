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
import Idris.Syntax
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

-- forked from Language.LSP.CodeAction.ExprSearch
fueledTimedRepeat : Ref LSPConf LSPConfiguration
                 => Nat -> Clock Monotonic -> Clock Duration -> List a -> (Nat -> Core (Maybe a)) -> Core (List a)
fueledTimedRepeat Z _ _ acc _ = pure (reverse acc)
fueledTimedRepeat (S k) start timeout acc f = do
  Just res <- f k
    | Nothing => pure (reverse acc)
  time <- coreLift $ clockTime Monotonic
  let diff = timeDifference time start
  if diff < timeout
     then fueledTimedRepeat k start timeout (res :: acc) f
     else do logString Info "GeneratedDef timed out"
             pure (reverse acc)

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
  Just name <- guessName (line, col)
    | Nothing => do
        logString Debug "generateDef: couldn't find name at: \{show (line, col)}"
        pure []

  timeout <- gets LSPConf longActionTimeout
  start <- coreLift $ clockTime Monotonic

  Edited (DisplayEdit block)
    <- Idris.REPL.process
        (Editing (GenerateDef False (fromInteger (cast (line + 1))) name 0)) -- off-by-one
    | Edited (EditError err) => do
        logString Debug "generateDef: had an EditError \{show err}"
        pure []
    | other => do
        logString Debug "generateDef: return unexpected REPL result."
        pure []

  time <- coreLift $ clockTime Monotonic
  let diff = timeDifference time start
  blocks <- case diff < timeout of
    False => do logString Info "GeneratedDef timed out"
                pure []
    True => do
      fuel <- gets LSPConf searchLimit
      fueledTimedRepeat (pred fuel) start timeout [] $ \k => do
        Edited (DisplayEdit block) <- Idris.REPL.process (Editing GenerateDefNext)
          | Edited (EditError err) => do
              logString Debug "generateDef next: had an EditError in \{show k}: \{show err}"
              pure Nothing
          | other => do
              logString Debug "generateDef next: returned unexpected REPL result in \{show k}"
              pure Nothing
        pure (Just block)

  put Ctxt defs

  let docURI = params.textDocument.uri
  let rng = MkRange (MkPosition (line + 1) 0) (MkPosition (line + 1) 0) -- insert

  actions <- for (number 1 (block :: blocks)) $ \(i,b) => do
    let funcDef = show b
    let lastLine = last (String.lines funcDef)
    let edit = MkTextEdit rng (funcDef ++ "\n") -- extra newline
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

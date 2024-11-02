module Language.LSP.CodeAction.GenerateDefNext

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.UnifyState
import Data.String
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.CodeAction.Utils
import Language.LSP.Message
import Language.LSP.Utils
import Parser.Unlit
import Server.Configuration
import Server.Log
import Server.Utils
import Libraries.Data.Tap
import TTImp.Interactive.GenerateDef
import TTImp.Interactive.ExprSearch
import TTImp.TTImp
import TTImp.TTImp.Functor

generateDefNextKind : CodeActionKind
generateDefNextKind = Other "refactor.rewrite.GenerateDefNext"

isAllowed : CodeActionParams -> Bool
isAllowed params =
  maybe True (\filter => (generateDefNextKind `elem` filter) || (RefactorRewrite `elem` filter)) params.context.only

buildCodeAction : URI -> TextEdit -> CodeAction
buildCodeAction uri edit =
  MkCodeAction
    { title       = "Generate next definition"
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

-- first blank line going forward (in contrast to reversed implementation found in
-- some other code actions.
findBlankLine : List String -> Int -> Int
findBlankLine [] acc = acc
findBlankLine (x :: xs) acc = if trim x == "" then acc else findBlankLine xs (acc + 1)

-- reproduced from compiler repo because it is not exported there (as of now)
nextGenDef : {auto c : Ref Ctxt Defs} ->
             {auto u : Ref UST UState} ->
             {auto o : Ref ROpts REPLOpts} ->
             (reject : Nat) ->
             Core (Maybe (Int, (FC, List ImpClause)))
nextGenDef reject
    = do opts <- get ROpts
         let Just (line, res) = gdResult opts
              | Nothing => pure Nothing
         Just (res, next) <- nextResult res
              | Nothing =>
                    do put ROpts ({ gdResult := Nothing } opts)
                       pure Nothing
         put ROpts ({ gdResult := Just (line, next) } opts)
         case reject of
              Z => pure (Just (line, res))
              S k => nextGenDef k

export
generateDefNext : Ref LSPConf LSPConfiguration
               => Ref MD Metadata
               => Ref Ctxt Defs
               => Ref UST UState
               => Ref Syn SyntaxInfo
               => Ref ROpts REPLOpts
               => CodeActionParams -> Core (List CodeAction)
generateDefNext params = do
  let True = isAllowed params
    | False => logI GenerateDefNext "Skipped" >> pure []
  logI GenerateDefNext "Checking for \{show params.textDocument.uri} at \{show params.range}"

  withSingleLine GenerateDefNext params (pure []) $ \line => do
    withMultipleCache GenerateDefNext params GenerateDefNext $ do
      defs <- branch
      Just (loc, n, _, _) <- findTyDeclAt (\p, n => onLine line p)
        | Nothing => logD GenerateDef "No name found at line \{show line}" >> pure []
      logD CaseSplit "Found type declaration \{show n}"

      previousResults <- gdResult <$> get ROpts
      let staleDefs = case previousResults of
                           Nothing => True
                           Just (l, _) => l /= line
      when staleDefs $ do
        fuel <- gets LSPConf searchLimit
        existingDef <- lookupDefExact n defs.gamma
        case existingDef of
          Just None => do
             catch (do searchdefs <- makeDefSort (\p, n => onLine line p) fuel mostUsed n
                       update ROpts { gdResult := Just (line, pure searchdefs) }
                       pure ())
                   (\case Timeout _ => logI GenerateDefNext "Timed out" >> pure ()
                          err => logC GenerateDefNext "Unexpected error while searching" >> throw err)
          Just _ => logD GenerateDefNext "There is already a definition for \{show n}" >> pure ()
          Nothing => logD GenerateDefNext "Couldn't find type declaration at line \{show line}" >> pure ()

      Just (line', (fc, cs)) <- nextGenDef 0
        | Nothing => logD GenerateDefNext "No more results" >> pure []
      let l : Nat = integerToNat $ cast $ startCol (toNonEmptyFC fc)
      Just srcLine <- getSourceLine line'
        | Nothing => logE GenerateDefNext "Source line \{show line} not found" >> pure []
      let (markM, srcLineUnlit) = isLitLine srcLine
      lines <- traverse (printClause markM l) cs

      put Ctxt defs

      let newLine = endLine loc + 1

      -- Not having an easy time figuring out how to determine how many
      -- following lines should be replaced if there's a definition there
      -- already (cycling defs and not on first one). probably just use
      -- whitespace.
      defToOverride <- lookupDefExact n defs.gamma
      rng <- case defToOverride of
           Nothing => do
             logD GenerateDefNext "No def to override, inserting new def" 
             pure $ MkRange (MkPosition newLine 0) (MkPosition newLine 0) -- insert
           (Just None) => do
             logD GenerateDefNext "No def to override, inserting new def" 
             pure $ MkRange (MkPosition newLine 0) (MkPosition newLine 0) -- insert
           (Just (PMDef pminfo args treeCT treeRT pats)) => do
             src <- String.lines <$> getSource
             let srcFromDef = drop (integerToNat (cast line)) src
             let blank = findBlankLine srcFromDef line
             pure $ MkRange (MkPosition newLine 0) (MkPosition blank 0) -- replace
           (Just _) => do
             logE GenerateDefNext "UNEXPECTED"
             pure $ MkRange (MkPosition newLine 0) (MkPosition newLine 0) -- insert

      let docURI = params.textDocument.uri

      action <- do
        let edit = MkTextEdit rng (String.unlines lines)
        pure $ buildCodeAction docURI edit
      -- TODO: retrieve the line length efficiently
      pure [(MkRange (MkPosition line 0) (MkPosition line 1000), [action])]

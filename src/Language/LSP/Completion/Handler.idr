module Language.LSP.Completion.Handler

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.Name
import Data.List
import Data.Maybe
import Data.SortedMap
import Data.String
import Idris.Doc.String
import Idris.Pretty.Annotations
import Idris.REPL.Opts
import Idris.Syntax
import Language.JSON.Data
import Language.LSP.Completion.Info
import Language.LSP.Message
import Language.LSP.Utils
import Language.LSP.VirtualDocument
import Libraries.Data.NameMap
import Libraries.Data.UserNameMap
import Libraries.Data.WithDefault
import Server.Configuration
import Server.Log
import Server.Utils

%default total

-- When a file is opened the context is filled with definitions.
--
-- Cache names from opened files.
-- Update cache on saving a file.
-- Remove cache on closing a file.
--
-- TODO: Don't suggest on comment lines.

config : Config
config = MkConfig
  {showType     = False}
  {longNames    = True}
  {dropFirst    = False}
  {getTotality  = True}

safeTail : List a -> List a
safeTail []        = []
safeTail (x :: xs) = xs

number : List a -> List (Nat, a)
number = go 0 []
  where
    go : Nat -> List (Nat, a) -> List a -> List (Nat, a)
    go n ys []        = reverse ys
    go n ys (x :: xs) = go (S n) ((n,x) :: ys) xs

argument : Name -> Maybe String
argument n =
  let sn : Name := dropNS n in
  if (isUserName sn)
    then Just $ show sn
    else Nothing

arguments : Term vars -> List (Maybe String)
arguments (Bind fc n (Pi   fc1 rig Explicit ty) scope) = argument n :: arguments scope
arguments (Bind fc n (Lam  fc1 rig Explicit ty) scope) = argument n :: arguments scope
arguments (Bind fc n (PVar fc1 rig Explicit ty) scope) = argument n :: arguments scope
arguments (Bind fc x (Lam  fc1 rig pinfo    ty) scope) = arguments scope
arguments (Bind fc x (Let  fc1 rig val      ty) scope) = arguments scope
arguments (Bind fc x (Pi   fc1 rig pinfo    ty) scope) = arguments scope
arguments (Bind fc x (PVar fc1 rig pinfo    ty) scope) = arguments scope
arguments (Bind fc x (PLet fc1 rig val      ty) scope) = arguments scope
arguments (Bind fc x (PVTy fc1 rig          ty) scope) = arguments scope
arguments (As       fc side as pat)                    = arguments pat
arguments (TDelayed fc lz t)                           = arguments t
arguments (TDelay   fc lz ty arg)                      = arguments arg
arguments (TForce   fc lz t)                           = arguments t
arguments (App fc fn arg)                              = safeTail (arguments fn)
arguments (Local fc isLet idx p)                       = []
arguments (Ref fc nt name)                             = []
arguments (Meta fc n i ts)                             = []
arguments (PrimVal fc c)                               = []
arguments (Erased fc why)                              = []
arguments (TType fc n)                                 = []

-- PERF: This is currently *very* slow. It can take up 5-10 seconds for when running this on this (idris2-lsp) project
export
covering
completionNames : Ref Ctxt Defs
               => Ref LSPConf LSPConfiguration
               => Ref Syn SyntaxInfo
               => Ref ROpts REPLOpts
               => Core (SortedMap NameCategory (List Entry))
completionNames = do
  defs <- get Ctxt
  let ctxt = gamma defs
  let namespaces = sort $ nub (currentNS defs :: nestedNS defs ++ map (snd . snd) (imported defs))
  logD Completion "Found namespaces: \{unwords (map show namespaces)}"
  let categorizeName
        : (Namespace, UserName, Name) -> NameCategory
        := \(ns,un,n) =>
            if isJust (isField un)
              then FieldName
              else if currentNS defs == ns
                then CurrentNsName
                else if elem ns (nestedNS defs)
                  then InNestedNs
                  else OtherName
  let inImportedNamespaces = mapMaybe (accessibleName namespaces) $ keys $ resolvedAs ctxt
  visibleExportedNames <- traverse
    (\(ns, un, n) => do
        mdef <- lookupCtxtExact n ctxt
        case mdef of
          Nothing => pure Nothing
          Just def =>
            let visible = (ns == currentNS defs) || (collapseDefault (visibility def) /= Private) in
            if visible
              then do
                ty <- prettyType (const ()) def.type
                doc <- getDocsForName def.location def.fullname config
                pure $ Just $
                  MkEntry
                    { category      = categorizeName (ns, un, n)
                    , fullname      = def.fullname
                    , type          = show ty
                    , arguments     = arguments def.type
                    , documentation = show doc
                    }
              else pure Nothing)
    inImportedNamespaces
  let completionEntries = catMaybes visibleExportedNames
  logD Completion "Found \{show (length completionEntries)} completion entries."
  pure $ foldl (\m , c => mergeWith (++) m (singleton c.category [c])) empty completionEntries
  where
    accessibleName : List Namespace -> Name -> Maybe (Namespace, UserName, Name)
    accessibleName nps n = do
      (ns, un) <- isUN n
      if any (\p => isParentOf p ns || p == ns) nps
        then Just (ns, un, n)
        else Nothing

mkCompletionItem : Bool -> String -> Entry -> Maybe CompletionItem
mkCompletionItem isBrief w (MkEntry c n ty ar doc) =
  let shortName = show $ dropNS n in
  let longName  = show n in
  let item = MkCompletionItem
        { label               = ""
        , kind                = Nothing -- : Maybe CompletionItemKind
        , tags                = Nothing -- : Maybe (List CompletionItemTag)
        , detail              = Just ty
        , documentation       = Just $ make doc
        , deprecated          = Nothing -- : Maybe Bool
        , preselect           = Nothing -- : Maybe Bool
        , sortText            = Nothing -- : Maybe String
        , filterText          = Nothing -- : Maybe String
        , insertText          = Nothing -- : Maybe String
        , insertTextFormat    = Nothing -- : Maybe InsertTextFormat
        , insertTextMode      = Nothing -- : Maybe InsertTextMode
        , textEdit            = Nothing -- : Maybe (OneOf [TextEdit, InsertReplaceEdit])
        , additionalTextEdits = Nothing -- : Maybe (List TextEdit)
        , commitCharacters    = Nothing -- : Maybe (List Char)
        , command             = Nothing -- : Maybe Command
        , data_               = Nothing -- : Maybe JSON
        } in
  let text : String = if isBrief then shortName
                      else case ar of
              [] => shortName
              as => fastConcat
                      [ "("
                      , unwords
                          $ shortName ::
                            [ "?" ++ fromMaybe (shortName ++ "_arg_" ++ show i) x
                            | (i,x) <- number as
                            ]
                      , ")"
                      ]
  in if (isInfixOf w shortName || isInfixOf w longName)
    then Just $ case c of
          CurrentNsName =>
            { label      := shortName
            , insertText := Just text
            } item
          InNestedNs =>
            { label      := shortName
            , insertText := Just text
            } item
          other =>
            { label      := show n
            , insertText := Just text
            } item
      else Nothing

||| Gets the identifier at the position of cursor.
|||
||| If the document URI is not registered in the cache it returns Nothing.
identifierAtCursor : Ref LSPConf LSPConfiguration
                   => CompletionParams
                   -> Core (Maybe String)
identifierAtCursor params = do
  Just content <- gets LSPConf (map snd . lookup params.textDocument.uri . virtualDocuments)
    | Nothing => do
        logD Completion "No virtual content is found for identifierAtCursor"
        pure Nothing
  let identifier = identifierAtPosition params.position content
  logD Completion "Identifier at position \{show params.position} \{identifier}"
  pure $ Just identifier

||| Generating the completion response.
|||
||| The possible candidates are collected when saving the file, and the list could
||| be enormous. Filtering it based on the identifier at the cursor position
||| is necessary; otherwise, the generated JSON clogs up the channel and slows
||| the response generation, making the developer experience terrible.
export
completion : Ref Ctxt Defs
          => Ref MD Metadata
          => Ref LSPConf LSPConfiguration
          => CompletionParams
          -> Core (Maybe CompletionList)
completion params = do
  logI Completion "Completion for \{show params.textDocument.uri}"
  cache <- gets LSPConf completionCache
  isBrief <- gets LSPConf briefCompletions
  let Just completions = lookup params.textDocument.uri cache
      | Nothing => do
          logE Completion "No completions found for \{show params.textDocument.uri}"
          pure Nothing
  Just identifier <- identifierAtCursor params
    | Nothing => do
        pure Nothing
  let nameCategories = if strLength identifier < 3
        then [CurrentNsName, InNestedNs]
        else [CurrentNsName, InNestedNs, OtherName, FieldName]
  let candidates = concat $ mapMaybe (flip lookup completions) nameCategories
  let suggestions@(_ :: _) = List.mapMaybe (mkCompletionItem isBrief identifier) candidates
      | [] => do
          logW Completion "No suggestion found for \{show params.textDocument.uri} \{show params.position}"
          pure Nothing
  logD Completion "Found \{show (length suggestions)} suggestions."
  pure $ Just $ MkCompletionList
    { isIncomplete = True
    , items = suggestions
    }

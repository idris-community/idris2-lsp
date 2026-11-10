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
import Server.Input
import Server.Utils

%default total

-- Build the cache while idle, interrupting when input arrives
-- Invalidate cache on saving a file
-- Remove cache on closing a file
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

||| schedules a completion documnentation building retry
||| reads the clock and sets the completionRetry to 500ms
export
scheduleCompletions : Ref LSPConf LSPConfiguration => Core ()
scheduleCompletions = do
  now <- monotonicNanos
  update LSPConf ({ completionRetry := Just (now + 500000000) })

||| checks if the completion cache building is interrupted by input
completionInterrupted : Ref LSPConf LSPConfiguration => Core Bool
completionInterrupted = do
  Just inbox <- gets LSPConf inputQueue
    | Nothing => pure False
  coreLift $ hasInput inbox

||| traverses a list, returns Nothing if incoming input interrupts the work
collect : Ref LSPConf LSPConfiguration
       => (a -> Core b) -> List a -> Core (Maybe (List b))
collect f [] = pure $ Just []
collect f (x :: xs) = do
  False <- completionInterrupted
    | True => pure Nothing
  value <- f x
  Just values <- collect f xs
    | Nothing => pure Nothing
  pure $ Just (value :: values)

export
covering
completionNames : Ref Ctxt Defs
               => Ref LSPConf LSPConfiguration
               => Ref Syn SyntaxInfo
               => Ref ROpts REPLOpts
               => Core (Maybe (SortedMap NameCategory (List Entry)))
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
  Just visibleExportedNames <- collect
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
    | Nothing => pure Nothing
  False <- completionInterrupted
    | True => pure Nothing
  let completionEntries = catMaybes visibleExportedNames
  logD Completion "Found \{show (length completionEntries)} completion entries."
  pure $ Just $ foldl (\m , c => mergeWith (++) m (singleton c.category [c])) empty completionEntries
  where
    accessibleName : List Namespace -> Name -> Maybe (Namespace, UserName, Name)
    accessibleName nps n = do
      (ns, un) <- isUN n
      if any (\p => isParentOf p ns || p == ns) nps
        then Just (ns, un, n)
        else Nothing

||| builds and publishes the completion cache for a given document URI
||| calls completionNames. if that gets interrupted by input, it returns False,
||| does not update the cache, and schedules a retry
export covering
buildCompletionCache : Ref Ctxt Defs
                    => Ref LSPConf LSPConfiguration
                    => Ref Syn SyntaxInfo
                    => Ref ROpts REPLOpts
                    => DocumentURI -> Core Bool
buildCompletionCache uri = do
  logD Completion "Building completion cache for \{show uri}"
  Just names <- completionNames
    | Nothing => do
        logD Completion "Cancelled completion cache: input is waiting"
        scheduleCompletions
        pure False
  update LSPConf ({ completionCache $= insert uri names, completionRetry := Nothing })
  logD Completion "Published completion cache for \{show uri}"
  pure True

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
||| The possible candidates are cached while idle or on request, and the list could
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

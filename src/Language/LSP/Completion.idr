module Language.LSP.Completion

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Data.List
import Data.String
import Data.String.Parser
import Data.URI
import Idris.DocString
import Idris.Syntax
import Language.JSON.Data
import Language.LSP.Message
import Libraries.Data.NameMap
import Server.Configuration
import Server.Log
import Server.Utils
import System.File

-- NOTE: This is a quick and dirty solution for the compilation, which is based
-- on scavaging some names from the context, which operaton is cheap.
-- This needs a lot of improvements, around the name handling in the Idris
-- compiler itself, as retrieving all the names from a List of Namespaces
-- seems to be a costly operation.

export
completionProvider : CompletionOptions
completionProvider = MkCompletionOptions
  { workDoneProgress    = Nothing
  , triggerCharacters   = Just []
  , allCommitCharacters = Just []
  , resolveProvider     = Nothing
  }

mkCompletionItem : String -> CompletionItem
mkCompletionItem label =
  MkCompletionItem
    { label               = label
    , kind                = Nothing
    , tags                = Nothing
    , detail              = Nothing
    , documentation       = Nothing
    , deprecated          = Nothing
    , preselect           = Nothing
    , sortText            = Nothing
    , filterText          = Nothing
    , insertText          = Nothing
    , insertTextFormat    = Just PlainText
    , insertTextMode      = Nothing
    , textEdit            = Nothing
    , additionalTextEdits = Nothing
    , commitCharacters    = Nothing
    , command             = Nothing
    , data_               = Nothing
    }

simpleName : Name -> Maybe Name
simpleName n = if isUserName n then Just (dropAllNS n) else Nothing

inNS : Namespace -> Name -> Bool
inNS ns (NS xns (UN _)) = ns `isParentOf` xns
inNS _ _ = False

inNamespaces : List Namespace -> Name -> Maybe Name
inNamespaces ns n = if any (`inNS` n) ns then Just n else Nothing

-- TODO: Quickly retrieve all the visible names for the current module.
-- TODO: Cache the retrieved names for the document.
defNames : Ref Ctxt Defs
        => Ref Syn SyntaxInfo
        => Core (List Name)
defNames = do
  defs <- get Ctxt
  let namespaces = defs.currentNS :: defs.nestedNS ++ map (\(_,_,ns) => ns) defs.imported
  -- This is a hack as we need a better way of retrieving visible names of the given
  -- context, name resolution seems to be constly, probably it would be nice to
  -- maintain a namespace Trie for quick lookup purposes.
  -- TODO: Ask Idris devs how to do this, rather than scavange the names from the defs context.
  let allNames = nub
        (  keys defs.toSave
        ++ keys defs.typeHints
        ++ keys defs.autoHints
        ++ keys defs.openHints
        ++ keys defs.localHints
        ++ map fst defs.saveAutoHints
        ++ map fst defs.saveTransforms
        ++ keys defs.transforms
        ++ keys defs.namedirectives
        ++ defs.toCompileCase
        ++ keys defs.toIR
        )
  let allNames2 = concat (map (\(n1, n2, _) => [n1, n2]) (defs.saveTypeHints))
  pure $ nub $ mapMaybe (inNamespaces namespaces >=> simpleName) (allNames ++ allNames2)

export
completion : Ref Ctxt Defs
          => Ref MD Metadata
          => Ref LSPConf LSPConfiguration
          => Ref Syn SyntaxInfo
          => CompletionParams -> Core (Maybe CompletionList)
completion params = do
  -- Check same URI
  Just (actualUri, _) <- gets LSPConf openFile
    | Nothing => do
        logString Debug "completion: openFile returned Nothing"
        pure Nothing
  let True = actualUri == params.textDocument.uri
      | False => do
          logString Debug "completion: different URI than expected \{show (actualUri, params.textDocument.uri)}"
          pure Nothing

  -- Collect all global and local names
  meta <- get MD
  let localNames = mapMaybe (\(_, (n, _, _)) => simpleName n) meta.names
  let localTypes = mapMaybe (\(_, (n, _, _)) => simpleName n) meta.tydecls
  globalNames <- defNames

  -- Render the completion list
  pure $ Just $ MkCompletionList
    { isIncomplete = False
    , items = map (mkCompletionItem . show) (nub (localNames ++ localTypes ++ globalNames))
    }


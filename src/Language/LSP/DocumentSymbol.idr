module Language.LSP.DocumentSymbol

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.Name
import Data.List
import Idris.Doc.String
import Language.LSP.Message
import Libraries.Data.NameMap
import Server.Configuration
import Server.Log
import Server.Utils

-- NOTE: Structured information should be rendereded from the actual Term of the expressions
-- in a file, that would give a better high-level overview. For now this is more like a placeholder
-- rather than a useful functionality.
-- NOTE: Global symbols considered as functions, meta symbols considered as Variables.

||| Check if the name is visible in the local namespaces, if yes return Just its full name, otherwise Nothing.
||| This function meant to be used with GlobalNames.
checkVisibleName : Ref Ctxt Defs => List Namespace -> Name -> Core (Maybe Name)
checkVisibleName namespaces name = do
  fullName <- toFullNames name
  let True = isUserName fullName
    | False => pure Nothing
  let (Just fullNamespace, _) = displayName fullName
    | _ => pure Nothing
  pure $ if elem fullNamespace namespaces then Just fullName else Nothing

||| Check if the name is visible and returns the associated NonEmpty location.
currentNSNameWithLoc : Ref Ctxt Defs => List Namespace -> Name -> Core (Maybe (Name, FilePos, FilePos))
currentNSNameWithLoc namespaces name = do
  Just visibleName <- checkVisibleName namespaces name
    | Nothing => pure Nothing
  defs <- get Ctxt
  Just gdef <- lookupCtxtExact visibleName defs.gamma
    | Nothing => pure Nothing
  pure $ case gdef.location of
    EmptyFC                 => Nothing
    MkFC        _ start end => Just (gdef.fullname, start, end)
    MkVirtualFC _ start end => Just (gdef.fullname, start, end)

buildSymbolInformation : Name -> URI -> FileRange -> SymbolKind -> Bool -> SymbolInformation
buildSymbolInformation n uri range kind deprecated = MkSymbolInformation
  { name = show $ dropNS n
  , kind = kind
  , tags = if deprecated then Just [Deprecated] else Nothing
  , deprecated = Nothing
  , location = MkLocation uri (cast range)
  , containerName = Nothing
  }

||| Render all names form Meta and current namespace matching Global names as SymbolInformation.
||| The LSP client needs to handle the SymbolInformations correctly.
export
documentSymbol : Ref Ctxt Defs
              => Ref MD Metadata
              => Ref LSPConf LSPConfiguration
              => DocumentSymbolParams -> Core (List SymbolInformation)
documentSymbol params = do
  logI DocumentSymbol "Making for \{show params.textDocument.uri}"
  Just (uri, _) <- gets LSPConf openFile
    | Nothing => logE DocumentSymbol "No open file" >> pure []
  let True = uri == params.textDocument.uri
    | False => do
        logD DocumentSymbol "Expected request for the currently opened file \{show uri}, instead received \{show params.textDocument.uri}"
        pure []
  defs <- get Ctxt
  -- Get the current and visible namespaces from the context
  let currentNamespaces = defs.currentNS :: defs.nestedNS
  -- Get all resolved names, which are in the visible namesspaces
  let knownNames = NameMap.keys $ namesResolvedAs defs.gamma
  -- Render global and meta names as SymbolInformation
  visibleNames <- map catMaybes $ traverse (currentNSNameWithLoc currentNamespaces) knownNames
  let globalDocSymbols = map (\(n, range) => buildSymbolInformation n uri range Function False) visibleNames
  meta <- get MD
  let metaDocSymbols = map (\((_, range), (metaName, _, _)) => buildSymbolInformation metaName uri range Variable False) meta.names
  let docSymbols = globalDocSymbols ++ metaDocSymbols
  logI DocumentSymbol "Found \{show (length globalDocSymbols)} global and \{show (length metaDocSymbols)} meta document symbols."
  pure $ docSymbols

module Language.LSP.BrowseNamespace

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.Name
import Data.List
import Idris.Doc.String
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Language.LSP.Definition
import Language.LSP.Message
import Libraries.Data.NameMap
import Libraries.Data.WithDefault
import Parser.Source
import Parser.Rule.Source
import Server.Configuration
import Server.Log
import Server.Utils

visible : Defs -> Name -> Core Bool
visible defs n = do 
  Just def <- lookupCtxtExact n (gamma defs)
    | Nothing => pure False
  pure $ collapseDefault (visibility def) /= Private

inNS : Namespace -> Name -> Bool
inNS ns (NS xns (UN _)) = ns `isParentOf` xns
inNS _ _ = False

getNames : Ref ROpts REPLOpts
        => Ref Ctxt Defs
        => Ref Syn SyntaxInfo
        => Namespace -> Core (List Name)
getNames ns = do
  defs <- get Ctxt
  names <- allNames defs.gamma
  let allNames = filter (inNS ns) names
  allNames <- filterM (visible defs) allNames
  pure $ sort allNames

buildDocumentSymbol : Ref Ctxt Defs
                   => Ref LSPConf LSPConfiguration
                   => Ref Syn SyntaxInfo
                   => Name -> Core (Maybe SymbolInformation)
buildDocumentSymbol n = do
  defs <- get Ctxt
  Just def <- lookupCtxtExact n defs.gamma
    | _ => pure Nothing
  Just loc <- mkLocation def.location
    | _ => pure Nothing
  let isDeprecated = Deprecate `elem` def.flags
  let kind = case def.definition of
                  (PMDef {}) => Function
                  (ExternDef {}) => Function
                  (ForeignDef {}) => Function
                  (Builtin {}) => Function
                  (DCon {}) => EnumMember
                  (TCon {}) => Constructor
                  (Hole {}) => Variable
                  _ => Null
  ty <- resugar ScopeEmpty =<< normaliseHoles defs ScopeEmpty def.type
  pure $ Just $ MkSymbolInformation
    { name = "\{show $ dropNS n} : \{show ty}"
    , kind = kind
    , tags = if isDeprecated then Just [Deprecated] else Nothing
    , deprecated = Nothing
    , location = loc
    , containerName = Nothing
    }

||| Returns the list of functions visible in the given namespace.
||| The response in the same format as a textDocument/documentSymbol request.
export
browseNamespaceCmd : Ref Ctxt Defs
                  => Ref Syn SyntaxInfo
                  => Ref ROpts REPLOpts
                  => Ref LSPConf LSPConfiguration
                  => String -> Core (List SymbolInformation)
browseNamespaceCmd str = do
  let Right (_, _, ns) = runParser (Virtual Interactive) Nothing str namespaceId
    | _ => pure []
  logI Browse "Browsing namespace \{show ns}"
  names <- getNames ns
  logI Browse "Names in \{show ns} fetched, found \{show $ length names}"
  catMaybes <$> traverse buildDocumentSymbol names

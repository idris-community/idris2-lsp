module Language.LSP.Definition

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Data.List
import Data.String
import Data.String.Parser
import Data.URI
import Language.LSP.Message
import Server.Configuration
import Server.Log
import Server.Utils
import System.File

mkLocation : Ref Ctxt Defs
          => Ref LSPConf LSPConfiguration
          => String -> (Int, Int) -> (Int, Int) -> Core (Maybe Location)
mkLocation fname (sline, scol) (eline, ecol) = do
  Just (Just (Here path)) <- map rootPath <$> gets LSPConf initialized
    | _ => do
        logString Debug "gotoDefinition: Root path was not set."
        pure Nothing

  -- URIs can refer relative or absolute paths, also they can refer modules
  -- out of the current project.
  mSourceStr <-
    if path `isPrefixOf` fname
       then pure $ Just $ "file://" ++ fname
       else do
         let concatedName = path ++ "/" ++ fname
         if !(coreLift $ exists concatedName)
            then pure $ Just $ "file://" ++ concatedName
            else pure Nothing

  let Just sourceStr = mSourceStr
      | Nothing => do
          logString Debug "gotoDefinition: \{fname} referring module out of project."
          pure Nothing

  let Right (uri, _) = parse uriReferenceParser sourceStr
      | Left err => do
          logString Debug "gotoDefinition: URI parse error: \{err} \{show (sourceStr, sline, scol)}"
          pure Nothing

  pure $ Just $ MkLocation uri (MkRange (MkPosition sline scol) (MkPosition eline ecol))

export
gotoDefinition : Ref Ctxt Defs
              => Ref MD Metadata
              => Ref LSPConf LSPConfiguration
              => DefinitionParams -> Core (Maybe Location)
gotoDefinition params = do
  -- Check actual doc
  Just (actualUri, _) <- gets LSPConf openFile
    | Nothing => do
        logString Debug "gotoDefinition: openFile returned Nothing."
        pure Nothing
  let True = actualUri == params.textDocument.uri
      | False => do
          logString Debug "gotoDefinition: different URI than expected \{show (actualUri, params.textDocument.uri)}"
          pure Nothing

  let line = params.position.line
  let col  = params.position.character

  nameLocs <- gets MD nameLocMap
  let Just (_, name) = findInTreeLoc (line, col) nameLocs
    | Nothing => do
        logString Debug "gotoDefinition: Couldn't find a name at \{show (line, col)}"
        pure Nothing

  Nothing <- findTypeAt $ anyWithName name $ within (line, col)
    | Just _ => do
        logString Debug "gotoDefinition: \{show name} was a local name."
        pure Nothing

  Just globalDef <- lookupCtxtExact name !(gets Ctxt gamma)
    | Nothing => do
        logString Debug "gotoDefinition: \{show name} didn't have a GlobalDef."
        pure Nothing

  case globalDef.location of
    EmptyFC => do
      logString Debug "gotoDefinition: \{show name} didn't have location information."
      pure Nothing
    MkFC f s e => mkLocation f s e


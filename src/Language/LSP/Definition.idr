module Language.LSP.Definition

import Core.Context
import Core.Core
import Core.Directory
import Core.Env
import Core.Metadata
import Core.Options
import Data.List
import Data.String
import Data.String.Parser
import Data.URI
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Configuration
import Server.Log
import Server.Utils
import System.File
import System.Path

mkLocation : Ref Ctxt Defs
          => Ref LSPConf LSPConfiguration
          => OriginDesc -> (Int, Int) -> (Int, Int) -> Core (Maybe Location)
mkLocation origin (sline, scol) (eline, ecol) = do
  defs <- get Ctxt
  let PhysicalIdrSrc modIdent = origin
    | _ => do
       logString Debug "gotoDefinition: Origin doesn't have an Idris file attached to it \{show origin}"
       pure Nothing
  let wdir = defs.options.dirs.working_dir
  let pkg_dirs = filter (/= ".") defs.options.dirs.extra_dirs
  let exts = map show listOfExtensions
  Just fname_abs <- catch
      (Just . (wdir </>) <$> nsToSource replFC modIdent) -- Try local source first
      -- if not found, try looking for the file amongst the loaded packages.
      (const $ firstAvailable $ do
        pkg_dir <- pkg_dirs
        -- Assume that if the package dir is relative, then it must be relative to
        -- the working directory of the running Idris2 instance.
        let pkg_dir_abs = ifThenElse (isRelative pkg_dir) (wdir </> pkg_dir) pkg_dir
        ext <- exts
        pure (pkg_dir_abs </> ModuleIdent.toPath modIdent <.> ext))
    | _ => do
      logString Debug "gotoDefinition: Can't find file for module \{show modIdent}"
      pure Nothing

  let fname_abs_uri = "file://" ++ fname_abs

  let Right (uri, _) = parse uriReferenceParser fname_abs_uri
      | Left err => do
          logString Debug "gotoDefinition: URI parse error: \{err} \{show (fname_abs_uri, sline, scol)}"
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
  let Just (_, name) = findPointInTreeLoc (line, col) nameLocs
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
    MkFC f s e => mkLocation f s e
    MkVirtualFC f s e => mkLocation f s e
    _ => do
      logString Debug "gotoDefinition: \{show name} didn't have location information."
      pure Nothing

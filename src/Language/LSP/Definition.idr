module Language.LSP.Definition

import Core.Context
import Core.Core
import Core.Directory
import Core.Env
import Core.Metadata
import Core.Options
import Data.List
import Data.List.Lazy
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
  let pkg_dirs = filter (/= ".") defs.options.dirs.extra_dirs
  let exts = map show listOfExtensions
  Just fname <- catch
      (Just <$> nsToSource replFC modIdent) -- Try local source first
      -- if not found, try looking for the file amonst the loaded packages.
      (const $ firstAvailable $ do
        pkg_dir <- pkg_dirs
        ext <- exts
        pure (pkg_dir </> ModuleIdent.toPath modIdent <.> ext))
    | _ => do
      logString Debug "gotoDefinition: Can't find file for module \{show modIdent}"
      pure Nothing

  -- URIs can refer relative or absolute paths, also they can refer modules
  -- out of the current project.
  Just (Just (Here path)) <- map rootPath <$> gets LSPConf initialized
    | _ => do
        logString Debug "gotoDefinition: Root path was not set."
        pure Nothing

  mFileURI <- pure $ choice
    [ check !(coreLift (exists (path </> fname))) $ "file://" ++ (path </> fname)
    , check !(coreLift (exists fname))            $ "file://" ++ fname
    ]

  let Just fileURI = mFileURI
      | Nothing => do
          logString Debug "gotoDefinition: Could not create URI of path \{path} and file \{fname}."
          pure Nothing

  let Right (uri, _) = parse uriReferenceParser fileURI
      | Left err => do
          logString Debug "gotoDefinition: URI parse error: \{err} \{show (fname, sline, scol)}"
          pure Nothing

  pure $ Just $ MkLocation uri (MkRange (MkPosition sline scol) (MkPosition eline ecol))
  where
    check : Bool -> Lazy a -> Maybe a
    check True  a = Just a
    check False _ = Nothing

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

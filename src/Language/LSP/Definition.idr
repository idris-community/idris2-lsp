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

normalizePathSep : Char -> Char
normalizePathSep '\\' = '/'
normalizePathSep ch = ch

mkLocation : Ref Ctxt Defs
          => Ref LSPConf LSPConfiguration
          => OriginDesc -> (Int, Int) -> (Int, Int) -> Core (Maybe Location)
mkLocation origin (sline, scol) (eline, ecol) = do
  defs <- get Ctxt
  let PhysicalIdrSrc modIdent = origin
    | _ => do logD GotoDefinition "Origin doesn't have an Idris file attached to it \{show origin}"
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
    | _ => do logD GotoDefinition "Can't find file for module \{show modIdent}"
              pure Nothing

  let fname_abs2 = fastPack (map normalizePathSep (fastUnpack fname_abs))
  let fname_abs_uri = "file:///" ++ fname_abs2

  let Right (uri, _) = parse uriReferenceParser fname_abs_uri
    | Left err => do logE GotoDefinition "URI parse error: \{err} \{show (fname_abs_uri, sline, scol)}"
                     pure Nothing

  pure $ Just $ MkLocation uri (MkRange (MkPosition sline scol) (MkPosition eline ecol))

export
gotoDefinition : Ref Ctxt Defs
              => Ref MD Metadata
              => Ref LSPConf LSPConfiguration
              => DefinitionParams -> Core (Maybe Location)
gotoDefinition params = do
  logI GotoDefinition "Checking for \{show params.textDocument.uri} at \{show params.position}"
  -- Check actual doc
  Just (actualUri, _) <- gets LSPConf openFile
    | Nothing => do
        logE GotoDefinition "No open file"
        pure Nothing
  let True = actualUri == params.textDocument.uri
      | False => do
          logD GotoDefinition "Expected request for the currently opened file \{show actualUri}, instead received \{show params.textDocument.uri}"
          pure Nothing

  let line = params.position.line
  let col  = params.position.character

  nameLocs <- gets MD nameLocMap
  let Just (_, name) = findPointInTreeLoc (line, col) nameLocs
    | Nothing => do logD GotoDefinition "No name found at \{show line}:\{show col}}"
                    pure Nothing
  logI GotoDefinition "Found name \{show name}"

  Nothing <- findTypeAt $ anyWithName name $ within (line, col)
    | Just _ => do
        logD GotoDefinition "\{show name} is a local name"
        pure Nothing

  Just globalDef <- lookupCtxtExact name !(gets Ctxt gamma)
    | Nothing => do
        logD GotoDefinition "\{show name} is not in the global context"
        pure Nothing
  logI GotoDefinition "Found definition of \{show name}"

  case globalDef.location of
    MkFC f s e => mkLocation f s e
    MkVirtualFC f s e => mkLocation f s e
    _ => do
      logD GotoDefinition "\{show name} doesn't have location information."
      pure Nothing

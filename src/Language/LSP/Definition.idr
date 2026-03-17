module Language.LSP.Definition

import Core.Context
import Core.Core
import Core.Directory
import Core.Env
import Core.Metadata
import Core.Name.Namespace
import Core.Options
import Data.List
import Data.List1
import Data.String
import Data.String.Parser
import Data.URI
import Language.LSP.Message
import Language.LSP.Utils
import Libraries.Data.PosMap
import Server.Configuration
import Server.Log
import Server.Utils
import System.File
import System.Path

mkLocation' : Ref Ctxt Defs
           => Ref LSPConf LSPConfiguration
           => OriginDesc -> (Int, Int) -> (Int, Int) -> Core (Maybe Location)
mkLocation' origin (sline, scol) (eline, ecol) = do
  defs <- get Ctxt
  let PhysicalIdrSrc modIdent = origin
    | _ => logD GotoDefinition "Origin doesn't have an Idris file attached to it \{show origin}" >> pure Nothing
  let wdir = defs.options.dirs.working_dir
  let pkg_dirs = filter (/= ".") (defs.options.dirs.extra_dirs ++ defs.options.dirs.package_dirs)
  let exts = show <$> listOfExtensions
  Just fname_abs <- catch
      (Just . (wdir </>) <$> nsToSource replFC modIdent) -- Try local source first
      -- if not found, try looking for the file amongst the loaded packages.
      (const $ firstAvailable $ do
        pkg_dir <- pkg_dirs
        -- Assume that if the package dir is relative, then it must be relative to
        -- the working directory of the running Idris2 instance.
        let pkg_dir_abs = ifThenElse (isRelative pkg_dir) (wdir </> pkg_dir) pkg_dir
        ext <- exts
        pure (pkg_dir_abs </> toPath modIdent <.> ext))
    | _ => logD GotoDefinition "Can't find file for module \{show modIdent}" >> pure Nothing

  let fname_abs_uri = "file://\{systemPathToURIPath fname_abs}"

  -- Escape is needed, filename may contain characters reserved in URI, like whitespace
  let Right uri = escapeAndParseURI fname_abs_uri
    | Left err => do logE GotoDefinition "URI parse error: \{err} \{show (fname_abs_uri, sline, scol)}"
                     pure Nothing

  pure $ Just $ MkLocation uri (MkRange (MkPosition sline scol) (MkPosition eline ecol))

export
mkLocation : Ref Ctxt Defs
          => Ref LSPConf LSPConfiguration
          => FC -> Core (Maybe Location)
mkLocation (MkFC f s e) = mkLocation' f s e
mkLocation (MkVirtualFC f s e) = mkLocation' f s e
mkLocation _ = pure Nothing

-- Drop N characters from a String
dropStr : Nat -> String -> String
dropStr n s = pack (List.drop n (unpack s))

-- Extract module name from an import line, e.g.:
--   "import Data.List"          -> Just "Data.List"
--   "import public Data.String" -> Just "Data.String"
--   "import Data.List hiding (map)" -> Just "Data.List"
parseImportLine : String -> Maybe String
parseImportLine line =
  let trimmed = ltrim line
  in if "import " `isPrefixOf` trimmed
     then let rest  = ltrim (dropStr 7 trimmed)
              rest' = if "public " `isPrefixOf` rest
                      then ltrim (dropStr 7 rest)
                      else rest
              modName = pack $ takeWhile (\c => c /= ' ' && c /= '(' && c /= '\r' && c /= '\n') (unpack rest')
          in if modName == "" then Nothing else Just modName
     else Nothing

-- Convert "Data.List" -> ModuleIdent (parts stored in reverse)
parseModuleIdent : String -> Maybe ModuleIdent
parseModuleIdent s =
  let parts = filter (/= "") (forget (split (== '.') s))
  in case parts of
       [] => Nothing
       _  => Just (unsafeFoldModuleIdent (reverse parts))

-- Try go-to-definition for an import statement at the given line
gotoImportDefinition : Ref Ctxt Defs
                    => Ref LSPConf LSPConfiguration
                    => DocumentURI -> Int -> Core (Maybe Location)
gotoImportDefinition uri lineNum = do
  Just (_, src) <- gets LSPConf (lookup uri . virtualDocuments)
    | Nothing => pure Nothing
  let srcLines = lines src
  let lineStr :: _ = drop (the Nat (cast lineNum)) srcLines
    | [] => pure Nothing
  let Just modName = parseImportLine lineStr
    | Nothing => pure Nothing
  let Just modIdent = parseModuleIdent modName
    | Nothing => pure Nothing
  logI GotoDefinition "Trying import definition for module \{modName}"
  defs <- get Ctxt
  let wdir = defs.options.dirs.working_dir
  let pkg_dirs = filter (/= ".") (defs.options.dirs.extra_dirs ++ defs.options.dirs.package_dirs)
  let exts = show <$> listOfExtensions
  Just fname_abs <- catch
      (Just . (wdir </>) <$> nsToSource replFC modIdent)
      (const $ firstAvailable $ do
        pkg_dir <- pkg_dirs
        let pkg_dir_abs = ifThenElse (isRelative pkg_dir) (wdir </> pkg_dir) pkg_dir
        ext <- exts
        pure (pkg_dir_abs </> toPath modIdent <.> ext))
    | _ => logD GotoDefinition "Can't find source for module \{modName}" >> pure Nothing
  let fname_abs_uri = "file://\{systemPathToURIPath fname_abs}"
  let Right uri' = escapeAndParseURI fname_abs_uri
    | Left err => do logE GotoDefinition "URI parse error for import: \{err}"
                     pure Nothing
  pure $ Just $ MkLocation uri' (MkRange (MkPosition 0 0) (MkPosition 0 0))

export
gotoDefinition : Ref Ctxt Defs
              => Ref MD Metadata
              => Ref LSPConf LSPConfiguration
              => DefinitionParams -> Core (Maybe Location)
gotoDefinition params = do
  logI GotoDefinition "Checking for \{show params.textDocument.uri} at \{show params.position}"
  -- Check actual doc
  Just (actualUri, _) <- gets LSPConf openFile
    | Nothing => logE GotoDefinition "No open file" >> pure Nothing
  let True = actualUri == params.textDocument.uri
      | False => do
          logD GotoDefinition "Expected request for the currently opened file \{show actualUri}, instead received \{show params.textDocument.uri}"
          pure Nothing

  let line = params.position.line
  let col  = params.position.character

  nameLocs <- gets MD nameLocMap
  let Just (_, name) = findPointInTreeLoc (line, col) nameLocs
    | Nothing => do
        logD GotoDefinition "No name found at \{show line}:\{show col}}, trying import fallback"
        gotoImportDefinition params.textDocument.uri line
  logI GotoDefinition "Found name \{show name}"

  Nothing <- findTypeAt $ anyWithName name $ within (line, col)
    | Just _ => logD GotoDefinition "\{show name} is a local name" >> pure Nothing

  context <- gets Ctxt gamma
  Just globalDef <- lookupCtxtExact name context
    | Nothing => logD GotoDefinition "\{show name} is not in the global context" >> pure Nothing
  logI GotoDefinition "Found definition of \{show name}"

  mkLocation globalDef.location

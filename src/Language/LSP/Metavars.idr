module Language.LSP.Metavars

import Core.Context
import Core.Core
import Core.Directory
import Core.Env
import Core.TT
import Core.Name
import Idris.IDEMode.Holes
import Idris.Pretty
import Idris.Pretty.Render
import Idris.REPL.Opts
import Idris.Resugar
import Idris.Syntax
import Language.JSON
import Language.LSP.Message
import Language.LSP.Definition
import Server.Configuration
import Server.Log
import Server.Utils
import System.File
import System.Path
import Language.Reflection
import Libraries.Text.PrettyPrint.Prettyprinter.Symbols

%language ElabReflection
%hide Language.Reflection.TT.FC
%hide Language.Reflection.TT.Name
%hide Idris.IDEMode.Holes.getUserHolesData

-- Non-exported functions are temporary changes to functions in the module Idris.IDEMode.Holes.
-- TODO: upstream changes to the compiler when stability is reached.

public export
record Premise where
  constructor MkPremise
  name : String
  location : Maybe Location
  type : String
  multiplicity : String
  isImplicit : Bool
%runElab deriveJSON defaultOpts `{Metavars.Premise}

public export
record Metavar where
  constructor MkMetavar
  name : String
  location : Maybe Location
  type : String
  premises : List Metavars.Premise
%runElab deriveJSON defaultOpts `{Metavars.Metavar}

showName : Name -> Bool
showName (UN Underscore) = False
showName (MN _ _) = False
showName _ = True

getUserHolesData : Ref Ctxt Defs
                => Ref Syn SyntaxInfo
                => Core (List (Holes.Data, FC))
getUserHolesData = do
  defs <- get Ctxt
  ms <- getUserHoles
  globs <- concat <$> traverse (flip lookupCtxtName defs.gamma) ms
  let holesWithArgs = mapMaybe (\(n, i, gdef) => pure (n, gdef, !(isHole gdef))) globs
  traverse (\(n, gdef, args) => (, gdef.location) <$> holeData defs Env.empty n args gdef.type) holesWithArgs

||| Returns the list of metavariables visible in the current context with their location.
||| JSON schema for a single metavariable:
||| {
|||   location: Location | null;
|||   name: string;
|||   type: string;
|||   premises: Premise[]
||| }
||| JSON schema for a single premise:
||| {
|||   location: Location | null;
|||   name: string;
|||   type: string;
|||   multiplicity: string;
|||   isImplicit: bool;
||| }
export
metavarsCmd : Ref Ctxt Defs
           => Ref Syn SyntaxInfo
           => Ref ROpts REPLOpts
           => Ref LSPConf LSPConfiguration
           => Core (List Metavar)
metavarsCmd = do
    logI Metavars "Fetching metavar"
    c <- getColor
    setColor False
    holes <- getUserHolesData
    logI Metavars "Metavars fetched, found \{show $ length holes}"
    res <- traverse (uncurry mapHole) holes
    setColor c
    pure res
  where
    mapContext : Holes.Premise -> Core Metavars.Premise
    mapContext p = do
      loc <- mkLocation replFC
      type <- render (reAnnotate Syntax $ pretty p.type)
      pure $ MkPremise { location = loc, name = show p.name, type = type, isImplicit = p.isImplicit, multiplicity = show p.multiplicity }
    mapHole : Holes.Data -> FC -> Core Metavar
    mapHole h fc = do
      loc <- mkLocation fc
      let name = show h.name
      type <- render (reAnnotate Syntax $ pretty h.type)
      premises <- traverse mapContext h.context
      pure $ MkMetavar { location = loc, name = show h.name, type = type, premises = premises }

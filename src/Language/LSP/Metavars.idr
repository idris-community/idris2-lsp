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
%hide Holes.HolePremise
%hide Holes.HoleData
%hide Holes.getUserHolesData

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
%runElab deriveJSON defaultOpts `{Premise}

public export
record Metavar where
  constructor MkMetavar
  name : String
  location : Maybe Location
  type : String
  premises : List Premise
%runElab deriveJSON defaultOpts `{Metavar}

record HolePremise where
  constructor MkHolePremise
  name         : Name
  location     : FC
  type         : IPTerm
  multiplicity : RigCount
  isImplicit   : Bool

record HoleData where
  constructor MkHoleData
  name : Name
  location : FC
  type : IPTerm
  context : List HolePremise

showName : Name -> Bool
showName (UN Underscore) = False
showName (MN _ _) = False
showName _ = True

-- TODO: Check why premise location is replFC.
extractHoleData : {vars : _}
               -> Ref Ctxt Defs
               => Ref Syn SyntaxInfo
               => Defs -> Env Term vars -> Name -> FC -> Nat -> Term vars
               -> Core HoleData
extractHoleData defs env fn fc (S args) (Bind _ _ (Let _ _ val _) sc) =
  extractHoleData defs env fn fc args (subst val sc)
extractHoleData defs env fn fc (S args) (Bind fc' x b sc) = do
  rest <- extractHoleData defs (b :: env) fn fc args sc
  let True = showName x
    | False => pure rest
  ity <- resugar env =<< normalise defs env (binderType b)
  let premise = MkHolePremise x fc' ity (multiplicity b) (isImplicit b)
  pure $ record { context $= (premise ::) } rest
extractHoleData defs env fn fc args ty = do
  ity <- resugar env =<< normalise defs env ty
  pure $ MkHoleData fn fc ity []

holeData : {vars : _}
        -> Ref Ctxt Defs
        => Ref Syn SyntaxInfo
        => Defs -> Env Term vars -> Name -> FC -> Nat -> Term vars
        -> Core HoleData
holeData gam env fn fc args ty = do
  hdata <- extractHoleData gam env fn fc args ty
  pp <- getPPrint
  pure $ if showImplicits pp
            then hdata
            else record { context $= dropShadows } hdata
  where
    dropShadows : List HolePremise -> List HolePremise
    dropShadows [] = []
    dropShadows (premise :: rest) =
      if premise.name `elem` map name rest
         then            dropShadows rest
         else premise :: dropShadows rest

getUserHolesData : Ref Ctxt Defs
                => Ref Syn SyntaxInfo
                => Core (List HoleData)
getUserHolesData = do
  defs <- get Ctxt
  ms <- getUserHoles
  globs <- concat <$> traverse (flip lookupCtxtName defs.gamma) ms
  let holesWithArgs = mapMaybe (\(n, i, gdef) => pure (n, gdef, !(isHole gdef))) globs
  traverse (\(n, gdef, args) => holeData defs [] n gdef.location args gdef.type) holesWithArgs

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
  logI Metavars "Fetching metavars"
  c <- getColor
  setColor False
  holes <- getUserHolesData
  logI Metavars "Metavars fetched, found \{show $ length holes}"
  res <- for holes $ \h => do
    loc <- mkLocation h.location
    let name = show h.name
    type <- render (reAnnotate Syntax $ prettyTerm h.type)
    premises <- for h.context $ \p => do
      loc <- mkLocation p.location
      let name = show p.name
      type <- render (reAnnotate Syntax $ prettyTerm p.type)
      pure $ MkPremise { location = loc, name = name, type = type, isImplicit = p.isImplicit, multiplicity = show p.multiplicity }
    pure $ MkMetavar { location = loc, name = name, type = type, premises = premises }
  setColor c
  pure res

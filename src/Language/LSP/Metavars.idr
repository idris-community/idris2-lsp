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
import Server.Utils
import System.File
import System.Path

%hide Holes.HolePremise
%hide Holes.HoleData
%hide Holes.getUserHolesData

-- Non-exported functions are temporary changes to functions in the module Idris.IDEMode.Holes.
-- TODO: upstream changes to the compiler when stability is reached.

record HolePremise where
  constructor MkHolePremise
  name         : Name
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

extractHoleData : {vars : _}
               -> Ref Ctxt Defs
               => Ref Syn SyntaxInfo
               => Defs -> Env Term vars -> Name -> FC -> Nat -> Term vars
               -> Core HoleData
extractHoleData defs env fn fc (S args) (Bind _ _ (Let _ _ val _) sc) =
  extractHoleData defs env fn fc args (subst val sc)
extractHoleData defs env fn fc (S args) (Bind _ x b sc) = do
  rest <- extractHoleData defs (b :: env) fn fc args sc
  let True = showName x
    | False => pure rest
  ity <- resugar env !(normalise defs env (binderType b))
  let premise = MkHolePremise x ity (multiplicity b) (isImplicit b)
  pure $ record { context $= (premise ::) } rest
extractHoleData defs env fn fc args ty = do
  nty <- normalise defs env ty
  ity <- resugar env nty
  pure $ MkHoleData fn fc ity []

holeData : {vars : _}
        -> Ref Ctxt Defs
        => Ref Syn SyntaxInfo
        => Defs -> Env Term vars -> Name -> FC -> Nat -> Term vars ->
           Core HoleData
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
  let ctxt = gamma defs
  ms  <- getUserHoles
  let globs = concat !(traverse (\n => lookupCtxtName n ctxt) ms)
  let holesWithArgs = mapMaybe (\(n, i, gdef) => pure (n, gdef, !(isHole gdef))) globs
  traverse (\(n, gdef, args) => holeData defs [] n (location gdef) args (type gdef)) holesWithArgs

||| Returns the list of metavariables visible in the current context with their location.
||| JSON schema for a single metavariable:
||| {
|||   location: Location | null;
|||   name: string;
|||   type: string;
||| }
export
metavarsCmd : Ref Ctxt Defs
           => Ref Syn SyntaxInfo
           => Ref ROpts REPLOpts
           => Ref LSPConf LSPConfiguration
           => Core JSON
metavarsCmd = do
  c <- getColor
  setColor False
  holes <- getUserHolesData
  res <- for holes $ \h => do
    loc <- case h.location of
      MkFC f s e => mkLocation f s e
      MkVirtualFC f s e => mkLocation f s e
      _ => pure Nothing
    let name = show h.name
    type <- render (reAnnotate Syntax $ prettyTerm h.type)
    pure $ JObject [("location", toJSON loc), ("name", toJSON name), ("type", toJSON type)]
  setColor c
  pure $ JArray res

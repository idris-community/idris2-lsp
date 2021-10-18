module Language.LSP.CodeAction.RefineHole

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.Unify
import Core.UnifyState
import Core.Value
import Data.Nat
import Data.String
import Idris.REPL.Common
import Idris.REPL.Opts
import Idris.Syntax
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.CodeAction.Utils
import Language.LSP.Message
import Libraries.Data.NameMap
import Libraries.Data.PosMap
import TTImp.Elab.App
import TTImp.Elab.Check
import TTImp.TTImp
import TTImp.Unelab
import Server.Configuration
import Server.Log
import Server.Utils

-- CodeAction for filling holes, not necessarily type correct constructions.
-- This code action does three things:
-- * Tries to determine the type of the hole and offers valid constructors with new holes for parameters.
--   Poor man's hole refining.
-- * Tries to find similar names from the context.
--   In many cases I find myself writing out the hole name somewhat similar to final function I want to
--   call later on, in the given hole.
-- * Tries to find names which are safely fit to the hole.

||| Tries to resolve a type where Like Nat, Maybe
||| It ignores the type parameters
resolveTypeName
  :  Ref Ctxt Defs
  => Ref LSPConf LSPConfiguration
  => {vars : _}
  -> Term vars
  -> Core (Maybe Name)
resolveTypeName (Ref fc x name)     = Just <$> toFullNames name
resolveTypeName (Bind fc x b scope) = resolveTypeName scope -- ignores explicit/implicit parameters: {a} -> DataType
resolveTypeName (App fc fn arg)     = resolveTypeName fn    -- ignores type parameters: DataType a
resolveTypeName other = do
  logD RefineHole "resolveTypeName: \{show other}"
  pure Nothing

||| Checks if the the Term represents an explicit function
isFunctionType : Term vars -> Bool
isFunctionType (Bind fc x (Lam fc1 y Implicit ty)         scope) = isFunctionType scope
isFunctionType (Bind fc x (Lam fc1 y Explicit ty)         scope) = True
isFunctionType (Bind fc x (Lam fc1 y AutoImplicit ty)     scope) = isFunctionType scope
isFunctionType (Bind fc x (Lam fc1 y (DefImplicit z) ty)  scope) = isFunctionType scope
isFunctionType (Bind fc x (Pi  fc1 y Implicit ty)         scope) = isFunctionType scope
isFunctionType (Bind fc x (Pi  fc1 y Explicit ty)         scope) = True
isFunctionType (Bind fc x (Pi  fc1 y AutoImplicit ty)     scope) = isFunctionType scope
isFunctionType (Bind fc x (Pi  fc1 y (DefImplicit z) ty)  scope) = isFunctionType scope
isFunctionType other               = False

||| Checks if the name is visible in the namespace of the hole.
checkVisibleName : Ref Ctxt Defs => Name -> Name -> GlobalDef -> Core Bool
checkVisibleName h n g = do
  fn <- toFullNames n
  fh <- toFullNames h
  let (Just namespc, _) = displayName fn
      | _ => pure False
  let (Just hnamespc, _) = displayName fh
      | _ => pure False
  case namespc == hnamespc of
    -- Same namespace everything is visible.
    True  => pure True
    -- Different namespace, Private members are hidden.
    False => do
      vs <- isVisible namespc
      pure $ vs && g.visibility /= Private

||| Learn the user defined name of the explicit paramenters for a type of the constructor, of the length
||| of the names matches the arity, then these names are the names for the field with
||| a high probability.
constructorParameters : Term vars -> List (Maybe Name)
constructorParameters (Bind fc x (Lam fc1 y Explicit ty) scope) =
  case isUserName x of
    True  => Just x :: constructorParameters scope
    False => Nothing :: constructorParameters scope
constructorParameters (Bind fc x (Lam fc1 y z ty) scope) =
  constructorParameters scope
constructorParameters (Bind fc x (Pi fc1 y Explicit ty) scope) =
  case isUserName x of
    True  => Just x :: constructorParameters scope
    False => Nothing :: constructorParameters scope
constructorParameters (Bind fc x (Pi fc1 y z ty) scope) =
  constructorParameters scope
constructorParameters other = []

data ConstructorInfo
  = UnnamedFieldsCon Name Nat
  | NamedFieldsCon   Name (List (Maybe Name))

||| Generate a list of constructors for the given type.
constructors : Ref LSPConf LSPConfiguration => Ref Ctxt Defs => Name -> Name -> Core (Maybe (List ConstructorInfo))
constructors holeName typeName = do
  defs <- get Ctxt
  Just gdef <- lookupCtxtExact !(toFullNames typeName) defs.gamma
    | Nothing => do
        pure Nothing
  let (TCon tag arity parampos detpos flags mutwith datacons detagabbleBy) = gdef.definition
    | _ => do
        pure Nothing
  -- Possible constructor informations, if something is not a constructor (for some reason)
  -- it is marked as Nothing
  mConsInfo
    <- traverse
        (\consName => do
          rConName <- toFullNames consName
          gConDef <- lookupCtxtExact rConName defs.gamma
          case gConDef of
            Nothing => pure Nothing
            Just gd => case (!(checkVisibleName holeName rConName gd), gd.definition) of
              (False, _) => pure Nothing
              (_, DCon tag arity newtypeArg) => do
                let arity' = minus arity (length gd.eraseArgs)
                possibleFieldNames <- constructorParameters <$> toFullNames gd.type
                pure
                  $ Just
                  $ if arity' == length possibleFieldNames
                      then NamedFieldsCon   rConName possibleFieldNames
                      else UnnamedFieldsCon rConName arity'
              other => pure Nothing)
        datacons
  -- If all are constructors we are OK
  pure $ sequence mConsInfo

generateHoleName : Ref Ctxt Defs => String -> Nat -> Core (String, Nat)
generateHoleName holeName k = do
  defs <- get Ctxt
  let n = holeName ++ "_" ++ show k
  case !(lookupCtxtName (UN $ Basic n) defs.gamma) of
    [] => pure (n, k+1)
    _ => generateHoleName holeName (k + 1)

||| Generate hole names if we only have arity information
generateHoleNames : Ref Ctxt Defs => String -> Nat -> Core (Nat, List String)
generateHoleNames baseName 0     = pure (0, [])
generateHoleNames baseName (S k) = do
  (n, rest) <- generateHoleNames baseName k
  (holeName, n') <- generateHoleName baseName n
  pure (n',("?" ++ holeName) :: rest)

||| Generate hole names if we have mixed information, names an unknown names in the arguments
genereateFieldHoleNames : Ref Ctxt Defs => String -> List (Maybe Name) -> Core (Nat, List String)
genereateFieldHoleNames baseName [] = pure (0, [])
genereateFieldHoleNames baseName (f :: fs) = do
  (n, rest) <- genereateFieldHoleNames baseName fs
  (holeName, n') <- generateHoleName (maybe baseName (show . dropAllNS) f) n
  pure (n', ("?" ++ holeName) :: rest)

renderConstructor : Ref Ctxt Defs => Name -> ConstructorInfo -> Core (Maybe String)
renderConstructor holeName conInfo = do
  case conInfo of
    UnnamedFieldsCon fullName 0 => pure $ Just $ show $ dropAllNS fullName
    UnnamedFieldsCon fullName n => do
      let Just holeName' = displayUserName <$> userNameRoot holeName
          | Nothing => pure Nothing
      (_, newHoles) <- generateHoleNames holeName' n
      pure $ Just $ "(" ++ show (dropAllNS fullName) ++ " " ++ unwords newHoles ++ ")"
    NamedFieldsCon fullName [] => pure $ Just $ show $ dropAllNS fullName
    NamedFieldsCon fullName fieldNames => do
      let Just holeName' = displayUserName <$> userNameRoot holeName
          | Nothing => pure Nothing
      (_, newHoles) <- genereateFieldHoleNames holeName' fieldNames
      pure $ Just $ "(" ++ show (dropAllNS fullName) ++ " " ++ unwords newHoles ++ ")"

fcToRange : FC -> Maybe Range
fcToRange (MkFC x (sline,scol) (eline, ecol))
  = Just (MkRange (MkPosition sline scol)
                  (MkPosition eline ecol))
fcToRange (MkVirtualFC x (sline,scol) (eline, ecol))
  = Just (MkRange (MkPosition sline scol)
                  (MkPosition eline ecol))
fcToRange EmptyFC = Nothing

||| Create a CodeAction with string to replace the hole, insert (!) if the substituition possibly non-well-typed.
refineHoleWith : Bool -> FC -> CodeActionParams -> String -> CodeAction
refineHoleWith maybeNonTypesafe holeLoc params nameString =
  MkCodeAction
    { title       = "Fill Hole ~ \{strSubstr 0 50 nameString}" ++ if maybeNonTypesafe then " (!)" else ""
    , kind        = Just RefactorRewrite
    , diagnostics = Just []
    , isPreferred = Just False -- not a quickfix
    , disabled    = Nothing
    , edit        = Just $ MkWorkspaceEdit
        { changes           = Just (singleton params.textDocument.uri
            [ MkTextEdit range nameString ])
        , documentChanges   = Nothing
        , changeAnnotations = Nothing
        }
    , command     = Nothing
    , data_       = Nothing
    }
  where
    range : Range
    range = fromMaybe params.range $ fcToRange holeLoc

isHole : Defs -> Name -> Core Bool
isHole defs n = do
  Just def <- lookupCtxtExact n (gamma defs)
    | Nothing => do pure False
  pure $ case definition def of
    Hole _ _ => True
    _        => False

||| Check if the name has the exact type
nameHasType : Ref LSPConf LSPConfiguration
  => Ref Ctxt Defs
  => Ref MD Metadata
  => Ref UST UState
  => Name -> Name -> Term [] -> Core Bool
nameHasType holeName nameRef expected = do
  defs <- get Ctxt
  fullName <- toFullNames nameRef
  Just def <- lookupCtxtExact fullName defs.gamma
    | Nothing => pure False
  True <- checkVisibleName holeName fullName def
    | False => pure False
  nt <- normaliseAll defs [] (GlobalDef.type def)
  ex <- normaliseAll defs [] expected
  equivTypes nt ex

findFirstN : Nat -> (a -> Core (Maybe b)) -> List a -> Core (List b)
findFirstN Z f xs = pure []
findFirstN (S k) f [] = pure []
findFirstN (S k) f (x :: xs) = case !(f x) of
  Nothing => findFirstN (S k) f xs
  Just y  => map (y ::) (findFirstN k f xs)

||| Keeps the name with same preffix as the hole, ignoring namespaces.
typeMatchingNames
  :  Ref LSPConf LSPConfiguration
  => Ref Ctxt Defs
  => Ref MD Metadata
  => Ref UST UState
  => Name -> Term [] -> Nat -> Core (List String)
typeMatchingNames holeName ty limit = do
  defs <- get Ctxt
  let hn = userNameRoot holeName
  findFirstN limit
      (\funcName =>
        let sn = userNameRoot funcName
        in if hn /= sn && !(nameHasType holeName funcName ty)
              then pure (displayUserName <$> userNameRoot funcName)
              else pure Nothing)
    $ keys
    $ namesResolvedAs defs.gamma

keepNonHoleNames : Ref Ctxt Defs => List String -> Core (List String)
keepNonHoleNames = map catMaybes . traverse nonHoleName
  where
    -- If the name is uniquely determines a hole filter out from the list
    -- if the name is not unique keep it around.
    nonHoleName : String -> Core (Maybe String)
    nonHoleName name = do
      -- Find the type information at this hole
      defs <- get Ctxt
      [(_, (_, holeGlobalDef))] <- lookupCtxtName (UN $ Basic name) (gamma defs)
        | _ => do
          pure $ Just name
      pure $ case holeGlobalDef.definition of
        Hole{} => Nothing
        _      => Just name

export
refineHole
  :  Ref LSPConf LSPConfiguration
  => Ref MD Metadata
  => Ref Ctxt Defs
  => Ref UST UState
  => Ref Syn SyntaxInfo
  => Ref ROpts REPLOpts
  => CodeActionParams -> Core (List CodeAction)
refineHole params = do
  logI RefineHole "Checking for \{show params.textDocument.uri} at \{show params.range}"
  withSingleLine RefineHole params (pure []) $ \line => do
    withMultipleCache RefineHole params RefineHole $ do

      meta <- get MD
      let col = params.range.start.character
      let Just (loc, name) = findPointInTreeLoc (line, col) (nameLocMap meta)
        | Nothing => do logD RefineHole "No name found at \{show line}:\{show col}}"
                        pure []
      logD RefineHole "Found name \{show name}"

      -- The name is a hole
      defs <- get Ctxt
      True <- isHole defs name -- should only work on holes
        | _ => do logD RefineHole $ "\(show name) is not a metavariable"
                  pure []

      -- Find the type information at this hole
      [(holeName, (x, holeGlobalDef))] <- lookupCtxtName name (gamma defs)
        | _ => do
          logD RefineHole "Non-unique context entries for \{show name}"
          pure []

      logI RefineHole "Searching constructors"
      -- Try to find some constructors
      constructorStrings <-
        -- If the type is a datatype find its definition
        case !(resolveTypeName holeGlobalDef.type) of
          Nothing => pure (the (List String) [])
          Just dataTypeName =>
            if isFunctionType holeGlobalDef.type
              -- If a hole represents a function, we shouldn't offer constructors
              then pure []
              -- Find its constructors
              else case !(constructors holeName dataTypeName) of
                    Nothing => pure []
                    Just [] => pure []
                    Just cns => do
                      -- Render the (Constructor ?field1 ?field2) like fields
                      someNameStrings <- traverse (renderConstructor name) cns
                      case the (Maybe (List String)) (sequence someNameStrings) of
                        Nothing => pure []
                        Just cs => pure cs

      -- Limit the results
      cfg <- get LSPConf
      let limit = cfg.searchLimit

      logI RefineHole "Searching type matching names"
      typesafeNames <- keepNonHoleNames !(typeMatchingNames holeName holeGlobalDef.type 1000)
      logI RefineHole "Searching type similar names"
      Just (simStr, allSimilars) <- getSimilarNames name
        --TODO: better error message
        | _ => do logD RefineHole $ "Error finding similar names to \(show name)"
                  pure []
      let similarStrings = showSimilarNames name simStr allSimilars
      similarNames <- keepNonHoleNames (filter (\n => not (elem n typesafeNames)) similarStrings)

      -- Render code actions that inject constructors or similar names
      let fillerStrings = nub (constructorStrings ++ typesafeNames)
      let safeFillers = map (refineHoleWith False holeGlobalDef.location params) fillerStrings
      let nonSafeFillerStrings = take limit $ filter (\x => elemBy (/=) x fillerStrings) similarNames
      let nonSafeFillers = map (refineHoleWith True holeGlobalDef.location params) nonSafeFillerStrings
      let fillers = safeFillers ++ nonSafeFillers

      pure [(cast loc, fillers)]

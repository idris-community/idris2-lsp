||| Automatic deriviation of JSON conversion interfaces.
|||
||| (C) The Idris Community, 2021
module Language.LSP.Message.Derive

-- NOTE: Elaborator functions are evaluated as if they are defined inside the
-- call location module, thus imported functions used inside elaborator
-- functions should be exported publicly or explicitly imported at call
-- location.
import public Data.Maybe
import public Data.SortedMap
import Data.String
import Language.JSON
import public Language.JSON.Interfaces
import Language.Reflection

%language ElabReflection
%default total

||| Options for automatic derivation of `ToJSON`/`FromJSON` instances.
public export
record JSONDeriveOpts where
  constructor MkOpts
  ||| If tagged, values are converted to JSON object with one field where the
  ||| key is the name of the constructor and the value is the object obtained
  ||| from converting the arguments of the constructor.
  tagged : Bool
  ||| Renaming rules for arguments where the name of the argument does not
  ||| match the correspondent key in the translated JSON object.
  renames : List (String, String)
  ||| Fields that must be present in the JSON translation but have static
  ||| values.
  staticFields : List (String, JSON)

public export
defaultOpts : JSONDeriveOpts
defaultOpts = MkOpts False [] []

stripNs : Name -> Name
stripNs (NS _ x) = x
stripNs x = x

covering
genReadableSym : String -> Elab Name
genReadableSym hint = do
  MN v i <- genSym hint
    | _ => fail "cannot generate readable argument name"
  pure $ UN $ Basic (v ++ show i)

app : TTImp -> TTImp -> TTImp
app = IApp EmptyFC

var : Name -> TTImp
var = IVar EmptyFC

bindvar : String -> TTImp
bindvar = IBindVar EmptyFC

primStr : String -> TTImp
primStr = IPrimVal EmptyFC . Str

patClause : TTImp -> TTImp -> Clause
patClause = PatClause EmptyFC

implicit' : TTImp
implicit' = Implicit EmptyFC True

||| Constructs a term with a given type constructor fully applied with
||| its explicit binders.
saturate : TTImp -> TTImp -> Elab TTImp
saturate (IPi _ rig ExplicitArg (Just n) arg ret) acc =
  saturate ret (app acc (bindvar (show n)))
saturate (IPi _ rig ExplicitArg Nothing arg ret) acc =
  saturate ret (app acc (bindvar (show !(genSym "arg"))))
saturate (IPi _ rig _ name arg ret) acc = saturate ret acc
saturate (IType _) acc = pure acc
saturate _ _ = fail "Type constructor with an unsupported signature"

-- TODO: add support for polymorphic types and maybe use another type for
-- optional fields in place of Maybe.
||| Automatic derivation of `ToJSON` instances.
||| NOTE: all the fields in each constructor MUST be named, record already
||| comply but types declared with data must have constructors declared like
||| this:
||| ```idris example
||| data Position : Type where
|||   MkPosition : (x : Integer) -> (y : Integer) -> Position
||| ```
||| If the tagging option is enabled, types with multiple constructors are
||| translated to a JSON object with a single key-value pair where the key is
||| constructor name (without the namespace) and the value is the JSON object
||| translated as if untagged.
||| Constructor arguments that are `Maybe` instances are omitted if `Nothing`
||| and converted without the constructor if `Just`. If you need to translate a
||| mandatory field that can be nullable use the `Null` type.
|||
||| @ opts The automatic derivation options.
||| @ name The name of the type to derive for. Can be without namespace if unambigous.
public export covering
deriveToJSON : (opts : JSONDeriveOpts) -> (name : Name) -> Elab ()
deriveToJSON opts n = do
    [(name, imp)] <- getType n
      | xs => fail "\{show n} must be in scope and unique. Possible referred types are: \{show (fst <$> xs)}"
    satName <- saturate imp (var name)
    -- FIXME: temporary name for debugging, should be converted to a name impossible to define from users
    -- and should not be exported, unless a specific option is enabled.
    let funName = UN $ Basic ("toJSON" ++ show (stripNs n))
    let objName = UN $ Basic ("__impl_toJSON" ++ show (stripNs n))
    conNames <- getCons name
    cons <- for conNames $ \n => do
      [(conName, conImpl)] <- getType n
        | _ => fail "\{show n} constructor must be in scope and unique"
      (bindNames, rhs) <- genRHS conImpl
      let rhs = if opts.tagged
                   then `(JObject $ [MkPair ~(primStr $ show $ stripNs n) (JObject $ catMaybes ~rhs)])
                   else `(JObject $ catMaybes ~rhs)
      let lhs = `(~(var funName) ~(applyBinds (var conName) (reverse bindNames)))
      pure $ patClause lhs rhs
    let funclaim = IClaim EmptyFC MW Export [Inline] (MkTy EmptyFC EmptyFC funName `(~satName -> JSON))
    let fundecl = IDef EmptyFC funName cons
    declare [funclaim, fundecl]
    [(ifName, _)] <- getType `{ToJSON}
      | _ => fail "ToJSON interface must be in scope and unique"
    [NS _ (DN _ ifCon)] <- getCons ifName
      | _ => fail "Interface constructor error"
    let retty = `(ToJSON ~satName)
    let objclaim = IClaim EmptyFC MW Export [Hint True, Inline] (MkTy EmptyFC EmptyFC objName retty)
    let objrhs = `(~(var ifCon) ~(var funName))
    let objdecl = IDef EmptyFC objName [(PatClause EmptyFC (var objName) objrhs)]
    declare [objclaim, objdecl]
  where
    genRHS : TTImp -> Elab (List Name, TTImp)
    genRHS (IPi _ _ ExplicitArg (Just n) `(Prelude.Types.Maybe ~argTy) retTy) = do
      (ns, rest) <- genRHS retTy
      let name = primStr $ fromMaybe (show n) $ lookup (show n) opts.renames
      pure (n :: ns, `(((MkPair ~name . toJSON) <$> ~(var n)) :: ~rest))
    genRHS (IPi _ _ ExplicitArg (Just n) argTy retTy) = do
      (ns, rest) <- genRHS retTy
      let name = primStr $ fromMaybe (show n) $ lookup (show n) opts.renames
      pure (n :: ns, `(Just (MkPair ~name (toJSON ~(var n))) :: ~rest))
    genRHS (IPi _ _ ExplicitArg Nothing _ _) = fail "All arguments must be explicitly named"
    genRHS (IPi _ _ _ _ _ retTy) = genRHS retTy
    genRHS _ = do
      -- Hack required, because if you quote directly opts.staticFields the elaborator introduces unsolved holes
      r <- traverse (\(k, v) => (k,) <$> quote v) opts.staticFields
      pure ([], foldr (\(k, v), acc => `(Just (MkPair ~(primStr k) ~v) :: ~acc)) `([]) r)

    applyBinds : TTImp -> List Name -> TTImp
    applyBinds = foldr (\n, acc => `(~acc ~(bindvar $ show n)))

||| Automatic derivation of `FromJSON` instances.
||| NOTE: all the fields in each constructor MUST be named, record already
||| comply but types declared with data must have constructors declared like
||| this:
||| ```idris example
||| data Position : Type where
|||   MkPosition : (x : Integer) -> (y : Integer) -> Position
||| ```
||| If the tagging option is enabled, types with multiple constructors are
||| translated to a JSON object with a single key-value pair where the key is
||| constructor name (without the namespace) and the value is the JSON object
||| translated as if untagged.
||| Constructor arguments that are `Maybe` instances are omitted if `Nothing`
||| and converted without the constructor if `Just`. If you need to translate a
||| mandatory field that can be nullable use the `Null` type.
|||
||| @ opts The automatic derivation options.
||| @ name The name of the type to derive for. Can be without namespace if unambigous.
public export covering
deriveFromJSON : (opts : JSONDeriveOpts) -> (name : Name) -> Elab ()
deriveFromJSON opts n = do
    [(name, imp)] <- getType n
      | xs => fail "\{show n} must be in scope and unique. Possible referred types are: \{show (fst <$> xs)}"
    satName <- saturate imp (var name)
    -- FIXME: temporary name for debugging, should be converted to a name impossible to define from users
    -- and should not be exported, unless a specific option is enabled.
    let funName = UN $ Basic ("fromJSON" ++ show (stripNs n))
    let objName = UN $ Basic ("__impl_fromJSON" ++ show (stripNs n))
    conNames <- getCons name
    argName <- genReadableSym "arg"
    cons <- for conNames $ \n => do
      [(conName, conImpl)] <- getType n
        | _ => fail "\{show n} constructor must be in scope and unique"
      args <- getArgs conImpl
      pure (conName, args)
    clauses <- traverse (\(cn, as) => genClause funName cn argName (reverse as)) cons
    let clauses = if opts.tagged
                     then (uncurry patClause <$> clauses)
                     else [patClause `(~(var funName) (JObject ~(bindvar $ show argName)))
                                     (foldl (\acc, x => `(~x <|> ~acc)) `(Nothing) (snd <$> clauses))]
    let funClaim = IClaim EmptyFC MW Export [Inline] (MkTy EmptyFC EmptyFC funName `(JSON -> Maybe ~satName))
    let funDecl = IDef EmptyFC funName (clauses ++ [patClause `(~(var funName) ~implicit') `(Nothing)])
    declare [funClaim, funDecl]
    [(ifName, _)] <- getType `{FromJSON}
      | _ => fail "FromJSON interface must be in scope and unique"
    [NS _ (DN _ ifCon)] <- getCons ifName
      | _ => fail "Interface constructor error"
    let retty = `(FromJSON ~satName)
    let objClaim = IClaim EmptyFC MW Export [Hint True, Inline] (MkTy EmptyFC EmptyFC objName retty)
    let objrhs = `(~(var ifCon) ~(var funName))
    let objDecl = IDef EmptyFC objName [(PatClause EmptyFC (var objName) objrhs)]
    declare [objClaim, objDecl]
  where
    getArgs : TTImp -> Elab (List (Name, TTImp))
    getArgs (IPi _ _ ExplicitArg (Just n) argTy retTy) = ((n, argTy) ::) <$> getArgs retTy
    getArgs (IPi _ _ ExplicitArg (Just n) argTy retTy) = ((n, argTy) ::) <$> getArgs retTy
    getArgs (IPi _ _ ExplicitArg Nothing _ _) = fail "All arguments must be explicitly named"
    getArgs (IPi _ _ _ _ _ retTy) = getArgs retTy
    getArgs _ = pure []

    genClause : Name -> Name -> Name -> List (Name, TTImp) -> Elab (TTImp, TTImp)
    genClause funName t m xs = do
      let lhs = `(~(var funName) (JObject [MkPair ~(primStr $ show $ stripNs t) (JObject ~(bindvar $ show m))]))
      let rhs = foldr (\(n, type), acc => let name = primStr $ fromMaybe (show n) $ lookup (show n) opts.renames in
                                              case type of
                                                   `(Prelude.Types.Maybe _) => `(~acc <*> (pure $ lookup ~name ~(var m) >>= fromJSON))
                                                   _ => `(~acc <*> (lookup ~name ~(var m) >>= fromJSON)))
                      `(pure ~(var t)) xs
      r <- traverse (\(k, v) => (k,) <$> quote v) opts.staticFields
      let rhs = foldr (\(k, v), acc => `((lookup ~(primStr k) ~(var m) >>= (guard . (== ~v))) *> ~acc)) rhs r
      pure (lhs, rhs)

||| Automatic derivation of `ToJSON` and `FromJSON` instances.
||| See `deriveToJSON` and `deriveFromJSON`.
|||
||| @ opts The automatic derivation options.
||| @ name The name of the type to derive for. Can be without namespace if unambigous.
public export covering
deriveJSON : (opts : JSONDeriveOpts) -> (name : Name) -> Elab ()
deriveJSON opts name = deriveToJSON opts name *> deriveFromJSON opts name

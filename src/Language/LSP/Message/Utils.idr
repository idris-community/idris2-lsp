||| Common types and instances for JSON interoperability.
|||
||| (C) The Idris Community, 2021
module Language.LSP.Message.Utils

import public Data.OneOf
import public JSON.Simple
import public JSON.Simple.Derive

%default total

||| Singleton type that models `null` in JSON.
public export
data Null = MkNull

public export
Eq Null where
  MkNull == MkNull = True

public export
Ord Null where
  compare MkNull MkNull = EQ

public export
Show Null where
  show MkNull = "null"

export
ToJSON Null where
  toJSON MkNull = JNull

export
FromJSON Null where
  fromJSON JNull = pure MkNull
  fromJSON _ = Left neutral

||| Converts an `UntaggedEither` value to the isomorphic value in `Either`.
public export
toEither : OneOf [a, b] -> Either a b
toEither (Here x)  = Left x
toEither (There (Here x)) = Right x

||| Converts an `Either` value to the isomorphic value in `UntaggedEither`.
public export
fromEither : Either a b -> OneOf [a, b]
fromEither (Left x)  = make x
fromEither (Right x) = make x

public export
toMaybe : OneOf [a, Null] -> Maybe a
toMaybe (Here x) = Just x
toMaybe (There (Here MkNull)) = Nothing

public export
fromMaybe : Maybe a -> OneOf [a, Null]
fromMaybe (Just x) = make x
fromMaybe Nothing = make MkNull

public export
ConstraintList : (Type -> Type) -> List Type -> Type
ConstraintList f [] = ()
ConstraintList f (x :: xs) = (f x, ConstraintList f xs)

export
ConstraintList ToJSON as => ToJSON (OneOf as) where
  toJSON (Here x) = toJSON x
  toJSON (There x) = toJSON x

private
Alternative (Either JSONErr) where
  empty = Left ([], "")
  (<|>) (Left e) (Left _) = Left e
  (<|>) (Left _) (Right v) = Right v
  (<|>) (Right v) _ = Right v

parseConstraints :
  {as : List Type} ->
  ConstraintList FromJSON as =>
  (v : JSON) ->
  Either (List JSONPathElement, String) (OneOf as)
parseConstraints {as = []} v = Left ([], "expected at least one element")
parseConstraints {as = (x :: xs)} @{(c, cs)} v
  = let t = (There <$> parseConstraints @{cs} v) <|> (OneOf.Here <$> fromJSON @{c} v)
    in t

export
{as : _} -> ConstraintList FromJSON as => FromJSON (OneOf as) where
  -- NOTE: The rightmost type is parsed first, since in the LSP specification
  --       the most specific type appears also rightmost.
  fromJSON {as} v = parseConstraints v

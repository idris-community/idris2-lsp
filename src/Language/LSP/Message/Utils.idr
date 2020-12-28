||| Common types and instances for JSON interoperability.
|||
||| (C) The Idris Community, 2020
module Language.LSP.Message.Utils

import Language.JSON
import Language.LSP.Message.Derive

%default total

-- TODO: Upstream this implementations.
public export
Foldable (Pair a) where
  foldr f init (_, input) = f input init
  null _ = False

public export
Traversable (Pair a) where
  traverse f (x, y) = (x,) <$> f y

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
  fromJSON _ = neutral

||| Data type isomorphic to `Either`, but is translated untagged from and to JSON.
public export
data UntaggedEither a b = Left a | Right b

export
(Eq a, Eq b) => Eq (UntaggedEither a b) where
  (Left x)  == (Left y)  = x == y
  (Right x) == (Right y) = x == y
  _ == _ = False

export
(Ord a, Ord b) => Ord (UntaggedEither a b) where
  compare (Left x)  (Left y)  = compare x y
  compare (Left _)  (Right _) = LT
  compare (Right _) (Left _)  = GT
  compare (Right x) (Right y) = compare x y

export
(Show a, Show b) => Show (UntaggedEither a b) where
  showPrec p (Left x)  = showCon p "Left" $ showArg x
  showPrec p (Right y) = showCon p "Right" $ showArg y

export
Bifunctor UntaggedEither where
  bimap f g (Left x) = Left (f x)
  bimap f g (Right x) = Right (g x)

infixr 0 .+.
||| Operator synonym for `UntaggedEither`.
public export
(.+.) : Type -> Type -> Type
(.+.) = UntaggedEither

||| Converts an `UntaggedEither` value to the isomorphic value in `Either`.
public export
toEither : UntaggedEither a b -> Either a b
toEither (Left x)  = Left x
toEither (Right x) = Right x

||| Converts an `Either` value to the isomorphic value in `UntaggedEither`.
public export
fromEither : Either a b -> UntaggedEither a b
fromEither (Left x)  = Left x
fromEither (Right x) = Right x

export
(ToJSON a, ToJSON b) => ToJSON (UntaggedEither a b) where
  toJSON (Left x)  = toJSON x
  toJSON (Right x) = toJSON x

export
(FromJSON a, FromJSON b) => FromJSON (UntaggedEither a b) where
  -- NOTE: The rightmost type is parsed first, since in the LSP specification
  --       the most specific type appears also rightmost.
  fromJSON v = (Right <$> fromJSON v) <|> (Left <$> fromJSON v)

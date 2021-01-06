||| Interfaces for converting values from/to JSON.
|||
||| (C) The Idris Community, 2021
module Language.JSON.Interfaces

import Data.SortedMap
import Data.Strings
import Language.JSON.Lexer
import Language.JSON.Parser

import Language.JSON

%default total

-- TODO: Maybe we should be using a more expressive type than Maybe.
||| A type that can be converted to JSON.
||| An example type and implementation is:
||| ```idris example
||| record Position where
|||   constructor MkPosition
|||   x : Integer
|||   y : Integer
|||
||| ToJSON Position where
|||   toJSON pos = JObject [("x", toJSON pos.x), ("y", toJSON pos.y)]
||| ```
public export
interface ToJSON a where
  toJSON : a -> JSON

||| A type that can be possibly converted from JSON.
||| An example type and implementation is:
||| ```idris example
||| record Position where
|||   constructor MkPosition
|||   x : Integer
|||   y : Integer
|||
||| FromJSON Position where
|||   fromJSON (JObject arg) = do
|||     x <- lookup "x" arg >>= fromJSON
|||     y <- lookup "y" arg >>= fromJSON
|||     pure $ MkPosition x y
|||   fromJSON _ = neutral
||| ```
public export
interface FromJSON a where
  fromJSON : JSON -> Maybe a

export
ToJSON JSON where
  toJSON = id

export
FromJSON JSON where
  fromJSON = pure

export
ToJSON Integer where
  toJSON = JNumber . cast

-- NOTE: In JSON all numbers are `Double`, for integers we should cast or fail
-- for invalid values?
export
FromJSON Integer where
  fromJSON (JNumber x) = pure (cast x)
  fromJSON _ = neutral

export
ToJSON Int where
  toJSON = JNumber . cast

export
FromJSON Int where
  fromJSON (JNumber x) = pure (cast x)
  fromJSON _ = neutral

export
ToJSON Bits8 where
  toJSON = JNumber . cast . cast {to = Int}

export
FromJSON Bits8 where
  fromJSON (JNumber x) = pure (fromInteger $ cast x)
  fromJSON _ = neutral

export
ToJSON Bits16 where
  toJSON = JNumber . cast . cast {to = Int}

export
FromJSON Bits16 where
  fromJSON (JNumber x) = pure (fromInteger $ cast x)
  fromJSON _ = neutral

export
ToJSON Bits32 where
  toJSON = JNumber . cast . cast {to = Int}

export
FromJSON Bits32 where
  fromJSON (JNumber x) = pure (fromInteger $ cast x)
  fromJSON _ = neutral

export
ToJSON Bits64 where
  toJSON = JNumber . cast . cast {to = Int}

export
FromJSON Bits64 where
  fromJSON (JNumber x) = pure (fromInteger $ cast x)
  fromJSON _ = neutral

export
ToJSON Double where
  toJSON = JNumber

export
FromJSON Double where
  fromJSON (JNumber x) = pure x
  fromJSON _ = neutral

export
ToJSON Nat where
  toJSON = JNumber . cast

export
FromJSON Nat where
  fromJSON (JNumber x) = pure (fromInteger $ cast x)
  fromJSON _ = neutral

export
ToJSON String where
  toJSON = JString

export
FromJSON String where
  fromJSON (JString s) = pure s
  fromJSON _ = neutral

export
ToJSON Char where
  toJSON = toJSON . Strings.singleton

export
FromJSON Char where
  fromJSON (JString s) = case strM s of
                              StrCons c "" => pure c
                              _ => neutral
  fromJSON _ = neutral

export
ToJSON Bool where
  toJSON = JBoolean

export
FromJSON Bool where
  fromJSON (JBoolean b) = pure b
  fromJSON _ = neutral

export
ToJSON a => ToJSON (Maybe a) where
  toJSON Nothing = JNull
  toJSON (Just x) = toJSON x

export
FromJSON a => FromJSON (Maybe a) where
  fromJSON JNull = pure Nothing
  fromJSON json @{impl} = pure <$> fromJSON @{impl} json

export
ToJSON a => ToJSON (List a) where
  toJSON = JArray . map toJSON

export
FromJSON a => FromJSON (List a) where
  fromJSON (JArray xs) = traverse fromJSON xs
  fromJSON _ = neutral

export
ToJSON () where
  toJSON () = JObject empty

export
FromJSON () where
  fromJSON (JObject xs) = guard $ null xs
  fromJSON _ = neutral

export
(ToJSON a, ToJSON b) => ToJSON (a, b) where
  toJSON (x, y) = JArray [toJSON x, toJSON y]

export
(FromJSON a, FromJSON b) => FromJSON (a, b) where
  fromJSON (JArray [x, y]) = (,) <$> fromJSON x <*> fromJSON y
  fromJSON _ = neutral

export
ToJSON v => ToJSON (SortedMap String v) where
  toJSON m = JObject (toList $ toJSON <$> m)

export
FromJSON v => FromJSON (SortedMap String v) where
  fromJSON (JObject xs) = fromList <$> traverse (\(k, v) => (k,) <$> fromJSON v) xs
  fromJSON _ = neutral

export
ToJSON a => ToJSON (Inf a) where
  toJSON x = toJSON x

export
FromJSON a => FromJSON (Inf a) where
  fromJSON @{impl} x = map (\x => Delay x) (fromJSON @{impl} x)

module Data.OneOf

import public Data.List.Elem
import public Data.List.Quantifiers

%default total

public export
data OneOf : List Type -> Type where
  Here : x -> OneOf (x :: xs)
  There : OneOf xs -> OneOf (x :: xs)

public export
make : Elem a as => a -> OneOf as
make x @{Here} = Here x
make x @{There _} = There (make x)

public export
TypeAt : (as : List Type) -> OneOf as -> Type
TypeAt (x :: _) (Here _) = x
TypeAt (_ :: xs) (There x) = TypeAt xs x

public export
Eliminators : (as : List Type) -> (r : Type) -> Type
Eliminators xs r = HList (map (\x => x -> r) xs)

public export
get : (o : OneOf as) -> TypeAt as o
get (Here x) = x
get (There x) = get x

public export
match : OneOf as -> Eliminators as r -> r
match (Here x) (f :: _) = f x
match (There x) (_ :: fs) = match x fs

public export
extend : OneOf as -> OneOf (as ++ bs)
extend (Here x) = Here x
extend (There x) = There (extend x)

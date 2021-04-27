module Data.HList

%default total

public export
data HList : List Type -> Type where
  Nil : HList []
  (::) : (a : x) -> HList xs -> HList (x :: xs)

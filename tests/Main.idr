module Main

import Data.String
import Test.Golden

%default covering

main : IO ()
main = runner []

some : (a, b, c : Nat) -> Nat
some x y z = ?todo -- ssone

record Rec where
  constructor MkRec
  name : String

tryIt : Rec -> Rec
tryIt (MkRec name) = ?tryIt_rhs_0

someOther : String -> Nat
someOther s = ?todo01

||| Some docs
caseIt : Bool -> Int
caseIt x = case x of
             False => ?caseIt_rhs_2
             True  => ?caseIt_rhs_3

data Foo = A
         | B

useRec : Rec -> Int
useRec x = ?useRec_rhs

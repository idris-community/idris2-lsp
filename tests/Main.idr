module Main

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

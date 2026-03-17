module Main

import Test.Golden

%default covering

main : IO ()
main = runner []

some : (a, b, c : Nat) -> Nat

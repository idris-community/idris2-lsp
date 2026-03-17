 module Test

double : Nat -> Nat
double n = n + n

triple : Nat -> Nat
triple n = double (double n)

main : IO ()
main = do
  let x = double 3
  let y = double 5
  printLn (triple x)

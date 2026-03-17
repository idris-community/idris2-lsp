module Server.FormatBench

import Data.IORef
import Data.List
import Data.Nat
import Data.String
import Language.LSP.Message.DocumentFormatting
import Server.ProcessMessage
import System.Clock

defaultOpts : FormattingOptions
defaultOpts = MkFormattingOptions
  { tabSize = 4
  , insertSpaces = True
  , trimTrailingWhitespace = Just True
  , insertFinalNewline = Just True
  , trimFinalNewlines = Nothing
  , other = []
  }

fmt : String -> String
fmt src = formatIdrisSource True True True [unpack "$"] defaultOpts src False

sampleSrc : String
sampleSrc = "module Main\n\nimport Data.List\n\nfoo : Nat -> Nat\nfoo x = x+1\n\nbar : String\nbar = \"hello\"\n"

elapsedNs : Clock Monotonic -> Clock Monotonic -> Integer
elapsedNs t0 t1 =
  (seconds t1 - seconds t0) * 1000000000 + (nanoseconds t1 - nanoseconds t0)

benchLoop : IORef Int -> Nat -> IO ()
benchLoop ref 0     = pure ()
benchLoop ref (S k) = do
  modifyIORef ref (+ cast (length (fmt sampleSrc)))
  benchLoop ref k

addBallast : IORef (List String) -> Nat -> IO ()
addBallast ref 0 = pure ()
addBallast ref (S k) = do
  modifyIORef ref (replicate 100 (chr (cast (mod (cast k) 26) + 65)) ::)
  addBallast ref k

showMs : Integer -> String
showMs ns = show (ns `div` 1000000)

main : IO ()
main = do
  putStrLn "=== Format Benchmark: GC Pressure ==="

  -- Clean
  ref1 <- newIORef (the Int 0)
  t0 <- clockTime Monotonic
  benchLoop ref1 50
  t1 <- clockTime Monotonic
  let clean = elapsedNs t0 t1
  putStrLn "  Clean  : \{showMs clean} ms (50 iters)"

  -- 1 MB ballast
  b1 <- newIORef (the (List String) [])
  addBallast b1 10000
  ref2 <- newIORef (the Int 0)
  t2 <- clockTime Monotonic
  benchLoop ref2 50
  t3 <- clockTime Monotonic
  let loaded1 = elapsedNs t2 t3
  putStrLn "  1 MB   : \{showMs loaded1} ms (50 iters, 10k strings)"

  -- 10 MB ballast
  b2 <- newIORef (the (List String) [])
  addBallast b2 100000
  ref3 <- newIORef (the Int 0)
  t4 <- clockTime Monotonic
  benchLoop ref3 50
  t5 <- clockTime Monotonic
  let loaded10 = elapsedNs t4 t5
  putStrLn "  10 MB  : \{showMs loaded10} ms (50 iters, 100k strings)"

  -- Summary
  when (clean > 0) $ do
    putStrLn ""
    putStrLn "  1 MB  vs clean: \{show (loaded1 * 100 `div` clean)}%"
    putStrLn "  10 MB vs clean: \{show (loaded10 * 100 `div` clean)}%"

  ignore $ readIORef b1
  ignore $ readIORef b2

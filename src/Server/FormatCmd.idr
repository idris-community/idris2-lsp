||| Standalone formatter binary.
||| Reads Idris2 source from stdin, writes formatted output to stdout.
||| Runs in a small process with no GC pressure from the Idris2 compiler context.
module Server.FormatCmd

import Data.List
import Data.List1
import Data.String
import Language.LSP.Message.DocumentFormatting
import Server.ProcessMessage
import System
import System.File

readStdin : IO String
readStdin = do
  Right content <- fRead stdin
    | Left err => do putStrLn "Error reading stdin" >> exitWith (ExitFailure 1)
                     pure ""
  pure content

-- Sort operator strings longest-first so multi-char ops match before prefixes.
sortOpsLongestFirst : List String -> List (List Char)
sortOpsLongestFirst = map unpack . sortBy (\a, b => compare (length b) (length a))

main : IO ()
main = do
  args <- getArgs
  let tabSize   = parseArg "--tab-size" args 4
  let spaces    = not (elem "--no-insert-spaces" args)
  let trimWS    = not (elem "--no-trim" args)
  let newline   = not (elem "--no-final-newline" args)
  let literate  = elem "--literate" args
  let rangeOnly = elem "--range" args
  let noStruct  = elem "--no-structural" args
  let noAlign   = elem "--no-alignment" args
  let noPerLine = elem "--no-per-line" args
  let noOps     = elem "--no-ops" args
  -- --ops "op1,op2,..." overrides the default operator list; --no-ops disables spacing
  let opsArg    = parseStrArg "--ops" args ""
  let ops : List (List Char) = if noOps then []
            else if opsArg == "" then sortOpsLongestFirst ["$"]
            else sortOpsLongestFirst (filter (\s => not (null s)) (forget (split (== ',') opsArg)))

  let opts = MkFormattingOptions
        { tabSize = cast tabSize
        , insertSpaces = spaces
        , trimTrailingWhitespace = if trimWS then Just True else Nothing
        , insertFinalNewline = if newline then Just True else Nothing
        , trimFinalNewlines = Nothing
        , other = []
        }

  src <- readStdin

  let result = if rangeOnly
        then formatIdrisSourceRange (not noPerLine) ops opts src literate
        else formatIdrisSource (not noPerLine) (not noStruct) (not noAlign) ops opts src literate

  _ <- fPutStr stdout result
  pure ()
  where
    parseArg : String -> List String -> Integer -> Integer
    parseArg flag [] def = def
    parseArg flag (x :: v :: rest) def =
      if x == flag then cast (cast {to = Integer} v)
                   else parseArg flag (v :: rest) def
    parseArg flag (_ :: rest) def = parseArg flag rest def

    parseStrArg : String -> List String -> String -> String
    parseStrArg flag [] def = def
    parseStrArg flag (x :: v :: rest) def =
      if x == flag then v else parseStrArg flag (v :: rest) def
    parseStrArg flag (_ :: rest) def = parseStrArg flag rest def

module Server.FormatTest

import Language.LSP.Message.DocumentFormatting
import Server.ProcessMessage

defaultOpts : FormattingOptions
defaultOpts = MkFormattingOptions
  { tabSize                = 4
  , insertSpaces           = True
  , trimTrailingWhitespace = Just True
  , insertFinalNewline     = Just True
  , trimFinalNewlines      = Nothing
  , other                  = []
  }

check : String -> String -> String -> IO ()
check name expected got =
  if expected == got
     then putStrLn "  \{name}"
     else do
       putStrLn "FAIL \{name}"
       putStrLn "  expected: \{show expected}"
       putStrLn "  got:      \{show got}"

fmt : String -> String
fmt src = formatIdrisSource defaultOpts src False

section : String -> IO ()
section name = putStrLn "\n-- \{name} --"

main : IO ()
main = do
  section "Trailing whitespace"
  check "trims trailing spaces"
    "foo : Nat\n"
    (fmt "foo : Nat   \n")
  check "trims on indented line"
    "  bar x =\n"
    (fmt "  bar x =  \n")

  section "Interior spaces"
  check "collapses multiple interior spaces"
    "foo : String -> String\n"
    (fmt "foo :          String -> String\n")
  check "preserves single interior space"
    "foo : Nat\n"
    (fmt "foo : Nat\n")
  check "preserves indentation"
    "  foo x = x\n"
    (fmt "  foo x = x\n")

  section "Comma spacing"
  check "adds space after comma in tuple"
    "f (a, b, c)\n"
    (fmt "f (a,b,c)\n")
  check "adds space after comma in list"
    "[1, 2, 3]\n"
    (fmt "[1,2,3]\n")
  check "adds space after comma in function params with shared type"
    "some : (a, b, c : Nat) -> Nat\n"
    (fmt "some : (a,b,c : Nat) -> Nat\n")
  check "does not double-space already spaced commas"
    "f (a, b)\n"
    (fmt "f (a, b)\n")
  check "does not add space before closing paren"
    "f (a, b)\n"
    (fmt "f (a,b)\n")
  check "does not mangle char literal comma"
    "x = ','\n"
    (fmt "x = ','\n")
  check "does not mangle comma inside string"
    "x = \"a,b\"\n"
    (fmt "x = \"a,b\"\n")
  check "does not mangle escaped quote char literal"
    "x = '\\''\n"
    (fmt "x = '\\''\n")

  section "Colon spacing"
  check "adds space after colon"
    "foo : Int\n"
    (fmt "foo :Int\n")
  check "adds space before colon"
    "foo : Int\n"
    (fmt "foo: Int\n")
  check "adds space both sides of colon"
    "foo : Int\n"
    (fmt "foo:Int\n")
  check "does not double-space already correct colon"
    "foo : Int\n"
    (fmt "foo : Int\n")
  check "does not mangle ::"
    "x :: xs\n"
    (fmt "x :: xs\n")
  check "does not mangle := record update"
    "{ field := 42 }\n"
    (fmt "{ field := 42 }\n")
  check "adds space around colon in parameter list"
    "some : (a, b, c : Nat) -> Nat\n"
    (fmt "some : (a, b, c:Nat) -> Nat\n")
  check "does not mangle colon inside string"
    "x = \"a:b\"\n"
    (fmt "x = \"a:b\"\n")
  check "does not mangle colon inside char literal"
    "x = ':'\n"
    (fmt "x = ':'\n")

  section "Comment spacing"
  check "adds space after -- in comment"
    "foo -- a comment\n"
    (fmt "foo --a comment\n")
  check "does not modify already-spaced comment"
    "foo -- a comment\n"
    (fmt "foo -- a comment\n")
  check "does not modify --- divider"
    "---\n"
    (fmt "---\n")
  check "does not add space after --| doc style"
    "--|doc\n"
    (fmt "--|doc\n")
  check "does not mangle -- inside string"
    "x = \"--\"\n"
    (fmt "x = \"--\"\n")

  section "Blank line normalization"
  check "collapses multiple blank lines"
    "foo : Nat\n\nbar : String\n"
    (fmt "foo : Nat\n\n\nbar : String\n")
  check "removes leading blank lines"
    "foo : Nat\n"
    (fmt "\n\nfoo : Nat\n")
  check "removes trailing blank lines"
    "foo : Nat\n"
    (fmt "foo : Nat\n\n")

  section "Module and import spacing"
  check "blank line after module declaration"
    "module Main\n\nfoo : Nat\n"
    (fmt "module Main\nfoo : Nat\n")
  check "blank line after import block"
    "module Main\n\nimport Data.List\n\nfoo : Nat\n"
    (fmt "module Main\n\nimport Data.List\nfoo : Nat\n")

  putStrLn "\nDone."

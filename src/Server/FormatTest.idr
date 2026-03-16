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

fmtRange : String -> String
fmtRange src = formatIdrisSourceRange defaultOpts src False

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

  section "Equals spacing"
  check "adds spaces around = in definition"
    "foo x = x\n"
    (fmt "foo x=x\n")
  check "does not double-space already correct ="
    "foo x = x\n"
    (fmt "foo x = x\n")
  check "adds spaces around =="
    "x == y\n"
    (fmt "x==y\n")
  check "does not mangle <="
    "x <= y\n"
    (fmt "x <= y\n")
  check "does not mangle >="
    "x >= y\n"
    (fmt "x >= y\n")
  check "does not mangle /="
    "x /= y\n"
    (fmt "x /= y\n")
  check "does not mangle :="
    "{ field := 42 }\n"
    (fmt "{ field := 42 }\n")
  check "does not mangle => (arrow already handled)"
    "Just x => x\n"
    (fmt "Just x => x\n")
  check "does not mangle = inside string"
    "x = \"a=b\"\n"
    (fmt "x = \"a=b\"\n")
  check "does not mangle = inside char literal"
    "x = '='\n"
    (fmt "x = '='\n")
  check "adds spaces around = in let binding"
    "let x = 5\n"
    (fmt "let x=5\n")

  section "Brace spacing"
  check "adds spaces inside braces in record literal"
    "{ field = val }\n"
    (fmt "{field = val}\n")
  check "does not double-space already correct braces"
    "{ field = val }\n"
    (fmt "{ field = val }\n")
  check "adds space after { only"
    "{ field = val }\n"
    (fmt "{ field = val}\n")
  check "adds space before } only"
    "{ field = val }\n"
    (fmt "{field = val }\n")
  check "does not mangle empty braces"
    "{}\n"
    (fmt "{}\n")
  check "does not mangle block comment {- -}"
    "{- comment -}\n"
    (fmt "{- comment -}\n")
  check "does not mangle braces inside string"
    "x = \"{field}\"\n"
    (fmt "x = \"{field}\"\n")

  section "Pipe and bind spacing"
  check "adds spaces around | in sum type"
    "data Foo = A | B | C\n"
    (fmt "data Foo = A|B|C\n")
  check "does not double-space already correct |"
    "data Foo = A | B\n"
    (fmt "data Foo = A | B\n")
  check "adds spaces around || logical or"
    "x || y\n"
    (fmt "x||y\n")
  check "does not mangle ||| doc comment"
    "||| This is a doc comment\n"
    (fmt "||| This is a doc comment\n")
  check "adds spaces around <- bind"
    "x <- getLine\n"
    (fmt "x<-getLine\n")
  check "does not double-space already correct <-"
    "x <- getLine\n"
    (fmt "x <- getLine\n")
  check "does not mangle | inside string"
    "x = \"a|b\"\n"
    (fmt "x = \"a|b\"\n")
  check "does not mangle | inside char literal"
    "x = '|'\n"
    (fmt "x = '|'\n")
  check "adds spaces around && logical and"
    "x && y\n"
    (fmt "x&&y\n")
  check "does not double-space already correct &&"
    "x && y\n"
    (fmt "x && y\n")
  check "adds spaces around ++ concatenation"
    "xs ++ ys\n"
    (fmt "xs++ys\n")
  check "does not double-space already correct ++"
    "xs ++ ys\n"
    (fmt "xs ++ ys\n")
  check "does not mangle && inside string"
    "x = \"a&&b\"\n"
    (fmt "x = \"a&&b\"\n")
  check "does not mangle ++ inside string"
    "x = \"a++b\"\n"
    (fmt "x = \"a++b\"\n")

  section "Arrow spacing"
  check "adds spaces around ->"
    "Nat -> Nat\n"
    (fmt "Nat->Nat\n")
  check "adds space before ->"
    "Nat -> Nat\n"
    (fmt "Nat ->Nat\n")
  check "adds space after ->"
    "Nat -> Nat\n"
    (fmt "Nat-> Nat\n")
  check "does not double-space already correct ->"
    "Nat -> Nat\n"
    (fmt "Nat -> Nat\n")
  check "adds spaces around => in case"
    "Just x => x\n"
    (fmt "Just x=>x\n")
  check "adds spaces around => in lambda"
    "\\x => x\n"
    (fmt "\\x=>x\n")
  check "does not mangle -> inside string"
    "x = \"a->b\"\n"
    (fmt "x = \"a->b\"\n")
  check "does not mangle -> inside char literal"
    "x = '>'\n"
    (fmt "x = '>'\n")
  check "handles chain of arrows"
    "Nat -> Nat -> Nat\n"
    (fmt "Nat->Nat->Nat\n")

  section "Dollar spacing"
  check "adds spaces around $ operator"
    "f $ g x\n"
    (fmt "f$g x\n")
  check "does not double-space already correct $"
    "f $ g x\n"
    (fmt "f $ g x\n")
  check "does not mangle $ inside string"
    "x = \"a$b\"\n"
    (fmt "x = \"a$b\"\n")
  check "does not mangle $ inside char literal"
    "x = '$'\n"
    (fmt "x = '$'\n")

  section "Arithmetic spacing"
  check "adds spaces around +"
    "x + y\n"
    (fmt "x+y\n")
  check "does not double-space already correct +"
    "x + y\n"
    (fmt "x + y\n")
  check "does not mangle ++"
    "xs ++ ys\n"
    (fmt "xs++ys\n")
  check "does not mangle - (skipped: unary ambiguity)"
    "x = -1\n"
    (fmt "x = -1\n")
  check "adds spaces around *"
    "x * y\n"
    (fmt "x*y\n")
  check "does not double-space already correct *"
    "x * y\n"
    (fmt "x * y\n")
  check "adds spaces around /"
    "x / y\n"
    (fmt "x/y\n")
  check "does not double-space already correct /"
    "x / y\n"
    (fmt "x / y\n")
  check "does not mangle /="
    "x /= y\n"
    (fmt "x /= y\n")
  check "does not mangle + inside string"
    "x = \"a+b\"\n"
    (fmt "x = \"a+b\"\n")
  check "adds spaces around <"
    "x < y\n"
    (fmt "x<y\n")
  check "does not double-space already correct <"
    "x < y\n"
    (fmt "x < y\n")
  check "does not mangle <-"
    "x <- getLine\n"
    (fmt "x<-getLine\n")
  check "adds spaces around <="
    "x <= y\n"
    (fmt "x<=y\n")
  check "adds spaces around >"
    "x > y\n"
    (fmt "x>y\n")
  check "does not double-space already correct >"
    "x > y\n"
    (fmt "x > y\n")
  check "adds spaces around >="
    "x >= y\n"
    (fmt "x>=y\n")
  check "does not mangle < inside string"
    "x = \"a<b\"\n"
    (fmt "x = \"a<b\"\n")
  check "does not mangle > inside string"
    "x = \"a>b\"\n"
    (fmt "x = \"a>b\"\n")

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

  section "Range formatting (no structural normalization)"
  check "applies spacing rules to range"
    "foo : Nat\n"
    (fmtRange "foo:Nat\n")
  check "does not collapse blank lines in range"
    "foo : Nat\n\n\nbar : String\n"
    (fmtRange "foo : Nat\n\n\nbar : String\n")
  check "does not remove leading blank lines in range"
    "\n\nfoo : Nat\n"
    (fmtRange "\n\nfoo : Nat\n")
  check "does not add blank after module declaration in range"
    "module Main\nfoo : Nat\n"
    (fmtRange "module Main\nfoo : Nat\n")
  check "does not add blank after import block in range"
    "import Data.List\nfoo : Nat\n"
    (fmtRange "import Data.List\nfoo : Nat\n")
  check "still trims trailing whitespace in range"
    "foo : Nat\n"
    (fmtRange "foo : Nat   \n")
  check "still spaces operators in range"
    "x + y\n"
    (fmtRange "x+y\n")

  putStrLn "\nDone."

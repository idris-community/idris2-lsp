module Server.FormatTest

import Language.LSP.Message.DocumentFormatting
import Server.ProcessMessage

defaultOpts : FormattingOptions
defaultOpts = MkFormattingOptions
  { tabSize = 4
  , insertSpaces = True
  , trimTrailingWhitespace = Just True
  , insertFinalNewline = Just True
  , trimFinalNewlines = Nothing
  , other = []
  }

check : String -> String -> String -> IO ()
check name expected got =
  if expected == got
     then putStrLn "  \{name}"
     else do
       putStrLn "FAIL \{name}"
       putStrLn "  expected: \{show expected}"
       putStrLn "  got:      \{show got}"

defaultOps : List (List Char)
defaultOps = [unpack "$"]

-- Arithmetic ops for testing configurable operator spacing
arithmeticOps : List (List Char)
arithmeticOps = map unpack ["<=", ">=", "++", "+", "*", "/"]

fmt : String -> String
fmt src = formatIdrisSource True True True defaultOps defaultOpts src False

fmtOps : List (List Char) -> String -> String
fmtOps ops src = formatIdrisSource True True True ops defaultOpts src False

fmtRange : String -> String
fmtRange src = formatIdrisSourceRange True defaultOps defaultOpts src False

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
  check "single | left as-is (custom operators)"
    "data Foo = A|B|C\n"
    (fmt "data Foo = A|B|C\n")
  check "preserves already spaced |"
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

  section "Trailing comma removal"
  check "removes trailing comma in record literal"
    "{ field = val }\n"
    (fmt "{ field = val, }\n")
  check "removes trailing comma without space before }"
    "{ field = val }\n"
    (fmt "{ field = val,}\n")
  check "removes trailing comma in record update"
    "{ x := 1, y := 2 }\n"
    (fmt "{ x := 1, y := 2, }\n")
  check "does not remove non-trailing comma"
    "{ x = 1, y = 2 }\n"
    (fmt "{ x = 1, y = 2 }\n")
  check "does not mangle comma inside string"
    "x = \"a,}\"\n"
    (fmt "x = \"a,}\"\n")
  check "does not mangle empty braces"
    "{}\n"
    (fmt "{}\n")

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

  section "Arithmetic spacing (configurable, off by default)"
  -- These operators are NOT spaced by default; they require explicit configuration.
  check "adds spaces around + (with arithmetic ops)"
    "x + y\n"
    (fmtOps arithmeticOps "x+y\n")
  check "does not space + by default (not in default ops)"
    "x+y\n"
    (fmt "x+y\n")
  check "does not double-space already correct +"
    "x + y\n"
    (fmtOps arithmeticOps "x + y\n")
  check "does not mangle ++"
    "xs ++ ys\n"
    (fmt "xs++ys\n")
  check "does not mangle - (skipped: unary ambiguity)"
    "x = -1\n"
    (fmt "x = -1\n")
  check "adds spaces around * (with arithmetic ops)"
    "x * y\n"
    (fmtOps arithmeticOps "x*y\n")
  check "does not space * by default"
    "x*y\n"
    (fmt "x*y\n")
  check "does not double-space already correct *"
    "x * y\n"
    (fmtOps arithmeticOps "x * y\n")
  check "adds spaces around / (with arithmetic ops)"
    "x / y\n"
    (fmtOps arithmeticOps "x/y\n")
  check "does not space / by default"
    "x/y\n"
    (fmt "x/y\n")
  check "does not double-space already correct /"
    "x / y\n"
    (fmtOps arithmeticOps "x / y\n")
  check "does not mangle /="
    "x /= y\n"
    (fmt "x /= y\n")
  check "does not mangle + inside string"
    "x = \"a+b\"\n"
    (fmt "x = \"a+b\"\n")
  check "single < left as-is (custom operators)"
    "x<y\n"
    (fmt "x<y\n")
  check "preserves already spaced <"
    "x < y\n"
    (fmt "x < y\n")
  check "does not mangle <-"
    "x <- getLine\n"
    (fmt "x<-getLine\n")
  check "adds spaces around <= (with arithmetic ops)"
    "x <= y\n"
    (fmtOps arithmeticOps "x<=y\n")
  check "does not space <= by default"
    "x<=y\n"
    (fmt "x<=y\n")
  check "single > left as-is (custom operators)"
    "x>y\n"
    (fmt "x>y\n")
  check "preserves already spaced >"
    "x > y\n"
    (fmt "x > y\n")
  check "adds spaces around >= (with arithmetic ops)"
    "x >= y\n"
    (fmtOps arithmeticOps "x>=y\n")
  check "does not space >= by default"
    "x>=y\n"
    (fmt "x>=y\n")
  check "does not mangle < inside string"
    "x = \"a<b\"\n"
    (fmt "x = \"a<b\"\n")
  check "does not mangle > inside string"
    "x = \"a>b\"\n"
    (fmt "x = \"a>b\"\n")
  -- Verify compound operators not broken by arithmetic ops config
  check "does not mangle <$> with arithmetic ops"
    "val = f <$> xs\n"
    (fmtOps arithmeticOps "val = f <$> xs\n")
  check "does not mangle *> with arithmetic ops"
    "seq = a *> b\n"
    (fmtOps arithmeticOps "seq = a *> b\n")
  check "does not mangle </> with arithmetic ops"
    "path = a </> b\n"
    (fmtOps arithmeticOps "path = a </> b\n")

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
  check "sorts imports alphabetically"
    "module Main\n\nimport Data.List\nimport Data.String\nimport Data.Vect\n\nfoo : Nat\n"
    (fmt "module Main\n\nimport Data.Vect\nimport Data.List\nimport Data.String\nfoo : Nat\n")
  check "preserves already-sorted imports"
    "module Main\n\nimport Data.List\nimport Data.String\n\nfoo : Nat\n"
    (fmt "module Main\n\nimport Data.List\nimport Data.String\nfoo : Nat\n")
  check "sorts imports with blank lines in block"
    "module Main\n\nimport Data.List\nimport Data.String\nimport Data.Vect\n\nfoo : Nat\n"
    (fmt "module Main\n\nimport Data.Vect\nimport Data.List\n\nimport Data.String\nfoo : Nat\n")
  check "sorts imports without module declaration"
    "import Data.List\nimport Data.Vect\n\nfoo : Nat\n"
    (fmt "import Data.Vect\nimport Data.List\nfoo : Nat\n")
  check "deduplicates imports"
    "module Main\n\nimport Data.List\nimport Data.Vect\n\nfoo : Nat\n"
    (fmt "module Main\n\nimport Data.List\nimport Data.List\nimport Data.Vect\nfoo : Nat\n")
  check "deduplicates imports after sorting"
    "module Main\n\nimport Data.List\nimport Data.Vect\n\nfoo : Nat\n"
    (fmt "module Main\n\nimport Data.Vect\nimport Data.List\nimport Data.List\nfoo : Nat\n")

  section "Type signature and definition cohesion"
  check "removes blank between sig and def"
    "foo : Nat\nfoo = 42\n"
    (fmt "foo : Nat\n\nfoo = 42\n")
  check "removes blank between sig and def with args"
    "foo : Nat -> Nat\nfoo x = x + 1\n"
    (fmt "foo : Nat -> Nat\n\nfoo x = x + 1\n")
  check "removes blank between sig and def with pattern"
    "foo : Maybe Nat -> Nat\nfoo (Just x) = x\n"
    (fmt "foo : Maybe Nat -> Nat\n\nfoo (Just x) = x\n")
  check "removes blank between sig and def with implicit"
    "foo : { n : Nat } -> Nat\nfoo { n } = n\n"
    (fmt "foo : {n : Nat} -> Nat\n\nfoo {n} = n\n")
  check "does not remove blank between unrelated lines"
    "foo : Nat\n\nbar : Nat\n"
    (fmt "foo : Nat\n\nbar : Nat\n")
  check "does not remove blank after data declaration"
    "data Foo : Type\n\nfoo : Foo\n"
    (fmt "data Foo : Type\n\nfoo : Foo\n")
  check "preserves already-adjacent sig and def"
    "foo : Nat\nfoo = 42\n"
    (fmt "foo : Nat\nfoo = 42\n")
  check "handles indented sig and def in where block"
    "main : IO ()\nmain = pure ()\n  where\n    helper : Nat\n    helper = 0\n"
    (fmt "main : IO ()\nmain = pure ()\n  where\n    helper : Nat\n\n    helper = 0\n")

  section "Doc comment attachment"
  check "removes blank between doc comment and sig"
    "||| A number\nfoo : Nat\nfoo = 42\n"
    (fmt "||| A number\n\nfoo : Nat\nfoo = 42\n")
  check "removes blank between doc comment and def"
    "||| A number\nfoo : Nat\nfoo = 42\n"
    (fmt "||| A number\n\nfoo : Nat\n\nfoo = 42\n")
  check "removes blank between consecutive doc comments"
    "||| Line one\n||| Line two\nfoo : Nat\n"
    (fmt "||| Line one\n\n||| Line two\n\nfoo : Nat\n")
  check "adds blank before doc comment when needed"
    "bar = 0\n\n||| A number\nfoo : Nat\n"
    (fmt "bar = 0\n||| A number\nfoo : Nat\n")
  check "preserves already-attached doc comment"
    "||| A number\nfoo : Nat\nfoo = 42\n"
    (fmt "||| A number\nfoo : Nat\nfoo = 42\n")

  section "Blank line between top-level definitions"
  check "adds blank between consecutive defs"
    "foo : Nat\nfoo = 42\n\nbar : Nat\nbar = 0\n"
    (fmt "foo : Nat\nfoo = 42\nbar : Nat\nbar = 0\n")
  check "does not double blank already-separated defs"
    "foo : Nat\nfoo = 42\n\nbar : Nat\nbar = 0\n"
    (fmt "foo : Nat\nfoo = 42\n\nbar : Nat\nbar = 0\n")
  check "adds blank after multi-line def"
    "foo x =\n  x + 1\n\nbar : Nat\n"
    (fmt "foo x =\n  x + 1\nbar : Nat\n")
  check "does not add blank between sig and def"
    "foo : Nat\nfoo = 42\n"
    (fmt "foo : Nat\nfoo = 42\n")
  check "adds blank between def and next sig"
    "foo = 42\n\nbar : Nat\nbar = 0\n"
    (fmt "foo = 42\nbar : Nat\nbar = 0\n")
  check "adds blank after data declaration"
    "data Foo = A | B\n\nfoo : Foo\nfoo = A\n"
    (fmt "data Foo = A | B\nfoo : Foo\nfoo = A\n")

  section "Case arm alignment"
  check "aligns => in case arms"
    "case x of\n  Just y  => y\n  Nothing => 0\n"
    (fmt "case x of\n  Just y => y\n  Nothing => 0\n")
  check "does not change already-aligned arms"
    "case x of\n  Just y  => y\n  Nothing => 0\n"
    (fmt "case x of\n  Just y  => y\n  Nothing => 0\n")
  check "aligns three arms"
    "case x of\n  Just (Just y) => y\n  Just Nothing  => 0\n  Nothing       => 1\n"
    (fmt "case x of\n  Just (Just y) => y\n  Just Nothing => 0\n  Nothing => 1\n")
  check "does not align top-level => (must be indented)"
    "Just x => x\n"
    (fmt "Just x => x\n")
  check "does not align arms separated by blank line"
    "case x of\n  Just y => y\n\n  Nothing => 0\n"
    (fmt "case x of\n  Just y => y\n\n  Nothing => 0\n")
  check "does not mangle => inside string"
    "x = \"a=>b\"\n"
    (fmt "x = \"a=>b\"\n")

  section "Definition = alignment"
  check "aligns simple function clauses"
    "foo x   = x + 1\nfoo bar = bar\n"
    (fmt "foo x = x + 1\nfoo bar = bar\n")
  check "aligns where-block bindings"
    "  x   = 1\n  foo = 2\n"
    (fmt "  x = 1\n  foo = 2\n")
  check "single definition not padded"
    "foo x = x\n"
    (fmt "foo x = x\n")
  check "does not align across blank line"
    "foo x = x\n\nbar = 1\n"
    (fmt "foo x = x\n\nbar = 1\n")
  check "does not align data constructor lines"
    "data Foo = Bar | Baz\n"
    (fmt "data Foo = Bar | Baz\n")
  check "does not mangle == operator"
    "x = (a == b)\n"
    (fmt "x = (a == b)\n")
  check "does not mangle = inside string"
    "x = \"a=b\"\n"
    (fmt "x = \"a=b\"\n")
  check "aligns mixed-length clauses"
    "f 1         = \"one\"\nf 2         = \"two\"\nf something = \"other\"\n"
    (fmt "f 1 = \"one\"\nf 2 = \"two\"\nf something = \"other\"\n")

  section "Record field = alignment"
  check "aligns two fields"
    "  { field1    = val1\n  , longField = val2\n"
    (fmt "  { field1 = val1\n  , longField = val2\n")
  check "aligns three fields"
    "  { a   = 1\n  , bb  = 2\n  , ccc = 3\n"
    (fmt "  { a = 1\n  , bb = 2\n  , ccc = 3\n")
  check "already aligned is unchanged"
    "  { field1    = val1\n  , longField = val2\n"
    (fmt "  { field1    = val1\n  , longField = val2\n")
  check "single field not padded"
    "  { field = val\n"
    (fmt "  { field = val\n")
  check "does not mangle == in record field"
    "  { field = (a == b)\n"
    (fmt "  { field = (a == b)\n")
  check "does not mangle => in record field"
    "  { field = (\\x => x)\n"
    (fmt "  { field = (\\x => x)\n")
  check "does not align across blank line"
    "  { field = val\n\n  , other = 2\n"
    (fmt "  { field = val\n\n  , other = 2\n")
  check "does not align single-line record"
    "{ field = val }\n"
    (fmt "{ field = val }\n")

  section "Record definition : alignment"
  check "aligns record fields"
    "record Foo where\n  constructor MkFoo\n  name     : String\n  longName : Nat\n"
    (fmt "record Foo where\n  constructor MkFoo\n  name : String\n  longName : Nat\n")
  check "aligns three fields"
    "record Foo where\n  constructor MkFoo\n  a   : Nat\n  bb  : String\n  ccc : Bool\n"
    (fmt "record Foo where\n  constructor MkFoo\n  a : Nat\n  bb : String\n  ccc : Bool\n")
  check "does not align constructor line"
    "record Foo where\n  constructor MkFoo\n  name : String\n"
    (fmt "record Foo where\n  constructor MkFoo\n  name : String\n")
  check "does not align top-level sigs"
    "foo : Nat\n\nbar : String\n"
    (fmt "foo : Nat\nbar : String\n")

  section "Data constructor | alignment"
  check "aligns | with = (two constructors)"
    "data Foo = Bar\n         | Baz\n"
    (fmt "data Foo = Bar\n  | Baz\n")
  check "aligns | with = (three constructors)"
    "data Foo = Bar\n         | Baz\n         | Qux\n"
    (fmt "data Foo = Bar\n  | Baz\n  | Qux\n")
  check "already aligned is unchanged"
    "data Foo = Bar\n         | Baz\n"
    (fmt "data Foo = Bar\n         | Baz\n")
  check "single-line data unchanged"
    "data Foo = Bar | Baz\n"
    (fmt "data Foo = Bar | Baz\n")
  check "data without = (GADT style) unchanged"
    "data Foo : Type where\n"
    (fmt "data Foo : Type where\n")
  check "blank line breaks alignment group"
    "data Foo = Bar\n\n  | Baz\n"
    (fmt "data Foo = Bar\n\n  | Baz\n")
  check "does not touch | in case expression"
    "data Foo = Bar\n         | Baz\n\nf x =\n  case x of\n    Bar => 1\n    Baz => 2\n"
    (fmt "data Foo = Bar\n  | Baz\n\nf x =\n  case x of\n    Bar => 1\n    Baz => 2\n")

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
  check "still spaces $ operator in range"
    "f $ g\n"
    (fmtRange "f$g\n")
  check "does not space + in range (not in default ops)"
    "x+y\n"
    (fmtRange "x+y\n")

  putStrLn "\nDone."

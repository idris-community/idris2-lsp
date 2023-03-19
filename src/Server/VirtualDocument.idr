module Server.VirtualDocument

-- The VirtualDocument is the version of the document that the LSP server maintains
-- based on the incoming DidChange events. It should mirror the actively edited but
-- unsaved content of the opened documents.

import Data.Nat
import Data.List
import Data.String
import Data.OneOf
import Language.LSP.Message.Location
import Language.LSP.Message.TextDocument
import Decidable.Equality
import Data.DPair
import Control.Function
import Syntax.PreorderReasoning
import Data.Void

%default total


-- As the edits come in incremental change, the virtual document must be
-- updated accordingly. The string needs to be split in a particular way,
-- as detailed below:
--
-- Cut the string in half.
--
-- The `rangeSplit` cuts the string into three parts based on a Range.
--
-- In a Range based edit, the client sends a text that must replace the part
-- within the Range found in the edit request. The Range consists of a start
-- and an end position. Positions consist of a line and a character index.
--
-- Although the Position is encoded with line and char index, the file is
-- represented as a String, in which a natural number indexes characters.
-- The Position must be mapped to the string index-based encoding, which
-- can be done via a string traversal. When a newline character is found,
-- the line index is decreased. When the line index reaches zero, the character
-- index is decreased. When the character index reaches zero, the string index of
-- the Position is found. See below:
--
-- ...      m         n         p
-- ...4567890123456789012345678901234567890....  (positions in string)
--          ^              ^
--        start           end                    (range in format (line:character))
--
-- As the text edit replaces a string in the Range, the prefix of the string
-- must be kept. This means (0..start-1) both indexes included, then the new
-- string must be inserted, and after the string from the end position must
-- be included (end..length-1), both indexes included.
--
-- Although the situation is complex, the position encoding is based on line
-- and character index, which must be decreased appropriately; the new line
-- encoding of \r\n vs \n makes it trickier. However, this module only handles \n case.
--
-- Another complication is that the positions could refer to non-existing parts
-- of the content. Such as, the Position could point after the end of
-- the line end of the file.
--
-- The [before the first character of the line] range is encoded as the start position
-- pointing after the line's last character and at the new line's first character
-- such as (l,c+1) and (l+1,0).
--
-- Characters that point after the last character are considered invalid Range,
-- and it must be interpreted as the last character of the line.


Pos : Type
Pos = (Nat, Nat)

||| Line length as defined in LSP, newline character is not counted in.
||| 
||| See more:
||| https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position
lineLength : String -> Nat
lineLength ""  = 0
lineLength str = assert_total $
  let l = strLength str in
  if isNL (strIndex str (l - 1))
    then minus (cast l) 1
    else cast l

-- Take the string up to the position, character addressed by the position is included.
takePos : Pos -> String -> String
takePos (l,c) str =
  let ls : List String = lines str in
  let ln : Nat         = length ls in
  case l < ln of
    False => fastUnlines ls
    True  =>
      let (pre, post) = splitAt l ls in
      case post of
        [] => fastUnlines pre
        (pl :: _) =>
          let ll : Nat := lineLength pl in
          let cx : Nat := if c > ll then ll else c in
          fastUnlines $ pre ++ [strSubstr 0 (cast cx) pl]

-- Drop the string up to the position, character addressed by the position is excluded.
dropPos : Pos -> String -> String
dropPos (l,c) str =
  let ls : List String = lines str in
  let ln : Nat         = length ls in
  case l < ln of
    False => "\n" -- Assumes always a newline at the end.
    True =>
      let (pre, post) = splitAt l ls in
      case post of
        [] => "\n"
        (pl :: prest) =>
          let ll : Nat := lineLength pl in
          let cx : Nat := if c > ll then ll else c in
          fastUnlines $ (strSubstr (cast cx) (cast (minus (cast (strLength pl)) cx)) pl :: prest)

-- Newline character is always assumed from takePos and dropPos, and they must be
-- removed to re-establish consistency. The SubString operations should be quick.
dropLastNL : String -> String
dropLastNL str = assert_total $ case strLength str of
  0 => str
  n => strSubstr 0 (n - 1) str

-- Computes the pre and post part for the given range. Drops the part which is addressed by the range.
rangeSplit : Pos -> Pos -> String -> (String, String)
rangeSplit s e str = (dropLastNL $ takePos s str, dropLastNL $ dropPos e str)

||| Updates the content string with the new string replacing the part between the start and end positions.
|||
||| This function is an approximation as the implementation is based on unlines and lines,
||| and the lines function adds newline characters. The edge-case around newlines
||| at the end of files is simplified to assume that the virtual content always
||| has the empty line as the last line.
update : Pos -> Pos -> String -> String -> String
update s e new content =
  let (pre, post) = rangeSplit s e content in
  fastConcat [pre, new, post, "\n"]

-- * Identifier

-- Oversimplified version of the identifier.
isIdentifierChar : Char -> Bool
isIdentifierChar c = or [isAlphaNum c, c == '.', c == '_', c == '\'']

-- * Word at position

-- Determines the identifier at the index.
--
-- The implementation tart out scanning for the start position
-- of the identifier and then looks for the end position.
identifierAtIndex : String -> Nat -> String
identifierAtIndex ""  i = ""
identifierAtIndex str i = identifierPre (minus i 1) i
  where
    l : Nat
    l = cast (strLength str)

    identifierPost : Nat -> Nat -> String
    identifierPost n Z = assert_total $ case isIdentifierChar (strIndex str (cast Z)) of
      True => identifierPost n (S Z)
      False => "" -- n < m -- assumed and this means an empty result
    identifierPost n (S m) = assert_total $ case (S m) == l of
      True  => strSubstr (cast n) (cast (minus (S m) n)) str
      False => case isIdentifierChar (strIndex str (cast (S m))) of
        True  => identifierPost n (S (S m))
        False => strSubstr (cast n) (cast (minus (S m) n)) str

    identifierPre : Nat -> Nat -> String
    identifierPre Z m = assert_total $ case isIdentifierChar (strIndex str (cast Z)) of
      True  => identifierPost Z m
      False => identifierPost (S Z) m
    identifierPre (S n) m = assert_total $ case isIdentifierChar (strIndex str (cast (S n))) of
      True  => identifierPre n m
      False => identifierPost (S (S n)) m

-- Identifier at position.
--
-- Picks the line and char, if exists, otherwise reports empty string.
identifierAtPos : Pos -> String -> String
identifierAtPos (l,c) str =
  let ls : List String := lines str in
  let ln : Nat         := length ls in
  case l > ln of
    True  => ""
    False => case drop l ls of
      []        => ""
      (line::_) =>
        let ll : Nat := lineLength line in
        let cx : Nat := if c > ll then ll else c in
        identifierAtIndex line cx

||| Determines an identifier like string at a given position.
|||
||| The empty String will be reported if the position points to a non-existent
||| part of the String. This function is needed when generating the suggestion list.
export
identifierAtPosition : Position -> String -> String
identifierAtPosition (MkPosition line character) str = identifierAtPos (cast line, cast character) str

-- * LSP API

validRange : (startline, startchar, endline, endchar : Nat) -> Bool
validRange startline startchar endline endchar =
  case compare startline endline of
    LT => True
    EQ => case compare startchar endchar of
      LT => True
      EQ => True
      GT => False
    GT => False

changeRange : Range -> String -> String -> Either String String
changeRange (MkRange (MkPosition sl sc) (MkPosition el ec)) content str =
  case validRange (cast sl) (cast sc) (cast el) (cast ec) of
    False => Left "Invalid range: \{show (sl,sc,el,ec)}"
    True  => Right $ update (cast sl, cast sc) (cast el, cast ec) str content

contentChangeEvent : TextDocumentContentChangeEvent -> String
contentChangeEvent (MkTextDocumentContentChangeEvent text) = text

contentChangeEventWithRange : TextDocumentContentChangeEventWithRange -> String -> Either String String
contentChangeEventWithRange
      (MkTextDocumentContentChangeEventWithRange range rangeLength text)
      content
  = changeRange range content text

contentChangeOneOf : OneOf [TextDocumentContentChangeEvent, TextDocumentContentChangeEventWithRange] -> String -> Either String String
contentChangeOneOf (Here c)         _ = Right $ contentChangeEvent c
contentChangeOneOf (There (Here r)) c = contentChangeEventWithRange r c

export
changeVirtualContent
  :  List (OneOf [TextDocumentContentChangeEvent, TextDocumentContentChangeEventWithRange])
  -> String -> Either String String
changeVirtualContent changes content = foldlM (flip contentChangeOneOf) content changes

---------------------------
-- Poor man's testing :) --
---------------------------

wordPosTestCase : Pos -> String -> String -> IO ()
wordPosTestCase p str expected = do
  let res = identifierAtPos p str
  if expected == res
    then do
      putStrLn "OK: identifierAtPos \{show p} \{show str} \{show expected}"
    else do
      putStrLn "Failed: identifierAtPos \{show p} \{show str} \{show expected}"
      putStrLn "Expected: \{show expected}"
      putStrLn "Got: \{show res}"

updateTestCase : Pos -> Pos -> String -> String -> String -> IO ()
updateTestCase s e new str expected = do
  let res = update s e new str
  if expected == res
    then do
      putStrLn "OK: update \{show s} \{show e} \{show new} \{show str}"
    else do
      putStrLn "Failed: update \{show s} \{show e} \{show new} \{show str}"
      putStrLn "Expected: \{show expected}"
      putStrLn "Got: \{show res}"

tests : IO ()
tests = do

  -- Update tests

  do -- Empty string
    let str = ""
    updateTestCase (0,0) (0,0) ""     str "\n"
    updateTestCase (0,0) (0,0) "a\nb" str "a\nb\n"
    updateTestCase (0,0) (0,1) "a"    str "a\n"
    updateTestCase (1,0) (1,0) "a"    str "a\n"
    updateTestCase (1,1) (1,1) "a"    str "a\n"
    updateTestCase (1,1) (1,2) "a"    str "a\n"
    updateTestCase (1,1) (2,2) "a"    str "a\n"

  do -- New line sting
    let str = " "
    updateTestCase (0,0) (0,0) ""     str " \n"
    updateTestCase (0,0) (0,0) "a\nb" str "a\nb \n"
    updateTestCase (0,0) (0,1) "a"    str "a\n"
    updateTestCase (1,0) (1,0) "a"    str " a\n"
    updateTestCase (1,1) (1,1) "a"    str " a\n"
    updateTestCase (2,1) (2,1) ""     str " \n"
    updateTestCase (2,1) (2,1) "a"    str " a\n"

  do -- New line sting
    let str = "\n"
    updateTestCase (0,0) (0,0) ""     str "\n"
    updateTestCase (0,0) (0,0) "a\nb" str "a\nb\n"
    updateTestCase (0,0) (0,1) "a"    str "a\n"
    updateTestCase (1,0) (1,0) "a"    str "a\n"
    updateTestCase (1,1) (1,1) "a"    str "a\n"
    updateTestCase (2,1) (2,1) ""     str "\n"
    updateTestCase (2,1) (2,1) "a"    str "a\n"

  do -- One line, no newline
    --         0         1   1
    --         012345678901234
    let str = "This is a line."
    updateTestCase (0,0) (0,0)   ""    str "This is a line.\n"
    updateTestCase (0,5) (0,5)   ""    str "This is a line.\n"
    updateTestCase (0,0) (0,0)   "a"   str "aThis is a line.\n"
    updateTestCase (0,5) (0,5)   "a"   str "This ais a line.\n"
    updateTestCase (0,5) (0,8)   "xx"  str "This xxa line.\n"
    updateTestCase (0,5) (0,15)  "aaa" str "This aaa\n"
    updateTestCase (0,3) (0,4)   ""    str "Thi is a line.\n"
    updateTestCase (0,11) (0,20) ""    str "This is a l\n"

  do -- One line, newline
    --         0         1       1
    --         0123456789012345678
    let str = "This is a newline.\n"
    updateTestCase (0,0) (0,0)  ""    str str
    updateTestCase (0,5) (0,5)  ""    str str
    updateTestCase (0,0) (0,0)  "a"   str "aThis is a newline.\n"
    updateTestCase (0,5) (0,5)  "a"   str "This ais a newline.\n"
    updateTestCase (0,5) (0,8)  "aa"  str "This aaa newline.\n"
    updateTestCase (0,5) (0,18) "aaa" str "This aaa\n"
    updateTestCase (0,18) (0,18) "a"  str "This is a newline.a\n"
    updateTestCase (0,11) (0,20) ""   str "This is a n\n"

  do -- Multiple lines, no newline at the last line.
          --   0         1         2  2 0         1         2   2 0         1         2 2
          --   012345678901234567890123 0123456789012345678901234 01234567890123456789012
    let str = "This is the first line.\nThis is the second line.\nThis is the third line."
    updateTestCase (1,0) (1,0)   ""   str (str ++ "\n")
    updateTestCase (1,0) (1,0)   "a"  str "This is the first line.\naThis is the second line.\nThis is the third line.\n"
    -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range
    -- If you want to specify a range that contains a line including the line ending character(s)
    -- then use an end position denoting the start of the next line.
    updateTestCase (1,24) (2,0)  "a"  str "This is the first line.\nThis is the second line.aThis is the third line.\n"
    updateTestCase (1,19) (2,0)  "a"  str "This is the first line.\nThis is the second aThis is the third line.\n"
    updateTestCase (1,0)  (2,0)  ""   str "This is the first line.\nThis is the third line.\n"
    updateTestCase (1,0)  (2,0)  "a"  str "This is the first line.\naThis is the third line.\n"
    updateTestCase (1,8)  (2,8)  ""   str "This is the first line.\nThis is the third line.\n"
    updateTestCase (1,8)  (2,8)  "a " str "This is the first line.\nThis is a the third line.\n"
    updateTestCase (1,8)  (2,9)  "x"  str "This is the first line.\nThis is xhe third line.\n"
    updateTestCase (1,23) (2,22) ""   str "This is the first line.\nThis is the second line.\n"
    updateTestCase (1,23) (2,22) "a"  str "This is the first line.\nThis is the second linea.\n"
    updateTestCase (1,23) (2,23) "a"  str "This is the first line.\nThis is the second linea\n"
    updateTestCase (0,23) (1,24) ""   str "This is the first line.\nThis is the third line.\n"
    updateTestCase (1,0)  (1,40) ""   str "This is the first line.\n\nThis is the third line.\n"
    updateTestCase (1,0)  (1,24) ""   str "This is the first line.\n\nThis is the third line.\n"
    updateTestCase (1,10) (1,40) ""   str "This is the first line.\nThis is th\nThis is the third line.\n"
    updateTestCase (1,0)  (4,40) ""   str "This is the first line.\n\n"
    updateTestCase (1,10) (4,40) ""   str "This is the first line.\nThis is th\n"

  do -- Multiple lines, newline at the last line.
          --   0         1         2    0         1         2   2 0         1         2  2
          --   012345678901234567890123 0123456789012345678901234 012345678901234567890123
    let str = "This is the first line.\nThis is the second line.\nThis is the third line.\n"
    updateTestCase (1,0) (1,0)   ""   str str
    updateTestCase (1,0) (1,0)   "a"  str "This is the first line.\naThis is the second line.\nThis is the third line.\n"
    -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range
    -- If you want to specify a range that contains a line including the line ending character(s)
    -- then use an end position denoting the start of the next line.
    updateTestCase (1,24) (2,0)  "a"  str "This is the first line.\nThis is the second line.aThis is the third line.\n"
    updateTestCase (1,19) (2,0)  "a"  str "This is the first line.\nThis is the second aThis is the third line.\n"
    updateTestCase (1,0)  (2,0)  ""   str "This is the first line.\nThis is the third line.\n"
    updateTestCase (1,0)  (2,0)  "a"  str "This is the first line.\naThis is the third line.\n"
    updateTestCase (1,8)  (2,8)  ""   str "This is the first line.\nThis is the third line.\n"
    updateTestCase (1,8)  (2,8)  "a " str "This is the first line.\nThis is a the third line.\n"
    updateTestCase (1,8)  (2,9)  "x"  str "This is the first line.\nThis is xhe third line.\n"
    updateTestCase (1,23) (2,22) ""   str "This is the first line.\nThis is the second line.\n"
    updateTestCase (1,23) (2,22) "a"  str "This is the first line.\nThis is the second linea.\n"
    updateTestCase (1,23) (2,23) "a"  str "This is the first line.\nThis is the second linea\n"
    updateTestCase (0,23) (1,24) ""   str "This is the first line.\nThis is the third line.\n"
    updateTestCase (1,0)  (1,40) ""   str "This is the first line.\n\nThis is the third line.\n"
    updateTestCase (1,0)  (1,24) ""   str "This is the first line.\n\nThis is the third line.\n"
    updateTestCase (1,10) (1,40) ""   str "This is the first line.\nThis is th\nThis is the third line.\n"
    updateTestCase (1,0)  (4,40) ""   str "This is the first line.\n\n"
    updateTestCase (1,10) (4,40) ""   str "This is the first line.\nThis is th\n"

  -- Word at position tests.

  do -- Empty string
    wordPosTestCase (0,0)  "" ""
    wordPosTestCase (0,10) "" ""
    wordPosTestCase (1,0)  "" ""
    wordPosTestCase (1,10) "" ""
  
  do -- New line
    wordPosTestCase (0,0)  "\n" ""
    wordPosTestCase (0,10) "\n" ""
    wordPosTestCase (1,0)  "\n" ""
    wordPosTestCase (1,10) "\n" ""
    wordPosTestCase (2,0)  "\n" ""
    wordPosTestCase (2,10) "\n" ""

  do -- Space character
    wordPosTestCase (0,0)  " " ""
    wordPosTestCase (0,10) " " ""
    wordPosTestCase (1,0)  " " ""
    wordPosTestCase (1,10) " " ""
  
  do -- One line without newline
    --         0         1   1
    --         012345678901234
    let str = "This is a line."
    wordPosTestCase (0,0)  str "This"
    wordPosTestCase (0,2)  str "This"
    wordPosTestCase (0,4)  str "This"
    wordPosTestCase (0,5)  str "is"
    wordPosTestCase (0,6)  str "is"
    wordPosTestCase (0,7)  str "is"
    wordPosTestCase (0,8)  str "a"
    wordPosTestCase (0,9)  str "a"
    wordPosTestCase (0,10) str "line."
    wordPosTestCase (0,12) str "line."
    wordPosTestCase (0,14) str "line."
    wordPosTestCase (0,20) str "line."
    wordPosTestCase (1,0)  str ""

  do -- One line with newline
    --         0         1    1
    --         0123456789012345
    let str = "This is a line.\n"
    wordPosTestCase (0,0)  str "This"
    wordPosTestCase (0,2)  str "This"
    wordPosTestCase (0,4)  str "This"
    wordPosTestCase (0,5)  str "is"
    wordPosTestCase (0,6)  str "is"
    wordPosTestCase (0,7)  str "is"
    wordPosTestCase (0,8)  str "a"
    wordPosTestCase (0,9)  str "a"
    wordPosTestCase (0,10) str "line."
    wordPosTestCase (0,12) str "line."
    wordPosTestCase (0,14) str "line."
    wordPosTestCase (0,20) str "line."
    wordPosTestCase (1,0)  str ""

  do -- Three lines without newline
          --   0         1         2    0         1         2   2 0         1         2 2
          --   012345678901234567890123 0123456789012345678901234 01234567890123456789012
    let str = "This is the first line.\nThis is the second line.\nThis is the third line."
    wordPosTestCase (0,0)  str "This"
    wordPosTestCase (0,2)  str "This"
    wordPosTestCase (0,4)  str "This"
    wordPosTestCase (0,5)  str "is"
    wordPosTestCase (0,6)  str "is"
    wordPosTestCase (0,7)  str "is"
    wordPosTestCase (0,8)  str "the"
    wordPosTestCase (0,12) str "first"
    wordPosTestCase (0,21) str "line."
    wordPosTestCase (0,30) str "line."
    wordPosTestCase (1,0)  str "This"
    wordPosTestCase (1,14) str "second"
    wordPosTestCase (1,21) str "line."
    wordPosTestCase (1,30) str "line."
    wordPosTestCase (2,0)  str "This"
    wordPosTestCase (2,14) str "third"
    wordPosTestCase (2,21) str "line."
    wordPosTestCase (2,30) str "line."
    wordPosTestCase (3,0)  str ""
    wordPosTestCase (3,30) str ""

  do -- Three lines with newline
          --   0         1         2    0         1         2   2 0         1         2  2
          --   012345678901234567890123 0123456789012345678901234 012345678901234567890123
    let str = "This is the first line.\nThis is the second line.\nThis is the third line.\n"
    wordPosTestCase (0,0)  str "This"
    wordPosTestCase (0,2)  str "This"
    wordPosTestCase (0,4)  str "This"
    wordPosTestCase (0,5)  str "is"
    wordPosTestCase (0,6)  str "is"
    wordPosTestCase (0,7)  str "is"
    wordPosTestCase (0,8)  str "the"
    wordPosTestCase (0,12) str "first"
    wordPosTestCase (0,21) str "line."
    wordPosTestCase (0,30) str "line."
    wordPosTestCase (1,0)  str "This"
    wordPosTestCase (1,14) str "second"
    wordPosTestCase (1,21) str "line."
    wordPosTestCase (1,30) str "line."
    wordPosTestCase (2,0)  str "This"
    wordPosTestCase (2,14) str "third"
    wordPosTestCase (2,21) str "line."
    wordPosTestCase (2,30) str "line."
    wordPosTestCase (3,0)  str ""
    wordPosTestCase (3,30) str ""

||| Standalone test for formatIdrisSource function
||| Run with : idris2 -- exec main FormatTest.idr
module Main

import Data.Nat
import Data.String

||| Simplified formatting options
record Options where
  constructor MkOptions
  trimTrailingWhitespace : Bool
  insertFinalNewline : Bool
  insertSpaces : Bool
  tabSize : Nat

||| Copy of formatIdrisSource logic for testing
formatIdrisSourceTest : Options -> String -> Bool -> String
formatIdrisSourceTest options src isLiterate =
  -- Helper: Normalize line endings
  let goLine : List Char -> List Char
      goLine [] = []
      goLine ('\r' :: '\n' :: xs) = '\n' :: goLine xs -- CRLF -> LF
      goLine ('\r' :: xs) = '\n' :: goLine xs -- CR -> LF
      goLine (c :: xs) = c :: goLine xs
  
      normalizedSrc = pack $ goLine (unpack src)
      lineList : List String
      lineList = lines normalizedSrc
      
      -- Step 2: Convert tabs to spaces (if option is set)
      tabSize : Nat
      tabSize = options.tabSize
      
      convertIndent : String -> String
      convertIndent line = 
        if options.insertSpaces
           then pack $ goTab (unpack line)
           else line
        where
          goTab : List Char -> List Char
          goTab [] = []
          goTab ('\t' :: xs) = replicate tabSize ' ' ++ goTab xs
          goTab (c :: xs) = c :: goTab xs
      
      -- Literate Idris: Only process lines starting with '>'
      processLiterate : String -> String
      processLiterate line =
        if isPrefixOf ">" line
           then ">" ++ processLine (substr 1 (length line) line)
           else line
        where
          processLine : String -> String
          processLine l =
            let withIndent = convertIndent l
                withTrim = if options.trimTrailingWhitespace
                              then trimTrailing withIndent
                              else withIndent
             in withTrim
          where
            trimTrailing : String -> String
            trimTrailing = pack . reverse . dropWhile isSpace . reverse . unpack
      
      passedIndent : List String
      passedIndent = if isLiterate 
                      then map processLiterate lineList
                      else map convertIndent lineList
      
      -- Step 3: Trim trailing whitespace
      trimTrailingWS : String -> String
      trimTrailingWS = pack . reverse . dropWhile isSpace . reverse . unpack

      processLine : String -> String 
      processLine = if options.trimTrailingWhitespace
                       then trimTrailingWS
                       else id
      
      -- For literate idris, we've already handled the lines
      pass1 : List String
      pass1 = if isLiterate then passedIndent else map processLine passedIndent

      -- Step 4-8: Structural normalization (ONLY for non-literate files for now)
      finalLines : List String
      finalLines = if isLiterate
                      then pass1
                      else normalizeStructure pass1
        where
          normalizeStructure : List String -> List String
          normalizeStructure linesInput =
            let collapseBlanks : List String -> List String
                collapseBlanks [] = []
                collapseBlanks (x :: xs) = x :: go False xs
                  where
                    go : Bool -> List String -> List String
                    go wasBlank [] = []
                    go wasBlank (y :: ys) =
                      let isBlank = trim y == ""
                       in if isBlank && wasBlank
                             then go True ys -- skip consecutive blank
                             else y :: go isBlank ys

                dropWhileEnd : (a -> Bool) -> List a -> List a
                dropWhileEnd p = reverse . dropWhile p . reverse

                isModuleLine : String -> Bool
                isModuleLine line = isPrefixOf "module" (trim line)
                
                ensureSingleBlank : List String -> List String
                ensureSingleBlank [] = [""]
                ensureSingleBlank (y :: ys) =
                  if trim y == ""
                     then "" :: ys -- already blank
                     else "" :: y :: ys -- insert blank
                
                ensureModuleBlank : List String -> List String
                ensureModuleBlank [] = []
                ensureModuleBlank (x :: xs) =
                  if isModuleLine x
                     then x :: ensureSingleBlank xs
                     else x :: xs

                normalizeImports : List String -> List String
                normalizeImports linesIn = go False [] linesIn
                  where
                    needBlankBeforeImports : List String -> Bool
                    needBlankBeforeImports [] = False
                    needBlankBeforeImports (y :: _) =
                      not (isModuleLine y) && trim y /= ""
                    
                    go : Bool -> List String -> List String -> List String
                    go inImports acc [] = 
                      let acc' = if inImports then "" :: acc else acc
                       in reverse acc'
                    go inImports acc (x :: xs) =
                      let isImport = isPrefixOf "import" (trim x)
                          isBlank = trim x == ""
                       in if isImport
                             then
                               let acc' = if not inImports && needBlankBeforeImports acc
                                             then "" :: acc
                                             else acc
                                in go True (x :: acc') xs
                             else if isBlank && inImports
                                     then go True acc xs -- skip blanks and STAY in import block
                                     else
                                       let acc' = if inImports then "" :: acc else acc
                                        in go False (x :: acc') xs
                                        
                collapsed = collapseBlanks linesInput
                trimmedLeading = dropWhile (\s => trim s == "") collapsed
                trimmedTrailing = dropWhileEnd (\s => trim s == "") trimmedLeading
                withModuleBlank = ensureModuleBlank trimmedTrailing
                withImportSpacing = normalizeImports withModuleBlank
             in withImportSpacing

      -- Step 9: Check if we should add final newline
      hadContent : Bool
      hadContent = not (null finalLines)

      -- Step 10: unlines adds trailing newline
   in if hadContent then unlines finalLines else ""

test' : String -> Options -> String -> Bool -> String -> IO ()
test' name options input isLiterate expected = do
  let result = formatIdrisSourceTest options input isLiterate
  if result == expected
     then putStrLn $ "✓ " ++ name
     else do
       putStrLn $ "✗ " ++ name
       putStrLn $ "  Expected: \{show expected}"
       putStrLn $ "  Got:      \{show result}"

test : String -> Options -> String -> String -> IO ()
test name options input expected = test' name options input False expected

defaultOptions : Options
defaultOptions = MkOptions True True True 4 -- trim, newline, spaces, 4 spaces

main : IO ()
main = do
  putStrLn "Testing formatIdrisSource..."
  putStrLn ""
  
  -- Original tests
  test "Trim trailing whitespace on indented lines" defaultOptions
       "foo : Nat\n  bar x =  \n    x + 1  \n"
       "foo : Nat\n  bar x =\n    x + 1\n"

  test "Trim trailing whitespace" defaultOptions
       "foo : Nat -> Nat  \nbar : String  \n"
       "foo : Nat -> Nat\nbar : String\n"
  
  test "Add final newline" defaultOptions
       "foo : Nat -> Nat\nbar : String"
       "foo : Nat -> Nat\nbar : String\n"
  
  test "Empty input" defaultOptions
       ""
       ""
  
  test "Idris code" defaultOptions
       "module Main  \n\nfoo : Nat -> Nat  \nfoo x = x + 1  \n"
       "module Main\n\nfoo : Nat -> Nat\nfoo x = x + 1\n"
  
  test "Already has newline" defaultOptions
       "foo : Nat\n"
       "foo : Nat\n"
  
  test "Multiple lines with trailing spaces" defaultOptions
       "a : Nat   \nb : Nat   \nc : Nat   "
       "a : Nat\nb : Nat\nc : Nat\n"
  
  -- NEW: Tab to space conversion tests
  putStrLn ""
  putStrLn "Tab to Space Conversion Tests:"
  putStrLn ""
  
  let noTrimOptions = MkOptions False True True 4 -- no trim, newline, spaces
  test "Single tab to 4 spaces" noTrimOptions
       "\tfoo : Nat"
       "    foo : Nat\n"
  
  test "Multiple tabs to spaces" noTrimOptions
       "\t\tfoo : Nat"
       "        foo : Nat\n"
  
  test "Mixed tabs and spaces" noTrimOptions
       "\t  foo : Nat"
       "      foo : Nat\n"
  
  let noConvertOptions = MkOptions False True False 4 -- no trim, newline, NO spaces conversion
  test "Keep tabs when insertSpaces is false" noConvertOptions
       "\tfoo : Nat"
       "\tfoo : Nat\n"
  
  -- NEW: Line ending normalization tests
  putStrLn ""
  putStrLn "Line Ending Normalization Tests:"
  putStrLn ""
  
  test "Windows line endings (CRLF)" defaultOptions
       "foo : Nat\r\nbar : String\r\n"
       "foo : Nat\nbar : String\n"
  
  test "Old Mac line endings (CR)" defaultOptions
       "foo : Nat\rbar : String\r"
       "foo : Nat\nbar : String\n"
  
  test "Mixed line endings" defaultOptions
       "foo : Nat\r\nbar : String\nbaz : Int\r"
       "foo : Nat\nbar : String\nbaz : Int\n"

  test "Tabs with Windows line endings" noTrimOptions
       "\tfoo : Nat\r\n\tbar : String\r\n"
       "    foo : Nat\n    bar : String\n"

  -- NEW: Blank line normalization tests
  putStrLn ""
  putStrLn "Blank Line Normalization Tests:"
  putStrLn ""

  test "Collapse multiple blank lines" defaultOptions
       "foo : Nat\n\n\nbar : String"
       "foo : Nat\n\nbar : String\n"

  test "Collapse many blank lines" defaultOptions
       "foo : Nat\n\n\n\n\nbar : String"
       "foo : Nat\n\nbar : String\n"

  test "Remove leading blank lines" defaultOptions
       "\n\nfoo : Nat\nbar : String"
       "foo : Nat\nbar : String\n"

  test "Remove trailing blank lines" defaultOptions
       "foo : Nat\nbar : String\n\n"
       "foo : Nat\nbar : String\n"

  test "Remove leading and trailing blank lines" defaultOptions
       "\n\nfoo : Nat\n\nbar : String\n\n"
       "foo : Nat\n\nbar : String\n"

  test "Preserve single blank line between code" defaultOptions
       "foo : Nat\n\nbar : String"
       "foo : Nat\n\nbar : String\n"

  -- NEW: Module and Import formatting tests
  putStrLn ""
  putStrLn "Module and Import Formatting Tests:"
  putStrLn ""

  test "Add blank line after module declaration" defaultOptions
       "module Main\nfoo : Nat"
       "module Main\n\nfoo : Nat\n"

  test "Preserve blank line after module declaration" defaultOptions
       "module Main\n\nfoo : Nat"
       "module Main\n\nfoo : Nat\n"

  test "Add blank line before imports" defaultOptions
       "module Main\n\nimport Data.List\nfoo : Nat"
       "module Main\n\nimport Data.List\n\nfoo : Nat\n"

  test "Collapse blank lines in import block" defaultOptions
       "module Main\n\nimport Data.List\n\n\nimport Data.Vect\nfoo : Nat"
       "module Main\n\nimport Data.List\nimport Data.Vect\n\nfoo : Nat\n"

  test "Add blank line after import block" defaultOptions
       "module Main\n\nimport Data.List\nfoo : Nat"
       "module Main\n\nimport Data.List\n\nfoo : Nat\n"

  -- NEW: Literate Idris tests
  putStrLn ""
  putStrLn "Literate Idris Tests:"
  putStrLn ""

  test' "Literate: format code line" defaultOptions
        "This is a comment\n\n> foo : Nat -> Nat  \n" True
        "This is a comment\n\n> foo : Nat -> Nat\n"
  
  test' "Literate: preserve comment line" defaultOptions
        "  This is a comment with spaces  \n> foo : Nat\n" True
        "  This is a comment with spaces  \n> foo : Nat\n"

  test' "Literate: tab to space in code" defaultOptions
        "> \tfoo : Nat" True
        ">     foo : Nat\n"

  test' "Literate: no structural normalization" defaultOptions
        "module Main\n> foo : Nat" True
        "module Main\n> foo : Nat\n"

  putStrLn ""
  putStrLn "Tests complete!"

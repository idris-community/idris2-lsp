module Server.Formatting

import Data.List
import Data.Maybe
import Data.Nat
import Data.String

-- ==========================================================================
-- Line record: pre-computed data to avoid repeated pack/unpack in transforms.
-- Each line is unpacked ONCE; all structural checks use the pre-computed fields.
-- ==========================================================================

||| A source line with pre-computed character data.
||| Created once per line via `mkLine`; avoids all intermediate pack/unpack.
public export
record Line where
  constructor MkLine
  raw     : String      -- original string (preserved for output)
  chars   : List Char   -- unpack raw
  trimmed : List Char   -- leading whitespace stripped (chars)
  ws      : List Char   -- leading whitespace chars

mkLine : String -> Line
mkLine s = let cs = unpack s
               w  = takeWhile isSpace cs
               t  = drop (length w) cs
            in MkLine s cs t w

blankLine : Line
blankLine = MkLine "" [] [] []

-- Rebuild a Line from a new raw string (used by transforms that modify content)
reLine : String -> Line
reLine = mkLine

-- ==========================================================================
-- Shared helpers operating on Line fields (zero allocation)
-- ==========================================================================

isBlank : Line -> Bool
isBlank l = null l.trimmed

-- Check if trimmed chars start with a given prefix (as List Char)
charsStartsWith : List Char -> List Char -> Bool
charsStartsWith [] _ = True
charsStartsWith _ [] = False
charsStartsWith (p :: ps) (c :: cs) = p == c && charsStartsWith ps cs

-- Pre-unpacked keyword prefixes (module-level constants, created once)
modulePrefix : List Char
modulePrefix = unpack "module"

importPrefix : List Char
importPrefix = unpack "import"

docPrefix : List Char
docPrefix = unpack "|||"

commentPrefix : List Char
commentPrefix = unpack "--"

dataPrefix : List Char
dataPrefix = unpack "data "

identChars : List Char -> List Char
identChars = takeWhile (\c => isAlphaNum c || c == '_' || c == '\'')

-- Keyword prefixes that indicate "not a type signature"
sigKwPrefixes : List (List Char)
sigKwPrefixes = map unpack
  [ "data ", "record ", "where", "let ", "module "
  , "import ", "namespace ", "mutual", "interface "
  , "implementation ", "covering ", "total "
  , "partial ", "export ", "public ", "private "
  , "using ", "parameters " ]

isSigKeyword : List Char -> Bool
isSigKeyword trimmed = any (\kw => charsStartsWith kw trimmed) sigKwPrefixes

-- ==========================================================================
-- Structural transforms (List Line -> List Line)
-- ==========================================================================

collapseBlanks : List Line -> List Line
collapseBlanks [] = []
collapseBlanks (x :: xs) = x :: go False xs
  where
    go : Bool -> List Line -> List Line
    go wasBlank [] = []
    go wasBlank (y :: ys) =
      let yBlank = isBlank y
       in if yBlank && wasBlank
             then go True ys
             else y :: go yBlank ys

dropWhileEnd : (a -> Bool) -> List a -> List a
dropWhileEnd p = reverse . dropWhile p . reverse

ensureModuleBlank : List Line -> List Line
ensureModuleBlank [] = []
ensureModuleBlank (x :: xs) =
  if charsStartsWith modulePrefix x.trimmed
     then x :: ensureSingleBlank xs
     else x :: xs
  where
    ensureSingleBlank : List Line -> List Line
    ensureSingleBlank [] = [blankLine]
    ensureSingleBlank (y :: ys) =
      if isBlank y then blankLine :: ys else blankLine :: y :: ys

nsNormalizeImports : List Line -> List Line
nsNormalizeImports linesIn = go False [] [] linesIn
  where
    isModuleLine : Line -> Bool
    isModuleLine l = charsStartsWith modulePrefix l.trimmed

    needBlankBefore : List Line -> Bool
    needBlankBefore [] = False
    needBlankBefore (y :: _) = not (isModuleLine y) && not (isBlank y)

    flushImports : List Line -> List Line -> List Line
    flushImports buf acc =
      let sorted = map reLine (reverse (nub (sort (map raw buf))))
       in sorted ++ acc

    go : Bool -> List Line -> List Line -> List Line -> List Line
    go inImp buf acc [] =
      if inImp then reverse (blankLine :: flushImports buf acc)
               else reverse acc
    go inImp buf acc (x :: xs) =
      let isImp = charsStartsWith importPrefix x.trimmed
       in if isImp
             then let acc' = if not inImp && needBlankBefore acc
                                then blankLine :: acc
                                else acc
                   in go True (x :: buf) acc' xs
             else if isBlank x && inImp
                     then go True buf acc xs
                     else let acc' = if inImp
                                        then blankLine :: flushImports buf acc
                                        else acc
                           in go False [] (x :: acc') xs

nsRemoveSigDefBlank : List Line -> List Line
nsRemoveSigDefBlank input = go input []
  where
    getSigName : Line -> Maybe (List Char, List Char)
    getSigName l =
      if isSigKeyword l.trimmed || null l.trimmed
         then Nothing
         else let ident = identChars l.trimmed
                  rest  = drop (length ident) l.trimmed
               in if null ident then Nothing
                  else case rest of
                    (' ' :: ':' :: ' ' :: _) => Just (l.ws, ident)
                    _ => Nothing

    isDefFor : List Char -> List Char -> Line -> Bool
    isDefFor wsChars identCs l =
      let ident = identChars l.trimmed
          after = drop (length identCs) l.trimmed
       in l.ws == wsChars && ident == identCs &&
          case after of
            []          => True
            (' ' :: _)  => True
            ('(' :: _)  => True
            ('{' :: _)  => True
            _ => False

    isComment : Line -> Bool
    isComment l = charsStartsWith commentPrefix l.trimmed

    isAnnotation : Line -> Bool
    isAnnotation l =
      let t = pack l.trimmed
          ws = words t
          kws = ["export", "public", "private", "covering", "partial", "total"]
       in not (null ws) && all (\w => elem w kws) ws

    go : List Line -> List Line -> List Line
    go [] acc = reverse acc
    go (x :: xs) acc =
      let (blanks, rest) = span isBlank xs
          shouldAttach =
            if charsStartsWith docPrefix x.trimmed then True
            else if isAnnotation x then True  -- export/public export attach
            -- consecutive comment lines: strip blanks between them
            else if isComment x then
              case rest of
                [] => False
                (y :: _) => isComment y
            else case getSigName x of
                   Nothing => False
                   Just (wsChars, identCs) =>
                     case rest of
                       [] => False
                       (y :: _) => isDefFor wsChars identCs y
       in if shouldAttach
             then go rest (x :: acc)
             else go rest (reverse blanks ++ (x :: acc))

nsEnsureBlankBetweenDefs : List Line -> List Line
nsEnsureBlankBetweenDefs linesIn = go linesIn Nothing False
  where
    getSigName : Line -> Maybe (List Char)
    getSigName l =
      if not (null l.ws) then Nothing
      else if isSigKeyword l.trimmed || null l.trimmed
         then Nothing
         else let ident = identChars l.trimmed
                  rest  = drop (length ident) l.trimmed
               in if null ident then Nothing
                  else case rest of
                    (' ' :: ':' :: ' ' :: _) => Just ident
                    _ => Nothing

    getDefName : Line -> Maybe (List Char)
    getDefName l =
      if not (null l.ws) then Nothing
      else if isSigKeyword l.trimmed || null l.trimmed
         then Nothing
         else let ident = identChars l.trimmed
                  rest  = drop (length ident) l.trimmed
               in if null ident then Nothing
                  else case rest of
                    (' ' :: ':' :: ' ' :: _) => Nothing
                    _ => Just ident

    isDefFor : List Char -> Line -> Bool
    isDefFor identCs l =
      let ident = identChars l.trimmed
          after = drop (length identCs) l.trimmed
       in null l.ws && ident == identCs &&
          case after of
            []         => True
            (' ' :: _) => True
            ('(' :: _) => True
            ('{' :: _) => True
            _ => False

    isTopLevel : Line -> Bool
    isTopLevel l =
      case l.chars of
        []       => False
        (c :: _) => not (isSpace c)
                  && not (charsStartsWith importPrefix l.trimmed)
                  && not (charsStartsWith modulePrefix l.trimmed)

    isComment : Line -> Bool
    isComment l = charsStartsWith commentPrefix l.trimmed

    isAnnotation : Line -> Bool
    isAnnotation l =
      let t = pack l.trimmed
          ws = words t
          kws = ["export", "public", "private", "covering", "partial", "total"]
       in not (null ws) && all (\w => elem w kws) ws

    isSigFor : Line -> Line -> Bool
    isSigFor prev cur =
      if charsStartsWith docPrefix prev.trimmed
         then True
         else if isComment prev && isComment cur
            then True  -- consecutive comments stay together
            else if isAnnotation prev
            then True  -- visibility modifier attaches to next item
            else case getSigName prev of
                   Just name => isDefFor name cur
                   Nothing   =>
                     case getDefName prev of
                       Nothing       => False
                       Just prevName => isDefFor prevName cur

    go : List Line -> Maybe Line -> Bool -> List Line
    go [] _ _ = []
    go (x :: xs) prevTop hadBlank =
      if isBlank x
         then x :: go xs prevTop True
         else if isTopLevel x
            then case prevTop of
              Nothing   => x :: go xs (Just x) False
              Just prev =>
                if hadBlank || isSigFor prev x
                   then x :: go xs (Just x) False
                   else blankLine :: x :: go xs (Just x) False
            else x :: go xs prevTop hadBlank

nsAlignCaseArms : List Line -> List Line
nsAlignCaseArms [] = []
nsAlignCaseArms linesIn = go linesIn
  where
    -- Split at ' => ' using pre-computed chars (reverse-accumulator, O(n))
    splitAtArrow : Line -> Maybe (String, String)
    splitAtArrow l = go' l.chars [] False False
      where
        go' : List Char -> List Char -> Bool -> Bool -> Maybe (String, String)
        go' [] _ _ _ = Nothing
        go' ('"' :: xs) acc s ic = go' xs ('"' :: acc) (not s) ic
        go' ('\\' :: c :: xs) acc True ic = go' xs (c :: '\\' :: acc) True ic
        go' ('\'' :: xs) acc False False = go' xs ('\'' :: acc) False True
        go' ('\\' :: c :: xs) acc False True = go' xs (c :: '\\' :: acc) False True
        go' ('\'' :: xs) acc False True = go' xs ('\'' :: acc) False False
        go' (c :: xs) acc True ic = go' xs (c :: acc) True ic
        go' (c :: xs) acc False True = go' xs (c :: acc) False True
        go' (' ' :: '=' :: '>' :: ' ' :: xs) acc False False =
          Just (pack (reverse acc), pack (' ' :: '=' :: '>' :: ' ' :: xs))
        go' (' ' :: '=' :: '>' :: []) acc False False =
          Just (pack (reverse acc), " =>")
        go' (c :: xs) acc is ic = go' xs (c :: acc) is ic

    isCaseArm : Line -> Bool
    isCaseArm l = isJust (splitAtArrow l) && not (null l.ws)

    collectGroup : Line -> List Line -> (List Line, List Line)
    collectGroup first rest =
      let wsLen = length first.ws
          (more, after) = span (\l => not (isBlank l)
                                    && length l.ws == wsLen
                                    && isCaseArm l) rest
       in (first :: more, after)

    alignGroup : List Line -> List Line
    alignGroup [] = []
    alignGroup group@(first :: _) =
      let splits = mapMaybe splitAtArrow group
          patLens = map (length . fst) splits
          maxLen  = foldl max 0 patLens
       in map (pad maxLen) group
      where
        pad : Nat -> Line -> Line
        pad maxLen l =
          case splitAtArrow l of
            Nothing => l
            Just (pat, arrow) =>
              let spaces = replicate (maxLen `minus` length pat) ' '
               in reLine (pat ++ spaces ++ arrow)

    go : List Line -> List Line
    go [] = []
    go (x :: xs) =
      if isCaseArm x
         then let (group, rest) = collectGroup x xs
               in alignGroup group ++ go rest
         else x :: go xs

nsAlignDefEquals : List Line -> List Line
nsAlignDefEquals [] = []
nsAlignDefEquals linesIn = go linesIn
  where
    trimRight : String -> String
    trimRight = pack . reverse . dropWhile (== ' ') . reverse . unpack

    isOpChar : Char -> Bool
    isOpChar c = c == '<' || c == '>' || c == '/' || c == ':'
                      || c == '=' || c == '!'

    splitAtEq : Line -> Maybe (String, String)
    splitAtEq l = go' l.chars [] False False Nothing 0
      where
        go' : List Char -> List Char -> Bool -> Bool
           -> Maybe Char -> Int -> Maybe (String, String)
        go' [] _ _ _ _ _ = Nothing
        go' ('"' :: xs) acc s ic p d =
          go' xs ('"' :: acc) (not s) ic (Just '"') d
        go' ('\\' :: c :: xs) acc True ic p d =
          go' xs (c :: '\\' :: acc) True ic (Just c) d
        go' ('\'' :: xs) acc False False p d =
          go' xs ('\'' :: acc) False True (Just '\'') d
        go' ('\\' :: c :: xs) acc False True p d =
          go' xs (c :: '\\' :: acc) False True (Just c) d
        go' ('\'' :: xs) acc False True p d =
          go' xs ('\'' :: acc) False False (Just '\'') d
        go' ('-' :: '-' :: _) _ False False _ _ = Nothing
        go' (c :: xs) acc True ic p d =
          go' xs (c :: acc) True ic (Just c) d
        go' (c :: xs) acc False True p d =
          go' xs (c :: acc) False True (Just c) d
        go' ('(' :: xs) acc False False p d =
          go' xs ('(' :: acc) False False (Just '(') (d + 1)
        go' (')' :: xs) acc False False p d =
          go' xs (')' :: acc) False False (Just ')') (d - 1)
        go' ('[' :: xs) acc False False p d =
          go' xs ('[' :: acc) False False (Just '[') (d + 1)
        go' (']' :: xs) acc False False p d =
          go' xs (']' :: acc) False False (Just ']') (d - 1)
        go' ('{' :: xs) acc False False p d =
          go' xs ('{' :: acc) False False (Just '{') (d + 1)
        go' ('}' :: xs) acc False False p d =
          go' xs ('}' :: acc) False False (Just '}') (d - 1)
        go' ('=' :: '=' :: xs) acc False False p d =
          go' xs ('=' :: '=' :: acc) False False (Just '=') d
        go' ('=' :: '>' :: xs) acc False False p d =
          go' xs ('>' :: '=' :: acc) False False (Just '>') d
        go' ('=' :: xs) acc False False (Just p) d =
          if isOpChar p || d /= 0
             then go' xs ('=' :: acc) False False (Just '=') d
             else Just (pack (reverse acc), pack ('=' :: xs))
        go' ('=' :: xs) acc False False Nothing 0 =
          Just (pack (reverse acc), pack ('=' :: xs))
        go' ('=' :: xs) acc False False Nothing d =
          go' xs ('=' :: acc) False False (Just '=') d
        go' (c :: xs) acc s ic _ d =
          go' xs (c :: acc) s ic (Just c) d

    excludePrefixes : List (List Char)
    excludePrefixes = map unpack
      [ "data ", "record ", "| ", "-- ", "||| "
      , "module ", "import ", ", " ]

    isDefLine : Line -> Bool
    isDefLine l =
      not (any (\pfx => charsStartsWith pfx l.trimmed) excludePrefixes)
      && not (null l.trimmed)
      && isJust (splitAtEq l)

    collectGroup : Line -> List Line -> (List Line, List Line)
    collectGroup first rest =
      let wsLen = length first.ws
          (more, after) = span (\l => not (isBlank l)
                                   && length l.ws == wsLen
                                   && isDefLine l) rest
       in (first :: more, after)

    alignGroup : List Line -> List Line
    alignGroup [] = []
    alignGroup group =
      case the (Maybe (List (String, String))) (traverse splitAtEq group) of
        Nothing     => group
        Just splits =>
          let maxLen = foldl (\m, (p, _) => max m (length (trimRight p))) 0 splits
           in map (pad maxLen) group
      where
        pad : Nat -> Line -> Line
        pad maxLen l =
          case splitAtEq l of
            Nothing        => l
            Just (pat, eq) =>
              let p      = trimRight pat
                  spaces = replicate (maxLen `minus` length p) ' '
               in reLine (p ++ spaces ++ " " ++ eq)

    go : List Line -> List Line
    go [] = []
    go (x :: xs) =
      if isDefLine x
         then let (group, rest) = collectGroup x xs
               in if length group > 1
                     then alignGroup group ++ go rest
                     else x :: go xs
         else x :: go xs

rfTryFieldEq : List Char -> List Char -> Maybe (String, String)
rfTryFieldEq pfx chs =
  let ident = identChars chs
      aft   = dropWhile (== ' ') (drop (length ident) chs)
   in if null ident then Nothing
      else case aft of
             ('=' :: '=' :: _) => Nothing
             ('=' :: '>' :: _) => Nothing
             ('=' :: v)        =>
               Just (pack (pfx ++ ident), pack ('=' :: v))
             _ => Nothing

rfSplitEq : Line -> Maybe (String, String)
rfSplitEq l =
  case l.trimmed of
    ('{' :: ' ' :: fr) => rfTryFieldEq (l.ws ++ ['{', ' ']) fr
    (',' :: ' ' :: fr) => rfTryFieldEq (l.ws ++ [',', ' ']) fr
    _ => Nothing

nsAlignRecordFields : List Line -> List Line
nsAlignRecordFields = rfGo
  where

    rfIsField : Line -> Bool
    rfIsField l = isJust (rfSplitEq l)

    rfTrimRight : String -> String
    rfTrimRight = pack . reverse . dropWhile (== ' ') . reverse . unpack

    rfAlignGroup : List Line -> List Line
    rfAlignGroup grp =
      let splits = mapMaybe rfSplitEq grp
          maxLen = foldl (\m, (p, _) => max m (length (rfTrimRight p))) 0 splits
          padLine = \l => case rfSplitEq l of
                            Nothing        => l
                            Just (pat, eq) =>
                              let p  = rfTrimRight pat
                                  sp = replicate (maxLen `minus` length p) ' '
                               in reLine (p ++ sp ++ " " ++ eq)
       in map padLine grp

    rfGo : List Line -> List Line
    rfGo [] = []
    rfGo (x :: xs) =
      if rfIsField x
         then
           let wsLen         = length x.ws
               (more, after) = span (\l => not (isBlank l)
                                         && length l.ws == wsLen
                                         && rfIsField l) xs
               grp           = x :: more
            in if length grp > 1
                  then rfAlignGroup grp ++ rfGo after
                  else x :: rfGo xs
         else x :: rfGo xs

-- Align ':' in consecutive record field declarations at the same indentation.
-- A record field line: indented, has an identifier followed by ' : '.
-- Skips 'constructor' lines and lines starting with keywords.
nsAlignRecordDefs : List Line -> List Line
nsAlignRecordDefs = rdGo
  where
    -- Split an indented field line into (name_part, ": type_part").
    -- Returns Nothing if not a field declaration line.
    rdSplitColon : Line -> Maybe (List Char, List Char)
    rdSplitColon l =
      if null l.ws then Nothing  -- must be indented
      else let ident = identChars l.trimmed
               after = drop (length ident) l.trimmed
            in if null ident then Nothing
               else case after of
                 (' ' :: ':' :: ' ' :: rest) => Just (l.ws ++ ident, ':' :: ' ' :: rest)
                 (' ' :: ':' :: ':' :: _)    => Nothing  -- skip ::
                 (' ' :: ':' :: '=' :: _)    => Nothing  -- skip :=
                 _ => Nothing

    rdIsField : Line -> Bool
    rdIsField l = isJust (rdSplitColon l)
              && not (charsStartsWith (unpack "constructor") l.trimmed)

    rdGo : List Line -> List Line
    rdGo [] = []
    rdGo (x :: xs) =
      if rdIsField x
         then
           let wsLen         = length x.ws
               (more, after) = span (\l => not (isBlank l)
                                         && length l.ws == wsLen
                                         && rdIsField l) xs
               grp           = x :: more
            in if length grp > 1
                  then let splits = mapMaybe rdSplitColon grp
                           maxLen = foldl (\m, (p, _) => max m (length p)) 0 splits
                           padLine = \l => case rdSplitColon l of
                                             Nothing      => l
                                             Just (nm, t) =>
                                               let sp = replicate (maxLen `minus` length nm) ' '
                                                in reLine (pack (nm ++ sp ++ [' '] ++ t))
                        in map padLine grp ++ rdGo after
                  else x :: rdGo xs
         else x :: rdGo xs

nsAlignDataCtors : List Line -> List Line
nsAlignDataCtors = dcGo
  where
    dcFindEqCol : Line -> Maybe Nat
    dcFindEqCol l =
      if not (charsStartsWith dataPrefix l.trimmed)
         then Nothing
         else scan 0 l.chars
      where
        scan : Nat -> List Char -> Maybe Nat
        scan n [] = Nothing
        scan n ('=' :: '=' :: cs) = scan (n + 2) cs
        scan n ('=' :: '>' :: cs) = scan (n + 2) cs
        scan n ('=' :: _)         = Just n
        scan n (_ :: cs)          = scan (n + 1) cs

    dcIsCtorLine : Line -> Bool
    dcIsCtorLine l =
      case l.trimmed of
        ('|' :: ' ' :: _) => True
        _ => False

    dcRealign : Nat -> Line -> Line
    dcRealign col l = reLine (replicate col ' ' ++ pack l.trimmed)

    dcGo : List Line -> List Line
    dcGo [] = []
    dcGo (x :: xs) =
      case dcFindEqCol x of
        Nothing  => x :: dcGo xs
        Just col =>
          let (ctors, rest) = span dcIsCtorLine xs
           in if null ctors
                 then x :: dcGo xs
                 else x :: map (dcRealign col) ctors ++ dcGo rest

-- ==========================================================================
-- Main entry point
-- ==========================================================================

||| Run structural normalization on a list of source lines.
||| @doStructural — enable blank line / import / sig+def transforms
||| @doAlignment  — enable case arm / def = / record / data ctor alignment
export
applyStructural : List Line -> List Line
applyStructural ls =
  let collapsed       = collapseBlanks ls
      trimmedLeading  = dropWhile isBlank collapsed
      trimmedTrailing = dropWhileEnd isBlank trimmedLeading
      withModuleBlank = ensureModuleBlank trimmedTrailing
      withImports     = nsNormalizeImports withModuleBlank
      withSigDef      = nsRemoveSigDefBlank withImports
      withDefBlanks   = nsEnsureBlankBetweenDefs withSigDef
   in withDefBlanks

applyAlignment : List Line -> List Line
applyAlignment ls =
  let withCaseAlign   = nsAlignCaseArms ls
      withDefAlign    = nsAlignDefEquals withCaseAlign
      withRecordAlign = nsAlignRecordFields withDefAlign
      withRecordDefs  = nsAlignRecordDefs withRecordAlign
      withDataCtors   = nsAlignDataCtors withRecordDefs
   in withDataCtors

export
normalizeStructure : (doStructural : Bool) -> (doAlignment : Bool)
                  -> List String -> List String
normalizeStructure doStructural doAlignment linesIn =
  let ls : List Line
      ls = map mkLine linesIn
      after1 : List Line
      after1 = if doStructural then applyStructural ls else ls
      after2 : List Line
      after2 = if doAlignment  then applyAlignment after1 else after1
   in map raw after2

||| Common types and utility funtions for the LSP server implementation.
|||
||| (C) The Idris Community, 2021
module Language.LSP.Utils

import Data.Bits
import Data.List
import Data.String
import Language.LSP.Message
import System.File
import System
import System.Info

export
headerLineEnd : String
headerLineEnd = if isWindows then "\n" else "\r\n"

||| Reads a single header from an LSP message on the supplied file handle.
||| Headers end with the string "\r\n".
export
fGetHeader : (handle : File) -> IO (Either FileError String)
fGetHeader handle = do
  False <- fEOF handle
    | True => exitWith (ExitFailure 1)
  Right l <- fGetLine handle
    | Left err => pure $ Left err
  -- TODO: reading up to a string should probably be handled directly by the FFI primitive
  --       or at least in a more efficient way in Idris2
  if isSuffixOf headerLineEnd l
     then pure $ Right l
     else (map (l ++)) <$> fGetHeader handle

private
b16ToHexString : Bits16 -> String
b16ToHexString n =
  case n of
    0 => "0"
    1 => "1"
    2 => "2"
    3 => "3"
    4 => "4"
    5 => "5"
    6 => "6"
    7 => "7"
    8 => "8"
    9 => "9"
    10 => "A"
    11 => "B"
    12 => "C"
    13 => "D"
    14 => "E"
    15 => "F"
    other => assert_total $
               b16ToHexString (n `shiftR` 4) ++
               b16ToHexString (n .&. 15)

||| Pad a string with leading zeros, if
||| its length is less than 4, up to 4 symbols.
pad4 : String -> String
pad4 str =
  case length str of
    0 => "0000"
    1 => "000" ++ str
    2 => "00" ++ str
    3 => "0" ++ str
    _ => str

||| See https://en.wikipedia.org/wiki/UTF-16 for the algorithm.
||| Returns the codepoint value represented as a hex string,
||| if it encodes a symbol from the Basic Multilingual Plane.
||| Otherwise, returns the 16-bit surrogate pair, every element of which
||| is, in turn, represented as a hex string.
encodeCodepointH : Bits32 -> Either String (String, String)
encodeCodepointH x =
  case x <= 0xFFFF of
    -- Basic Multilingual Plane
    True => Left $ pad4 (b16ToHexString (cast x))
    --  One of the Supplementary Planes
    False =>
      let x' = x - 0x10000 in
      Right $
        ( pad4 (b16ToHexString (cast $ 0xD800 + (x' `shiftR` 10)))
        , pad4 (b16ToHexString (cast $ 0xDC00 + (x' .&. 0b1111111111))))

||| Encode an arbitrary unicode codepointby escaping it
||| as defined in
||| https://tools.ietf.org/id/draft-ietf-json-rfc4627bis-09.html#rfc.section.7
encodeCodepoint : Bits32 -> String
encodeCodepoint x =
  case encodeCodepointH x of
    Left w => "\\u" ++ w
    Right (w1, w2) => "\\u" ++ w1 ++ "\\u" ++ w2

||| Here we escape all wide characters (exceeding 8 bit width).
||| JSON spec doesn't seem to require that,
||| but at least some of the editors (e.g. Neovim) expect
||| wide characters escaped, otherwise refusing to work.
private
showChar : Char -> String
showChar c =
  case c of
       '\b' => "\\b"
       '\f' => "\\f"
       '\n' => "\\n"
       '\r' => "\\r"
       '\t' => "\\t"
       '\\' => "\\\\"
       '"'  => "\\\""
       c => if isControl c || c >= '\127'
               then encodeCodepoint (cast $ ord c)
               else singleton c

private
showString : String -> String
showString x = "\"" ++ concatMap showChar (unpack x) ++ "\""

export
stringify : JSON -> String
stringify JNull = "null"
stringify (JBoolean x) = if x then "true" else "false"
stringify (JNumber x) =
  let s = show x
   in if isSuffixOf ".0" s then substr 0 (length s `minus` 2) s else s
stringify (JString x) = showString x
stringify (JArray xs) = "[" ++ stringifyValues xs ++ "]"
  where
    stringifyValues : List JSON -> String
    stringifyValues [] = ""
    stringifyValues (x :: xs) =
      stringify x ++ if isNil xs then "" else "," ++ stringifyValues xs
stringify (JObject xs) = "{" ++ stringifyProps xs ++ "}"
  where
    stringifyProp : (String, JSON) -> String
    stringifyProp (key, value) = showString key ++ ":" ++ stringify value

    stringifyProps : List (String, JSON) -> String
    stringifyProps [] = ""
    stringifyProps (x :: xs) =
      stringifyProp x ++ if isNil xs then "" else "," ++ stringifyProps xs

export
pathToURI : String -> URI
pathToURI path =
  MkURI { scheme    = "file"
        , authority = Just (MkURIAuthority Nothing "" Nothing)
        , path      = path
        , query     = ""
        , fragment  = ""
        }

export
Show Position where
  show (MkPosition line character) = "\{show line}:\{show character}"

export
Show Range where
  show (MkRange start end) = "\{show start} -- \{show end}"

export
systemPathToURIPath : String -> String
systemPathToURIPath p = if not isWindows then p else
  let p1 = fastPack (map (\c => if c == '\\' then '/' else c) (fastUnpack p))
  in case strUncons p1 of
    Just ('/', _) => p1
    _ => strCons '/' p1

export
uriPathToSystemPath : String -> String
uriPathToSystemPath p = if not isWindows then p else
  let p1 = case strUncons p of
        Just ('/', tail) => tail
        _ => p
  in fastPack (map (\c => if c == '/' then '\\' else c) (fastUnpack p1))

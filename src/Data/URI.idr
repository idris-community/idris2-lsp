||| Module for parsing and handling of URIs.
|||
||| References:
|||   [1] https://tools.ietf.org/html/rfc3986
|||   [2] https://tools.ietf.org/html/rfc6874
|||   [3] https://tools.ietf.org/html/rfc7320
|||   [4] https://tools.ietf.org/html/rfc8820
|||
||| (C) The Idris Community, 2021
module Data.URI

import Data.Bits
import Data.DPair
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.String.Extra
import Data.String.Parser
import Data.Vect
import Protocol.Hex

listElem : Eq a => a -> List a -> Bool
listElem = elem

foldSeq : (Applicative f, Traversable t, Monoid m) => t (f m) -> f m
foldSeq = map concat . sequence

||| Type of an authority within URIs.
public export
record URIAuthority where
  constructor MkURIAuthority
  userInfo : Maybe String
  regName : String
  port : Maybe Nat

export
Show URIAuthority where
  show authority =
    let info = maybe "" (+> '@') authority.userInfo
        port = maybe "" ((':' <+) . show) authority.port
     in "\{info}\{authority.regName}\{port}"

export
Eq URIAuthority where
  x == y = show x == show y

export
Ord URIAuthority where
  compare x y = compare (show x) (show y)

||| Type for a general URI.
public export
record URI where
  constructor MkURI
  scheme : String
  authority : Maybe URIAuthority
  path : String
  query : String
  fragment : String

export
Show URI where
  show uri = 
    let auth = maybe "" ((++ "//") . show) uri.authority
        query = if uri.query == "" then "" else '?' <+ uri.query
        fragment = if uri.fragment == "" then "" else '#' <+ uri.fragment
     in "\{uri.scheme}:\{auth}\{uri.path}\{query}\{fragment}"

export
Eq URI where
  x == y = show x == show y

export
Ord URI where
  compare x y = compare (show x) (show y)

||| Returns true for characters that are allowed in a URI but do not have a reserved purpose.
|||
||| @see RFC 3986, section 2.3
isUnreserved : Char -> Bool
isUnreserved c = isAlphaNum c || (c `listElem` ['-', '.', '_', '~'])

isGenDelim : Char -> Bool
isGenDelim c = c `listElem` [':', '/', '?', '#', '[', ']', '@']

||| Returns true for reserved characters that requires to be percent escaped.
|||
||| @see RFC 3986, section 2.2
isSubDelim : Char -> Bool
isSubDelim c = c `listElem` ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']

isReserved : Char -> Bool
isReserved c = isGenDelim c || isSubDelim c

||| Parser for percent encoded characters.
|||
||| @see RFC 3986, section 2.1
pctEncoded : Parser Char
pctEncoded = do
  ignore $ char '%'
  x <- satisfy isHexDigit
  y <- satisfy isHexDigit
  let Just d = fromHexChars [y, x]
    | Nothing => fail "Cannot convert \{show x}\{show y} to a hex number"
  pure $ chr (cast d)

||| Parser for a URI scheme.
|||
||| @see RFC 3986, section 3.1
schemeParser : Parser String
schemeParser = pack <$> [| alphaNum :: many (satisfy isValidForScheme) |]
  where isValidForScheme : Char -> Bool
        isValidForScheme c = isAlphaNum c || (c `listElem` ['+', '-', '.'])

||| Parser for a URI userinfo subcomponent.
|||
||| @see RFC 3986, section 3.2.1
userInfoParser : Parser String
userInfoParser = pack <$> many (satisfy isUnreserved <|> char ':' <|> pctEncoded <|> satisfy isSubDelim)

||| Parser for potentially future version of IPs.
|||
||| @see RFC 3986, section 3.2.2
ipVFutureParser : Parser String
ipVFutureParser = with Prelude.(::)
  foldSeq [ string "v"
          , pack <$> some (satisfy isHexDigit)
          , string "."
          , pack <$> some (satisfy (\c => isUnreserved c || isSubDelim c || c == ':'))
          ]

||| Parser for an IPv4 address.
|||
||| @see RFC 3986, section 3.2.2
ipV4AddressParser : Parser String
ipV4AddressParser = with Prelude.(::) foldSeq [octet, string ".", octet, string ".", octet, string ".", octet]
  where
    octet : Parser String
    octet = integer >>= (\v => guard (0 <= v && v <= 255) *> pure (show v))

||| Parser for an IPv6 address.
|||
||| @see RFC 3986, section 3.2.2
ipV6AddressParser : Parser String
ipV6AddressParser = with Prelude.(::)
    -- This implementation follows closely the definition in the RFC 3986, so is a mess and surely can be improved :(
        multipleH16LS32 6
    <|> foldSeq [string "::", multipleH16LS32 5]
    <|> foldSeq [optionMap "" id h16, string "::", multipleH16LS32 4]
    <|> foldSeq [optionMap "" id (multipleH16 1), string "::", multipleH16LS32 3]
    <|> foldSeq [optionMap "" id (multipleH16 2), string "::", multipleH16LS32 2]
    <|> foldSeq [optionMap "" id (multipleH16 3), string "::", [| h16 +> char ':' |], ls32]
    <|> foldSeq [optionMap "" id (multipleH16 4), string "::", ls32]
    <|> foldSeq [optionMap "" id (multipleH16 5), string "::", h16]
    <|> foldSeq [optionMap "" id (multipleH16 6), string "::"]
  where
    h16 : Parser String
    h16 = do
      c <- satisfy isHexDigit
      o1 <- optional (satisfy isHexDigit)
      o2 <- optional (satisfy isHexDigit)
      o3 <- optional (satisfy isHexDigit)
      pure $ pack (c :: fromMaybe [] (sequence [o1, o2, o3]))

    ls32 : Parser String
    ls32 = [| [| h16 +> char ':' |] ++ h16 |] <|> ipV4AddressParser

    multipleH16 : Nat -> Parser String
    multipleH16 n = [| (concat . toList <$> ntimes n [| h16 +> char ':' |]) ++ h16 |]

    multipleH16LS32 : Nat -> Parser String
    multipleH16LS32 n = [| (concat . toList <$> ntimes n [| h16 +> char ':' |]) ++ ls32 |]

||| Parser for a zone indentifier for a IPv6 Scoped Address.
|||
||| @see RFC 6874, section 2
zoneIDParser : Parser String
zoneIDParser = pack <$> some (satisfy isUnreserved <|> pctEncoded)

||| Parser for a IPv6 Scoped Address.
|||
||| @see RFC 6874, section 2
ipV6AddrzParser : Parser String
ipV6AddrzParser = with Prelude.(::) concat <$> sequence [ipV6AddressParser, string "%25", zoneIDParser]

||| Parser for a IPv6 Scoped Address.
|||
||| @see RFC 3986, section 3.2.2
||| @see RFC 6874, section 2
ipLiteralParser : Parser String
ipLiteralParser = char '[' *> (ipV6AddressParser <|> ipV6AddrzParser <|> ipVFutureParser) <* char ']'

||| Parser for a registered name.
|||
||| @see RFC 3986, section 3.2.2
regNameParser : Parser String
regNameParser = pack <$> many (satisfy isUnreserved <|> pctEncoded <|> satisfy isSubDelim)

||| Parser for a URI authority.
|||
||| @see RFC 3986, section 3.2
authorityParser : Parser URIAuthority
authorityParser = do
  ignore $ string "//"
  userInfo <- optional $ userInfoParser <* char '@'
  host <- ipLiteralParser <|> ipV4AddressParser <|> regNameParser
  port <- optional $ char ':' *> natural
  pure $ MkURIAuthority userInfo host port

||| Parser for a valid character in a path.
|||
||| @see RFC 3986, section 3.3
pchar : Parser Char
pchar = satisfy (\c => isUnreserved c || isSubDelim c) <|> pctEncoded <|> char ':' <|> char '@'

||| Parser for a segment in a path.
|||
||| @see RFC 3986, section 3.3
segment : Parser String
segment = pack <$> many pchar

||| Parser for a non-empty segment in a path.
|||
||| @see RFC 3986, section 3.3
segmentNZ : Parser String
segmentNZ = pack <$> some pchar

||| Parser for a non-empty segment in a path that does not start with a colon.
|||
||| @see RFC 3986, section 3.3
segmentNZNC : Parser String
segmentNZNC = pack <$> some (satisfy (\c => isUnreserved c || isSubDelim c) <|> pctEncoded <|> char '@')

||| Parser for an absolute or empty path.
|||
||| @see RFC 3986, section 3.3
abempty : Parser String
abempty = concat <$> many [| char '/' <+ segment |]

||| Parser for an absolute path.
|||
||| @see RFC 3986, section 3.3
absolute : Parser String
absolute = [| char '/' <+ optionMap "" id [| segmentNZ ++ (concat <$> many [| char '/' <+ segment |]) |] |]

||| Parser for path that starts without a colon.
|||
||| @see RFC 3986, section 3.3
noscheme : Parser String
noscheme = [| segmentNZNC ++ (concat <$> many [| char '/' <+ segment |]) |]

||| Parser for path that starts with a segment.
|||
||| @see RFC 3986, section 3.3
rootless : Parser String
rootless = [| segmentNZ ++ (concat <$> many [| char '/' <+ segment |]) |]

||| Parser for a, possibly empty, path.
|||
||| @see RFC 3986, section 3.3
pathParser : Parser String
pathParser = absolute <|> noscheme <|> rootless <|> abempty <|> pure ""

||| Parser for a query component.
|||
||| @see RFC 3986, section 3.4
queryParser : Parser String
queryParser = pack <$> many (pchar <|> char '/' <|> char '?')

||| Parser for a fragment identifier component.
|||
||| @see RFC 3986, section 3.5
fragmentParser : Parser String
fragmentParser = pack <$> many (pchar <|> char '/' <|> char '?')

||| Parser for a generic URI.
|||
||| @see RFC 3986, section 3
export
uriParser : Parser URI
uriParser = do
  scheme <- schemeParser
  ignore $ char ':'
  (authority, path) <- [| MkPair (Just <$> authorityParser) abempty |] <|> ((Nothing,) <$> (absolute <|> rootless <|> empty))
  query <- optionMap "" id (char '?' *> queryParser)
  fragment <- optionMap "" id (char '#' *> fragmentParser)
  pure $ MkURI scheme authority path query fragment

||| Parser for a URI relative reference.
|||
||| @see RFC 3986, section 4.2
relativeReferenceParser : Parser URI
relativeReferenceParser = do
  (authority, path) <- [| MkPair (Just <$> authorityParser) abempty |] <|> ((Nothing,) <$> (absolute <|> noscheme <|> empty))
  query <- optionMap "" id (char '?' *> queryParser)
  fragment <- optionMap "" id (char '#' *> fragmentParser)
  pure $ MkURI "" authority path query fragment

||| Parser for a URI reference.
|||
||| @see RFC 3986, section 4.1
export
uriReferenceParser : Parser URI
uriReferenceParser = uriParser <|> relativeReferenceParser

||| Parser for an absolute URI without a fragment identifier.
|||
||| @see RFC 3986, section 4.3
export
uriAbsoluteParser : Parser URI
uriAbsoluteParser = do
  scheme <- schemeParser
  ignore $ char ':'
  (authority, path) <- [| MkPair (Just <$> authorityParser) abempty |] <|> ((Nothing,) <$> (absolute <|> rootless <|> empty))
  query <- optionMap "" id (char '?' *> queryParser)
  pure $ MkURI scheme authority path query ""

export
uriIsAbsolute : URI -> Bool
uriIsAbsolute (MkURI {scheme, _}) = scheme /= ""

export
uriIsRelative : URI -> Bool
uriIsRelative = not . uriIsAbsolute

export
isUnescapedInURI : Char -> Bool
isUnescapedInURI c = isReserved c || isUnreserved c

export
isUnescapedInURIComponent : Char -> Bool
isUnescapedInURIComponent c = not (isReserved c || not (isUnescapedInURI c))

utf8EncodeChar : Char -> List Int
utf8EncodeChar = go . ord
  where
    go : Int -> List Int
    go x = if x <= 0x7f then [x]
           else if x <= 0x7ff then [ 0xc0 + (x `shiftR` 6)
                                   , 0x80 + x .&. 0x3f
                                   ]
           else if x <= 0xffff then [ 0xe0 + (x `shiftR` 12)
                                    , 0x80 + ((x `shiftR` 6) .&. 0x3f)
                                    , 0x80 + x .&. 0x3f
                                    ]
           else [ 0xf0 + (x `shiftR` 18)
                , 0x80 + ((x `shiftR` 12) .&. 0x3f)
                , 0x80 + ((x `shiftR` 6) .&. 0x3f)
                , 0x80 + x .&. 0x3f
                ]

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
    other => assert_total $ b16ToHexString (n `shiftR` 4) ++ b16ToHexString (n .&. 15)

pad2 : String -> String
pad2 str =
  case length str of
    0 => "00"
    1 => strCons '0' str
    _ => str

export
escapeURIChar : (Char -> Bool) -> Char -> String
escapeURIChar p c =
  if p c
     then cast c
     else concatMap (strCons '%' . pad2 . b16ToHexString . cast) (utf8EncodeChar c)

export
escapeURIString : (Char -> Bool) -> String -> String
escapeURIString p = foldl (\acc, c => acc ++ escapeURIChar p c) "" . unpack

export
escapeAndParseURI : String -> Either String URI
escapeAndParseURI = mapSnd fst . parse (uriReferenceParser <* eos) . escapeURIString isUnescapedInURI

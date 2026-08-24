module Language.LSP.Message.URI

import Data.Either
import Data.String.Parser
import public Data.URI
import Language.LSP.Message.Utils
import Data.SortedMap

%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#uri
public export
DocumentURI : Type
DocumentURI = URI

export
ToJSON URI where
  toJSON = JString . show

export covering
parseJSONURI : Parser String URI
parseJSONURI str = mapFst ([],) (fst <$> parse (uriParser <* eos) str)

export covering
FromJSON URI where
  fromJSON (JString str) = parseJSONURI str
  fromJSON _ = Left neutral

-- URI is a valid key for parsing dictionaries/SortedMap
export covering
FromJSONKey URI where
  fromKey = parseJSONURI

module Language.LSP.Message.URI

import Data.Either
import Data.String.Parser
import public Data.URI
import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Utils

%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#uri
public export
DocumentURI : Type
DocumentURI = URI

export
ToJSON URI where
  toJSON = JString . show

export covering
FromJSON URI where
  fromJSON (JString str) = eitherToMaybe (fst <$> parse (uriParser <* eos) str)
  fromJSON _ = neutral

export
ToJSON v => ToJSON (SortedMap URI v) where
  toJSON m = JObject (map (mapFst show) $ toList $ toJSON <$> m)

export covering
FromJSON v => FromJSON (SortedMap URI v) where
  fromJSON (JObject xs) = fromList <$> traverse (\(k, v) => (,) <$> fromJSON (JString k) <*> fromJSON v) xs
  fromJSON _ = neutral


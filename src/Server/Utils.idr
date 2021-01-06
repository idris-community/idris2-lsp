||| Common types and utility funtions for the LSP server implementation.
|||
||| (C) The Idris Community, 2021
module Server.Utils

import Core.Core
import Data.Strings
import System.File

||| Gets a specific component of a reference, using the supplied projection.
export
gets : (l : label) -> Ref l a => (a -> b) -> Core b
gets l f = f <$> get l

||| Reads a single header from an LSP message on the supplied file handle.
||| Headers end with the string "\r\n".
export
fGetHeader : (h : File) -> Core (Either FileError String)
fGetHeader handle = do
  Right l <- coreLift $ fGetLine handle
    | Left err => pure $ Left err
  -- TODO: reading up to a string should probably be handled directly by the FFI primitive
  --       or at least in a more efficient way in Idris2
  if isSuffixOf "\r\n" l
     then pure $ Right l
     else (map (l ++)) <$> fGetHeader handle

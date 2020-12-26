module Language.LSP.Message.URI

%default total

-- TODO: Define helper functions for URIs
||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#uri
public export
URI : Type
URI = String

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#uri
public export
DocumentURI : Type
DocumentURI = String

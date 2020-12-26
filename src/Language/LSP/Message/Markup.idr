module Language.LSP.Message.Markup

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

namespace MarkupKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#markupContent
  public export
  data MarkupKind = PlainText | Markdown

export
ToJSON MarkupKind where
  toJSON PlainText = JString "plaintext"
  toJSON Markdown  = JString "markdown"

export
FromJSON MarkupKind where
  fromJSON (JString "plaintext") = pure PlainText
  fromJSON (JString "markdown")  = pure Markdown
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#markupContent
public export
record MarkupContent where
  constructor MkMarkupContent
  kind : MarkupKind
  value : String
%runElab deriveJSON defaultOpts `{{MarkupContent}}

namespace MarkedString
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
  public export
  record MarkedStringWithLanguage where
    constructor MkMarkedStringWithLanguage
    language : String
    value : String
  %runElab deriveJSON defaultOpts `{{MarkedStringWithLanguage}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_hover
public export
MarkedString : Type
MarkedString = String .+. MarkedStringWithLanguage

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#markupContent
public export
record MarkdownClientCapabilities where
  constructor MkMarkdownClientCapabilities
  parser  : String
  version : Maybe String
%runElab deriveJSON defaultOpts `{{MarkdownClientCapabilities}}

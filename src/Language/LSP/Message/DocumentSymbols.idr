module Language.LSP.Message.DocumentSymbols

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

namespace SymbolKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
  public export
  data SymbolKind
    = File
    | Module
    | Namespace
    | Package
    | Class
    | Method
    | Property
    | Field
    | Constructor
    | Enum
    | Interface
    | Function
    | Variable
    | Constant
    | String_
    | Number
    | Boolean
    | Array
    | Object
    | Key
    | Null
    | EnumMember
    | Struct
    | Event
    | Operator
    | TypeParameter

export
ToJSON SymbolKind where
  toJSON File          = JNumber 1
  toJSON Module        = JNumber 2
  toJSON Namespace     = JNumber 3
  toJSON Package       = JNumber 4
  toJSON Class         = JNumber 5
  toJSON Method        = JNumber 6
  toJSON Property      = JNumber 7
  toJSON Field         = JNumber 8
  toJSON Constructor   = JNumber 9
  toJSON Enum          = JNumber 10
  toJSON Interface     = JNumber 11
  toJSON Function      = JNumber 12
  toJSON Variable      = JNumber 13
  toJSON Constant      = JNumber 14
  toJSON String_       = JNumber 15
  toJSON Number        = JNumber 16
  toJSON Boolean       = JNumber 17
  toJSON Array         = JNumber 18
  toJSON Object        = JNumber 19
  toJSON Key           = JNumber 20
  toJSON Null          = JNumber 21
  toJSON EnumMember    = JNumber 22
  toJSON Struct        = JNumber 23
  toJSON Event         = JNumber 24
  toJSON Operator      = JNumber 25
  toJSON TypeParameter = JNumber 26

export
FromJSON SymbolKind where
  fromJSON (JNumber 1)  = pure File
  fromJSON (JNumber 2)  = pure Module
  fromJSON (JNumber 3)  = pure Namespace
  fromJSON (JNumber 4)  = pure Package
  fromJSON (JNumber 5)  = pure Class
  fromJSON (JNumber 6)  = pure Method
  fromJSON (JNumber 7)  = pure Property
  fromJSON (JNumber 8)  = pure Field
  fromJSON (JNumber 9)  = pure Constructor
  fromJSON (JNumber 10) = pure Enum
  fromJSON (JNumber 11) = pure Interface
  fromJSON (JNumber 12) = pure Function
  fromJSON (JNumber 13) = pure Variable
  fromJSON (JNumber 14) = pure Constant
  fromJSON (JNumber 15) = pure String_
  fromJSON (JNumber 16) = pure Number
  fromJSON (JNumber 17) = pure Boolean
  fromJSON (JNumber 18) = pure Array
  fromJSON (JNumber 19) = pure Object
  fromJSON (JNumber 20) = pure Key
  fromJSON (JNumber 21) = pure Null
  fromJSON (JNumber 22) = pure EnumMember
  fromJSON (JNumber 23) = pure Struct
  fromJSON (JNumber 24) = pure Event
  fromJSON (JNumber 25) = pure Operator
  fromJSON (JNumber 26) = pure TypeParameter
  fromJSON _ = Nothing

namespace SymbolTag
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
  public export
  data SymbolTag = Deprecated

export
ToJSON SymbolTag where
  toJSON Deprecated = JNumber 1

export
FromJSON SymbolTag where
  fromJSON (JNumber 1) = pure Deprecated
  fromJSON _ = Nothing

namespace DocumentSymbolClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
  public export
  record DocumentSymbolKind where
    constructor MkDocumentSymbolKind
    valueSet : Maybe (List SymbolKind)
  %runElab deriveJSON defaultOpts `{{DocumentSymbolKind}}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
  public export
  record DocumentSymbolTag where
    constructor MkDocumentSymbolTag
    valueSet : Maybe (List SymbolTag)
  %runElab deriveJSON defaultOpts `{{DocumentSymbolTag}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
public export
record DocumentSymbolClientCapabilities where
  constructor MkDocumentSymbolClientCapabilities
  dynamicRegistration               : Maybe Bool
  symbolKind                        : Maybe DocumentSymbolKind
  hierarchicalDocumentSymbolSupport : Maybe Bool
  tagSupport                        : Maybe DocumentSymbolTag
  labelSupport                      : Maybe Bool
%runElab deriveJSON defaultOpts `{{DocumentSymbolClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
public export
record DocumentSymbolOptions where
  constructor MkDocumentSymbolOptions
  workDoneProgress : Maybe Bool
  label            : Maybe String
%runElab deriveJSON defaultOpts `{{DocumentSymbolOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
public export
record DocumentSymbolRegistrationOptions where
  constructor MkDocumentSymbolRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : DocumentSelector .+. Null
  label            : Maybe String
%runElab deriveJSON defaultOpts `{{DocumentSymbolRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
public export
record DocumentSymbolParams where
  constructor MkDocumentSymbolParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
%runElab deriveJSON defaultOpts `{{DocumentSymbolParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
public export
record DocumentSymbol where
  constructor MkDocumentSymbol
  name           : String
  detail         : Maybe String
  kind           : SymbolKind
  tags           : Maybe (List SymbolTag)
  deprecated     : Maybe Bool
  range          : Range
  selectionRange : Range
  children       : Maybe (List (Inf DocumentSymbol))

export -- FIXME?: Can I Avoid assert_total? Try indexing it with the depth
ToJSON DocumentSymbol where
  toJSON (MkDocumentSymbol name detail kind tags deprecated range selectionRange children) = assert_total $
    JObject (catMaybes [ Just ("name", toJSON name)
                       , (MkPair "detail" . toJSON) <$> detail
                       , Just ("kind", toJSON kind)
                       , (MkPair "tags" . toJSON) <$> tags
                       , Just ("deprecated", toJSON deprecated)
                       , Just ("range", toJSON range)
                       , Just ("selectionRange", toJSON selectionRange)
                       , (MkPair "children" . toJSON) <$> children
                       ])

export covering
FromJSON DocumentSymbol where
  fromJSON (JObject arg) =
    pure MkDocumentSymbol <*> (lookup "name" arg >>= fromJSON)
                          <*> (pure $ lookup "detail" arg >>= fromJSON)
                          <*> (lookup "kind" arg >>= fromJSON)
                          <*> (pure $ lookup "tags" arg >>= fromJSON)
                          <*> (pure $ lookup "deprecated" arg >>= fromJSON)
                          <*> (lookup "range" arg >>= fromJSON)
                          <*> (lookup "selectionRange" arg >>= fromJSON)
                          <*> (pure $ lookup "children" arg >>= fromJSON)
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
public export
record SymbolInformation where
  constructor MkSymbolInformation
  name          : String
  kind          : SymbolKind
  tags          : Maybe (List SymbolTag)
  deprecated    : Maybe Bool
  location      : Location
  containerName : Maybe String
%runElab deriveJSON defaultOpts `{{SymbolInformation}}

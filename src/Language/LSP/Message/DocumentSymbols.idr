module Language.LSP.Message.DocumentSymbols

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection
import Data.Either

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
  toJSON File          = JInteger 1
  toJSON Module        = JInteger 2
  toJSON Namespace     = JInteger 3
  toJSON Package       = JInteger 4
  toJSON Class         = JInteger 5
  toJSON Method        = JInteger 6
  toJSON Property      = JInteger 7
  toJSON Field         = JInteger 8
  toJSON Constructor   = JInteger 9
  toJSON Enum          = JInteger 10
  toJSON Interface     = JInteger 11
  toJSON Function      = JInteger 12
  toJSON Variable      = JInteger 13
  toJSON Constant      = JInteger 14
  toJSON String_       = JInteger 15
  toJSON Number        = JInteger 16
  toJSON Boolean       = JInteger 17
  toJSON Array         = JInteger 18
  toJSON Object        = JInteger 19
  toJSON Key           = JInteger 20
  toJSON Null          = JInteger 21
  toJSON EnumMember    = JInteger 22
  toJSON Struct        = JInteger 23
  toJSON Event         = JInteger 24
  toJSON Operator      = JInteger 25
  toJSON TypeParameter = JInteger 26

export
FromJSON SymbolKind where
  fromJSON (JInteger 1)  = pure File
  fromJSON (JInteger 2)  = pure Module
  fromJSON (JInteger 3)  = pure Namespace
  fromJSON (JInteger 4)  = pure Package
  fromJSON (JInteger 5)  = pure Class
  fromJSON (JInteger 6)  = pure Method
  fromJSON (JInteger 7)  = pure Property
  fromJSON (JInteger 8)  = pure Field
  fromJSON (JInteger 9)  = pure Constructor
  fromJSON (JInteger 10) = pure Enum
  fromJSON (JInteger 11) = pure Interface
  fromJSON (JInteger 12) = pure Function
  fromJSON (JInteger 13) = pure Variable
  fromJSON (JInteger 14) = pure Constant
  fromJSON (JInteger 15) = pure String_
  fromJSON (JInteger 16) = pure Number
  fromJSON (JInteger 17) = pure Boolean
  fromJSON (JInteger 18) = pure Array
  fromJSON (JInteger 19) = pure Object
  fromJSON (JInteger 20) = pure Key
  fromJSON (JInteger 21) = pure Null
  fromJSON (JInteger 22) = pure EnumMember
  fromJSON (JInteger 23) = pure Struct
  fromJSON (JInteger 24) = pure Event
  fromJSON (JInteger 25) = pure Operator
  fromJSON (JInteger 26) = pure TypeParameter
  fromJSON _ = Left neutral

namespace SymbolTag
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
  public export
  data SymbolTag = Deprecated

export
ToJSON SymbolTag where
  toJSON Deprecated = JInteger 1

export
FromJSON SymbolTag where
  fromJSON (JInteger 1) = pure Deprecated
  fromJSON _ = fail "expected 1 for symbol tag"

namespace DocumentSymbolClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
  public export
  record DocumentSymbolKind where
    constructor MkDocumentSymbolKind
    valueSet : Maybe (List SymbolKind)
  %runElab derive "DocumentSymbolKind" [FromJSON, ToJSON]

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
  public export
  record DocumentSymbolTag where
    constructor MkDocumentSymbolTag
    valueSet : Maybe (List SymbolTag)
  %runElab derive "DocumentSymbolTag" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
public export
record DocumentSymbolClientCapabilities where
  constructor MkDocumentSymbolClientCapabilities
  dynamicRegistration               : Maybe Bool
  symbolKind                        : Maybe DocumentSymbolKind
  hierarchicalDocumentSymbolSupport : Maybe Bool
  tagSupport                        : Maybe DocumentSymbolTag
  labelSupport                      : Maybe Bool
%runElab derive "DocumentSymbolClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
public export
record DocumentSymbolOptions where
  constructor MkDocumentSymbolOptions
  workDoneProgress : Maybe Bool
  label            : Maybe String
%runElab derive "DocumentSymbolOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
public export
record DocumentSymbolRegistrationOptions where
  constructor MkDocumentSymbolRegistrationOptions
  workDoneProgress : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
  label            : Maybe String
%runElab derive "DocumentSymbolRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_documentSymbol
public export
record DocumentSymbolParams where
  constructor MkDocumentSymbolParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
%runElab derive "DocumentSymbolParams" [FromJSON, ToJSON]

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
  children       : Maybe (List DocumentSymbol)

%runElab derive "DocumentSymbol" [FromJSON, ToJSON]

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
%runElab derive "SymbolInformation" [FromJSON, ToJSON]

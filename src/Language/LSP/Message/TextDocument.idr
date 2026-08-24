module Language.LSP.Message.TextDocument

import Language.LSP.Message.Location
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%hide Text.Bounds.Position
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocumentIdentifier
public export
record TextDocumentIdentifier where
  constructor MkTextDocumentIdentifier
  uri : DocumentURI
%runElab derive "TextDocumentIdentifier" [FromJSON, ToJSON]

export
FromJSONKey TextDocumentIdentifier where
  fromKey textId = MkTextDocumentIdentifier <$> fromKey textId

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#versionedTextDocumentIdentifier
public export
record VersionedTextDocumentIdentifier where
  constructor MkVersionedTextDocumentIdentifier
  uri     : DocumentURI
  version : Int
%runElab derive "VersionedTextDocumentIdentifier" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#versionedTextDocumentIdentifier
public export
record OptionalVersionedTextDocumentIdentifier where
  constructor MkOptionalVersionedTextDocumentIdentifier
  uri     : DocumentURI
  version : Maybe Int
%runElab derive "OptionalVersionedTextDocumentIdentifier" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocumentItem
public export
record TextDocumentItem where
  constructor MkTextDocumentItem
  uri        : DocumentURI
  languageId : String
  version    : Int
  text       : String
%runElab derive "TextDocumentItem" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocumentPositionParams
public export
record TextDocumentPositionParams where
  constructor MkTextDocumentPositionParams
  textDocument : TextDocumentIdentifier
  position     : Position
%runElab derive "TextDocumentPositionParams" [FromJSON, ToJSON]

namespace TextDocumentSyncKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_synchronization
  public export
  data TextDocumentSyncKind = None | Full | Incremental

export
ToJSON TextDocumentSyncKind where
  toJSON None        = JInteger 0
  toJSON Full        = JInteger 1
  toJSON Incremental = JInteger 2

export
FromJSON TextDocumentSyncKind where
  fromJSON (JInteger 0) = pure None
  fromJSON (JInteger 1) = pure Full
  fromJSON (JInteger 2) = pure Incremental
  fromJSON _ = Left neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didOpen
public export
record DidOpenTextDocumentParams where
  constructor MkDidOpenTextDocumentParams
  textDocument : TextDocumentItem
%runElab derive "DidOpenTextDocumentParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#documentFilter
public export
record DocumentFilter where
  constructor MkDocumentFilter
  language : Maybe String
  scheme   : Maybe String
  pattern  : Maybe String
%runElab derive "DocumentFilter" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#documentFilter
public export
DocumentSelector : Type
DocumentSelector = List DocumentFilter

public export
record TextDocumentRegistrationOptions where
  constructor MkTextDocumentRegistrationOptions
  documentSelector : OneOf [DocumentSelector, Null]
%runElab derive "TextDocumentRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocumentRegistrationOptions
public export
record TextDocumentChangeRegistrationOptions where
  constructor MkTextDocumentChangeRegistrationOptions
  syncKind : TextDocumentSyncKind
%runElab derive "TextDocumentChangeRegistrationOptions" [FromJSON, ToJSON]

namespace DidChangeTextDocumentParams
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didChange
  public export
  record TextDocumentContentChangeEvent where
    constructor MkTextDocumentContentChangeEvent
    text : String
  %runElab derive "TextDocumentContentChangeEvent" [FromJSON, ToJSON]

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didChange
  public export
  record TextDocumentContentChangeEventWithRange where
    constructor MkTextDocumentContentChangeEventWithRange
    range       : Range
    rangeLength : Maybe Int
    text        : String
  %runElab derive "TextDocumentContentChangeEventWithRange" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didChange
public export
record DidChangeTextDocumentParams where
  constructor MkDidChangeTextDocumentParams
  textDocument   : VersionedTextDocumentIdentifier
  contentChanges : List (OneOf [TextDocumentContentChangeEvent, TextDocumentContentChangeEventWithRange])
%runElab derive "DidChangeTextDocumentParams" [FromJSON, ToJSON]

namespace TextDocumentSaveReason
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_willSave
  public export
  data TextDocumentSaveReason = Manual | AfterDelay | FocusOut

export
ToJSON TextDocumentSaveReason where
  toJSON Manual     = JInteger 1
  toJSON AfterDelay = JInteger 2
  toJSON FocusOut   = JInteger 3

export
FromJSON TextDocumentSaveReason where
  fromJSON (JInteger 1) = pure Manual
  fromJSON (JInteger 2) = pure AfterDelay
  fromJSON (JInteger 3) = pure FocusOut
  fromJSON _ = Left neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_willSave
public export
record WillSaveTextDocumentParams where
  constructor MkWillSaveTextDocumentParams
  textDocument : TextDocumentIdentifier
  reason       : TextDocumentSaveReason
%runElab derive "WillSaveTextDocumentParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didSave
public export
record SaveOptions where
  constructor MkSaveOptions
  includeText : Maybe Bool
%runElab derive "SaveOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didSave
public export
record TextDocumentSaveRegistrationOptions where
  constructor MkTextDocumentSaveRegistrationOptions
  documentSelector : OneOf [DocumentSelector, Null]
  includeText      : Maybe Bool
%runElab derive "TextDocumentSaveRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didSave
public export
record DidSaveTextDocumentParams where
  constructor MkDidSaveTextDocumentParams
  textDocument : TextDocumentIdentifier
  text         : Maybe String
%runElab derive "DidSaveTextDocumentParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didClose
public export
record DidCloseTextDocumentParams where
  constructor MkDidCloseTextDocumentParams
  textDocument : TextDocumentIdentifier
%runElab derive "DidCloseTextDocumentParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didClose
public export
record TextDocumentSyncClientCapabilities where
  constructor MkTextDocumentSyncClientCapabilities
  dynamicRegistration : Maybe Bool
  willSave            : Maybe Bool
  willSaveWaitUntil   : Maybe Bool
  didSave             : Maybe Bool
%runElab derive "TextDocumentSyncClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didClose
public export
record TextDocumentSyncOptions where
  constructor MkTextDocumentSyncOptions
  openClose         : Maybe Bool
  change            : Maybe TextDocumentSyncKind
  willSave          : Maybe Bool
  willSaveWaitUntil : Maybe Bool
  save              : Maybe (OneOf [Bool, SaveOptions])
%runElab derive "TextDocumentSyncOptions" [FromJSON, ToJSON]

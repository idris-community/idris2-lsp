module Language.LSP.Message.TextDocument

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocumentIdentifier
public export
record TextDocumentIdentifier where
  constructor MkTextDocumentIdentifier
  uri : DocumentURI
%runElab deriveJSON defaultOpts `{{TextDocumentIdentifier}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#versionedTextDocumentIdentifier
public export
record VersionedTextDocumentIdentifier where
  constructor MkVersionedTextDocumentIdentifier
  uri     : DocumentURI
  version : Int
%runElab deriveJSON defaultOpts `{{VersionedTextDocumentIdentifier}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#versionedTextDocumentIdentifier
public export
record OptionalVersionedTextDocumentIdentifier where
  constructor MkOptionalVersionedTextDocumentIdentifier
  uri     : DocumentURI
  version : Maybe Int
%runElab deriveJSON defaultOpts `{{OptionalVersionedTextDocumentIdentifier}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocumentItem
public export
record TextDocumentItem where
  constructor MkTextDocumentItem
  uri        : DocumentURI
  languageId : String
  version    : Int
  text       : String
%runElab deriveJSON defaultOpts `{{TextDocumentItem}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocumentPositionParams
public export
record TextDocumentPositionParams where
  constructor MkTextDocumentPositionParams
  textDocument : TextDocumentIdentifier
  position     : Position
%runElab deriveJSON defaultOpts `{{TextDocumentPositionParams}}

namespace TextDocumentSyncKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_synchronization
  public export
  data TextDocumentSyncKind = None | Full | Incremental

export
ToJSON TextDocumentSyncKind where
  toJSON None        = JNumber 0
  toJSON Full        = JNumber 1
  toJSON Incremental = JNumber 2

export
FromJSON TextDocumentSyncKind where
  fromJSON (JNumber 0) = pure None
  fromJSON (JNumber 1) = pure Full
  fromJSON (JNumber 2) = pure Incremental
  fromJSON _ = Nothing

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didOpen
public export
record DidOpenTextDocumentParams where
  constructor MkDidOpenTextDocumentParams
  textDocument : TextDocumentItem
%runElab deriveJSON defaultOpts `{{DidOpenTextDocumentParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#documentFilter
public export
record DocumentFilter where
  constructor MkDocumentFilter
  language : Maybe String
  scheme   : Maybe String
  pattern  : Maybe String
%runElab deriveJSON defaultOpts `{{DocumentFilter}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#documentFilter
public export
DocumentSelector : Type
DocumentSelector = List DocumentFilter

public export
record TextDocumentRegistrationOptions where
  constructor MkTextDocumentRegistrationOptions
  documentSelector : OneOf [DocumentSelector, Null]
%runElab deriveJSON defaultOpts `{{TextDocumentRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocumentRegistrationOptions
public export
record TextDocumentChangeRegistrationOptions where
  constructor MkTextDocumentChangeRegistrationOptions
  syncKind : TextDocumentSyncKind
%runElab deriveJSON defaultOpts `{{TextDocumentChangeRegistrationOptions}}

namespace DidChangeTextDocumentParams
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didChange
  public export
  record TextDocumentContentChangeEvent where
    constructor MkTextDocumentContentChangeEvent
    text : String
  %runElab deriveJSON defaultOpts `{{TextDocumentContentChangeEvent}}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didChange
  public export
  record TextDocumentContentChangeEventWithRange where
    constructor MkTextDocumentContentChangeEventWithRange
    range       : Range
    rangeLength : Maybe Int
    text        : String
  %runElab deriveJSON defaultOpts `{{TextDocumentContentChangeEventWithRange}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didChange
public export
record DidChangeTextDocumentParams where
  constructor MkDidChangeTextDocumentParams
  textDocument   : VersionedTextDocumentIdentifier
  contentChanges : List (OneOf [TextDocumentContentChangeEvent, TextDocumentContentChangeEventWithRange])
%runElab deriveJSON defaultOpts `{{DidChangeTextDocumentParams}}

namespace TextDocumentSaveReason
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_willSave
  public export
  data TextDocumentSaveReason = Manual | AfterDelay | FocusOut

export
ToJSON TextDocumentSaveReason where
  toJSON Manual     = JNumber 1
  toJSON AfterDelay = JNumber 2
  toJSON FocusOut   = JNumber 3

export
FromJSON TextDocumentSaveReason where
  fromJSON (JNumber 1) = pure Manual
  fromJSON (JNumber 2) = pure AfterDelay
  fromJSON (JNumber 3) = pure FocusOut
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_willSave
public export
record WillSaveTextDocumentParams where
  constructor MkWillSaveTextDocumentParams
  textDocument : TextDocumentIdentifier
  reason       : TextDocumentSaveReason
%runElab deriveJSON defaultOpts `{{WillSaveTextDocumentParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didSave
public export
record SaveOptions where
  constructor MkSaveOptions
  includeText : Maybe Bool
%runElab deriveJSON defaultOpts `{{SaveOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didSave
public export
record TextDocumentSaveRegistrationOptions where
  constructor MkTextDocumentSaveRegistrationOptions
  documentSelector : OneOf [DocumentSelector, Null]
  includeText      : Maybe Bool
%runElab deriveJSON defaultOpts `{{TextDocumentSaveRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didSave
public export
record DidSaveTextDocumentParams where
  constructor MkDidSaveTextDocumentParams
  textDocument : TextDocumentIdentifier
  text         : Maybe String
%runElab deriveJSON defaultOpts `{{DidSaveTextDocumentParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didClose
public export
record DidCloseTextDocumentParams where
  constructor MkDidCloseTextDocumentParams
  textDocument : TextDocumentIdentifier
%runElab deriveJSON defaultOpts `{{DidCloseTextDocumentParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didClose
public export
record TextDocumentSyncClientCapabilities where
  constructor MkTextDocumentSyncClientCapabilities
  dynamicRegistration : Maybe Bool
  willSave            : Maybe Bool
  willSaveWaitUntil   : Maybe Bool
  didSave             : Maybe Bool
%runElab deriveJSON defaultOpts `{{TextDocumentSyncClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_didClose
public export
record TextDocumentSyncOptions where
  constructor MkTextDocumentSyncOptions
  openClose         : Maybe Bool
  change            : Maybe TextDocumentSyncKind
  willSave          : Maybe Bool
  willSaveWaitUntil : Maybe Bool
  save              : Maybe (OneOf [Bool, SaveOptions])
%runElab deriveJSON defaultOpts `{{TextDocumentSyncOptions}}

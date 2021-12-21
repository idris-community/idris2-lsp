module Language.LSP.Message.Completion

import Language.JSON
import Language.LSP.Message.Command
import Language.LSP.Message.Derive
import Language.LSP.Message.Location
import Language.LSP.Message.Markup
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.LSP.Message.Workspace
import Language.Reflection

%language ElabReflection
%default total

namespace CompletionItemKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  data CompletionItemKind
    = Text
    | Method
    | Function
    | Constructor
    | Field
    | Variable
    | Class
    | Interface
    | Module
    | Property
    | Unit_
    | Value
    | Enum
    | Keyword
    | Snippet
    | Color
    | File
    | Reference
    | Folder
    | EnumMember
    | Constant
    | Struct
    | Event
    | Operator
    | TypeParameter

export
ToJSON CompletionItemKind where
  toJSON Text          = JNumber 1
  toJSON Method        = JNumber 2
  toJSON Function      = JNumber 3
  toJSON Constructor   = JNumber 4
  toJSON Field         = JNumber 5
  toJSON Variable      = JNumber 6
  toJSON Class         = JNumber 7
  toJSON Interface     = JNumber 8
  toJSON Module        = JNumber 9
  toJSON Property      = JNumber 10
  toJSON Unit_         = JNumber 11
  toJSON Value         = JNumber 12
  toJSON Enum          = JNumber 13
  toJSON Keyword       = JNumber 14
  toJSON Snippet       = JNumber 15
  toJSON Color         = JNumber 16
  toJSON File          = JNumber 17
  toJSON Reference     = JNumber 18
  toJSON Folder        = JNumber 19
  toJSON EnumMember    = JNumber 20
  toJSON Constant      = JNumber 21
  toJSON Struct        = JNumber 22
  toJSON Event         = JNumber 23
  toJSON Operator      = JNumber 24
  toJSON TypeParameter = JNumber 25

export
FromJSON CompletionItemKind where
  fromJSON (JNumber 1)  = pure Text
  fromJSON (JNumber 2)  = pure Method
  fromJSON (JNumber 3)  = pure Function
  fromJSON (JNumber 4)  = pure Constructor
  fromJSON (JNumber 5)  = pure Field
  fromJSON (JNumber 6)  = pure Variable
  fromJSON (JNumber 7)  = pure Class
  fromJSON (JNumber 8)  = pure Interface
  fromJSON (JNumber 9)  = pure Module
  fromJSON (JNumber 10) = pure Property
  fromJSON (JNumber 11) = pure Unit_
  fromJSON (JNumber 12) = pure Value
  fromJSON (JNumber 13) = pure Enum
  fromJSON (JNumber 14) = pure Keyword
  fromJSON (JNumber 15) = pure Snippet
  fromJSON (JNumber 16) = pure Color
  fromJSON (JNumber 17) = pure File
  fromJSON (JNumber 18) = pure Reference
  fromJSON (JNumber 19) = pure Folder
  fromJSON (JNumber 20) = pure EnumMember
  fromJSON (JNumber 21) = pure Constant
  fromJSON (JNumber 22) = pure Struct
  fromJSON (JNumber 23) = pure Event
  fromJSON (JNumber 24) = pure Operator
  fromJSON (JNumber 25) = pure TypeParameter
  fromJSON _ = neutral

namespace CompletionItemTag
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  data CompletionItemTag = Deprecated

export
ToJSON CompletionItemTag where
  toJSON Deprecated = JNumber 1

export
FromJSON CompletionItemTag where
  fromJSON (JNumber 1) = pure Deprecated
  fromJSON _ = neutral

namespace InsertTextMode
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  data InsertTextMode = AsIs | AdjustIndentation

export
ToJSON InsertTextMode where
  toJSON AsIs              = JNumber 1
  toJSON AdjustIndentation = JNumber 2

export
FromJSON InsertTextMode where
  fromJSON (JNumber 1) = pure AsIs
  fromJSON (JNumber 2) = pure AdjustIndentation
  fromJSON _ = neutral

namespace InsertTextFormat
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  data InsertTextFormat = PlainText | Snippet

export
ToJSON InsertTextFormat where
  toJSON PlainText = JNumber 1
  toJSON Snippet   = JNumber 2

export
FromJSON InsertTextFormat where
  fromJSON (JNumber 1) = pure PlainText
  fromJSON (JNumber 2) = pure Snippet
  fromJSON _ = neutral

namespace CompletionItemClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  record CompletionItemTagSupportClientCapabilities where
    constructor MkCompletionItemTagSupportClientCapabilities
    valueSet : List CompletionItemTag
  %runElab deriveJSON defaultOpts `{CompletionItemTagSupportClientCapabilities}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  record CompletionItemResolveSupportClientCapabilities where
    constructor MkCompletionItemResolveSupportClientCapabilities
    properties : List String
  %runElab deriveJSON defaultOpts `{CompletionItemResolveSupportClientCapabilities}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  record InsertTextModeSupport where
    constructor MkInsertTextModeSupport
    valueSet : List InsertTextMode
  %runElab deriveJSON defaultOpts `{InsertTextModeSupport}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  record CompletionItemClientCapabilities where
    constructor MkCompletionItemClientCapabilities
    snippetSupport          : Maybe Bool
    commitCharactersSupport : Maybe Bool
    documentationFormat     : Maybe (List MarkupKind)
    deprecatedSupport       : Maybe Bool
    preselectSupport        : Maybe Bool
    tagSupport              : Maybe CompletionItemTagSupportClientCapabilities
    insertReplaceSupport    : Maybe Bool
    resolveSupport          : Maybe CompletionItemResolveSupportClientCapabilities
    insertTextModeSupport   : Maybe InsertTextModeSupport
  %runElab deriveJSON defaultOpts `{CompletionItemClientCapabilities}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  record CompletionItemKindClientCapabilities where
    constructor MkCompletionItemKindClientCapabilities
    valueSet : Maybe (List CompletionItemKind)
  %runElab deriveJSON defaultOpts `{CompletionItemKindClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionClientCapabilities where
  constructor MkCompletionClientCapabilities
  dynamicRegistration : Maybe Bool
  completionItem      : Maybe CompletionItemClientCapabilities
  completionItemKind  : Maybe CompletionItemKindClientCapabilities
  contextSupport      : Maybe Bool
%runElab deriveJSON defaultOpts `{CompletionClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionOptions where
  constructor MkCompletionOptions
  workDoneProgress    : Maybe Bool
  triggerCharacters   : Maybe (List Char)
  allCommitCharacters : Maybe (List Char)
  resolveProvider     : Maybe Bool
%runElab deriveJSON defaultOpts `{CompletionOptions}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionRegistrationOptions where
  constructor MkCompletionRegistrationOptions
  documentSelector    : (OneOf [DocumentSelector, Null])
  workDoneProgress    : Maybe Bool
  triggerCharacters   : Maybe (List Char)
  allCommitCharacters : Maybe (List Char)
  resolveProvider     : Maybe Bool
%runElab deriveJSON defaultOpts `{CompletionRegistrationOptions}

namespace CompletionTriggerKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  data CompletionTriggerKind = Invoked | TriggerCharacter | TriggerForIncompleteCompletions

export
ToJSON CompletionTriggerKind where
  toJSON Invoked                         = JNumber 1
  toJSON TriggerCharacter                = JNumber 2
  toJSON TriggerForIncompleteCompletions = JNumber 3

export
FromJSON CompletionTriggerKind where
  fromJSON (JNumber 1) = pure Invoked
  fromJSON (JNumber 2) = pure TriggerCharacter
  fromJSON (JNumber 3) = pure TriggerForIncompleteCompletions
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionContext where
  constructor MkCompletionContext
  triggerKind      : CompletionTriggerKind
  triggerCharacter : Maybe String
%runElab deriveJSON defaultOpts `{CompletionContext}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionParams where
  constructor MkCompletionParams
  textDocument       : TextDocumentIdentifier
  position           : Position
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  context            : Maybe CompletionContext
%runElab deriveJSON defaultOpts `{CompletionParams}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionItem where
  constructor MkCompletionItem
  label               : String
  kind                : Maybe CompletionItemKind
  tags                : Maybe (List CompletionItemTag)
  detail              : Maybe String
  documentation       : Maybe (OneOf [String, MarkupContent])
  deprecated          : Maybe Bool
  preselect           : Maybe Bool
  sortText            : Maybe String
  filterText          : Maybe String
  insertText          : Maybe String
  insertTextFormat    : Maybe InsertTextFormat
  insertTextMode      : Maybe InsertTextMode
  textEdit            : Maybe (OneOf [TextEdit, InsertReplaceEdit])
  additionalTextEdits : Maybe (List TextEdit)
  commitCharacters    : Maybe (List Char)
  command             : Maybe Command
  data_               : Maybe JSON
%runElab deriveJSON ({renames := [("data_", "data")]} defaultOpts) `{CompletionItem}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionList where
  constructor MkCompletionList
  isIncomplete : Bool
  items        : List CompletionItem
%runElab deriveJSON defaultOpts `{CompletionList}

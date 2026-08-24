module Language.LSP.Message.Completion

import Language.LSP.Message.Command
import Language.LSP.Message.Location
import Language.LSP.Message.Markup
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.LSP.Message.Workspace
import Language.Reflection

%language ElabReflection
%default total
%hide Text.Bounds.Position

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
  toJSON Text          = JInteger 1
  toJSON Method        = JInteger 2
  toJSON Function      = JInteger 3
  toJSON Constructor   = JInteger 4
  toJSON Field         = JInteger 5
  toJSON Variable      = JInteger 6
  toJSON Class         = JInteger 7
  toJSON Interface     = JInteger 8
  toJSON Module        = JInteger 9
  toJSON Property      = JInteger 10
  toJSON Unit_         = JInteger 11
  toJSON Value         = JInteger 12
  toJSON Enum          = JInteger 13
  toJSON Keyword       = JInteger 14
  toJSON Snippet       = JInteger 15
  toJSON Color         = JInteger 16
  toJSON File          = JInteger 17
  toJSON Reference     = JInteger 18
  toJSON Folder        = JInteger 19
  toJSON EnumMember    = JInteger 20
  toJSON Constant      = JInteger 21
  toJSON Struct        = JInteger 22
  toJSON Event         = JInteger 23
  toJSON Operator      = JInteger 24
  toJSON TypeParameter = JInteger 25

export
FromJSON CompletionItemKind where
  fromJSON (JInteger 1)  = pure Text
  fromJSON (JInteger 2)  = pure Method
  fromJSON (JInteger 3)  = pure Function
  fromJSON (JInteger 4)  = pure Constructor
  fromJSON (JInteger 5)  = pure Field
  fromJSON (JInteger 6)  = pure Variable
  fromJSON (JInteger 7)  = pure Class
  fromJSON (JInteger 8)  = pure Interface
  fromJSON (JInteger 9)  = pure Module
  fromJSON (JInteger 10) = pure Property
  fromJSON (JInteger 11) = pure Unit_
  fromJSON (JInteger 12) = pure Value
  fromJSON (JInteger 13) = pure Enum
  fromJSON (JInteger 14) = pure Keyword
  fromJSON (JInteger 15) = pure Snippet
  fromJSON (JInteger 16) = pure Color
  fromJSON (JInteger 17) = pure File
  fromJSON (JInteger 18) = pure Reference
  fromJSON (JInteger 19) = pure Folder
  fromJSON (JInteger 20) = pure EnumMember
  fromJSON (JInteger 21) = pure Constant
  fromJSON (JInteger 22) = pure Struct
  fromJSON (JInteger 23) = pure Event
  fromJSON (JInteger 24) = pure Operator
  fromJSON (JInteger 25) = pure TypeParameter
  fromJSON _ = fail "not a completion item, [1-25]"

namespace CompletionItemTag
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  data CompletionItemTag = Deprecated

export
ToJSON CompletionItemTag where
  toJSON Deprecated = JInteger 1

export
FromJSON CompletionItemTag where
  fromJSON (JInteger 1) = pure Deprecated
  fromJSON _ = fail "not a completion item tag, 1"

namespace InsertTextMode
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  data InsertTextMode = AsIs | AdjustIndentation

export
ToJSON InsertTextMode where
  toJSON AsIs              = JInteger 1
  toJSON AdjustIndentation = JInteger 2

export
FromJSON InsertTextMode where
  fromJSON (JInteger 1) = pure AsIs
  fromJSON (JInteger 2) = pure AdjustIndentation
  fromJSON _ = fail "not an insert text mode, 1|2"

namespace InsertTextFormat
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  data InsertTextFormat = PlainText | Snippet

export
ToJSON InsertTextFormat where
  toJSON PlainText = JInteger 1
  toJSON Snippet   = JInteger 2

export
FromJSON InsertTextFormat where
  fromJSON (JInteger 1) = pure PlainText
  fromJSON (JInteger 2) = pure Snippet
  fromJSON _ = fail "not an insert text format, 1|2"

namespace CompletionItemClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  record CompletionItemTagSupportClientCapabilities where
    constructor MkCompletionItemTagSupportClientCapabilities
    valueSet : List CompletionItemTag
  %runElab derive "CompletionItemTagSupportClientCapabilities" [FromJSON, ToJSON]

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  record CompletionItemResolveSupportClientCapabilities where
    constructor MkCompletionItemResolveSupportClientCapabilities
    properties : List String
  %runElab derive "CompletionItemResolveSupportClientCapabilities" [FromJSON, ToJSON]

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  record InsertTextModeSupport where
    constructor MkInsertTextModeSupport
    valueSet : List InsertTextMode
  %runElab derive "InsertTextModeSupport" [FromJSON, ToJSON]

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
  %runElab derive "CompletionItemClientCapabilities" [FromJSON, ToJSON]

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  record CompletionItemKindClientCapabilities where
    constructor MkCompletionItemKindClientCapabilities
    valueSet : Maybe (List CompletionItemKind)
  %runElab derive "CompletionItemKindClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionClientCapabilities where
  constructor MkCompletionClientCapabilities
  dynamicRegistration : Maybe Bool
  completionItem      : Maybe CompletionItemClientCapabilities
  completionItemKind  : Maybe CompletionItemKindClientCapabilities
  contextSupport      : Maybe Bool
%runElab derive "CompletionClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionOptions where
  constructor MkCompletionOptions
  workDoneProgress    : Maybe Bool
  triggerCharacters   : Maybe (List Char)
  allCommitCharacters : Maybe (List Char)
  resolveProvider     : Maybe Bool
%runElab derive "CompletionOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionRegistrationOptions where
  constructor MkCompletionRegistrationOptions
  documentSelector    : (OneOf [DocumentSelector, Null])
  workDoneProgress    : Maybe Bool
  triggerCharacters   : Maybe (List Char)
  allCommitCharacters : Maybe (List Char)
  resolveProvider     : Maybe Bool
%runElab derive "CompletionRegistrationOptions" [FromJSON, ToJSON]

namespace CompletionTriggerKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
  public export
  data CompletionTriggerKind = Invoked | TriggerCharacter | TriggerForIncompleteCompletions

export
ToJSON CompletionTriggerKind where
  toJSON Invoked                         = JInteger 1
  toJSON TriggerCharacter                = JInteger 2
  toJSON TriggerForIncompleteCompletions = JInteger 3

export
FromJSON CompletionTriggerKind where
  fromJSON (JInteger 1) = pure Invoked
  fromJSON (JInteger 2) = pure TriggerCharacter
  fromJSON (JInteger 3) = pure TriggerForIncompleteCompletions
  fromJSON _ = fail "not a completion trigger, 1|2|3"

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionContext where
  constructor MkCompletionContext
  triggerKind      : CompletionTriggerKind
  triggerCharacter : Maybe String
%runElab derive "CompletionContext" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionParams where
  constructor MkCompletionParams
  textDocument       : TextDocumentIdentifier
  position           : Position
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  context            : Maybe CompletionContext
%runElab derive "CompletionParams" [FromJSON, ToJSON]

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
%runElab derive "CompletionItem" [FromJSONLSP, ToJSONLSP]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record CompletionList where
  constructor MkCompletionList
  isIncomplete : Bool
  items        : List CompletionItem
%runElab derive "CompletionList" [FromJSON, ToJSON]

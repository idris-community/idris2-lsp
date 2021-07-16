module Language.LSP.Message.CodeAction

import Language.JSON
import Language.LSP.Message.Command
import Language.LSP.Message.Derive
import Language.LSP.Message.Diagnostics
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.LSP.Message.Workspace
import Language.Reflection

%language ElabReflection
%default total

namespace CodeActionKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
  public export
  data CodeActionKind
    = Empty
    | QuickFix
    | Refactor
    | RefactorExtract
    | RefactorInline
    | RefactorRewrite
    | Source
    | SourceOrganizeImport
    | Other String

export
ToJSON CodeActionKind where
  toJSON Empty                = JString ""
  toJSON QuickFix             = JString "quickfix"
  toJSON Refactor             = JString "refactor"
  toJSON RefactorExtract      = JString "refactor.extract"
  toJSON RefactorInline       = JString "refactor.inline"
  toJSON RefactorRewrite      = JString "refactor.rewrite"
  toJSON Source               = JString "source"
  toJSON SourceOrganizeImport = JString "source.organizeImports"
  toJSON (Other act)          = JString act

export
FromJSON CodeActionKind where
  fromJSON (JString "")                       = pure Empty
  fromJSON (JString "quickfix")               = pure QuickFix
  fromJSON (JString "refactor")               = pure Refactor
  fromJSON (JString "refactor.extract")       = pure RefactorExtract
  fromJSON (JString "refactor.inline")        = pure RefactorInline
  fromJSON (JString "refactor.rewrite")       = pure RefactorRewrite
  fromJSON (JString "source")                 = pure Source
  fromJSON (JString "source.organizeImports") = pure SourceOrganizeImport
  fromJSON (JString act)                      = pure (Other act)
  fromJSON _ = neutral

namespace CodeActionClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
  public export
  record CodeActionKindValueSet where
    constructor MkCodeActionKindValueSet
    valueSet : List CodeActionKind
  %runElab deriveJSON defaultOpts `{CodeActionKindValueSet}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
  public export
  record CodeActionLiteralSupport where
    constructor MkCodeActionLiteralSupport
    codeActionKind : CodeActionKindValueSet
  %runElab deriveJSON defaultOpts `{CodeActionLiteralSupport}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
  public export
  record ResolveSupport where
    constructor MkResolveSupport
    properties : List String
  %runElab deriveJSON defaultOpts `{ResolveSupport}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
public export
record CodeActionClientCapabilities where
  constructor MkCodeActionClientCapabilities
  dynamicRegistration      : Maybe Bool
  codeActionLiteralSupport : Maybe CodeActionLiteralSupport
  isPreferredSupport       : Maybe Bool
  disabledSupport          : Maybe Bool
  dataSupport              : Maybe Bool
  resolveSupport           : Maybe ResolveSupport
  honorsChangeAnnotations  : Maybe Bool
%runElab deriveJSON defaultOpts `{CodeActionClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
public export
record CodeActionOptions where
  constructor MkCodeActionOptions
  workDoneProgress : Maybe Bool
  codeActionKinds  : Maybe (List CodeActionKind)
  resolveProvider  : Maybe Bool
%runElab deriveJSON defaultOpts `{CodeActionOptions}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
public export
record CodeActionRegistrationOptions where
  constructor MkCodeActionRegistrationOptions
  workDoneProgress : Maybe Bool
  codeActionKinds  : Maybe (List CodeActionKind)
  resolveProvider  : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab deriveJSON defaultOpts `{CodeActionRegistrationOptions}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
public export
record CodeActionContext where
  constructor MkCodeActionContext
  diagnostics : List Diagnostic
  only        : Maybe (List CodeActionKind)
%runElab deriveJSON defaultOpts `{CodeActionContext}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
public export
record CodeActionParams where
  constructor MkCodeActionParams
  workDoneToken      : Maybe ProgressToken
  partialResultToken : Maybe ProgressToken
  textDocument       : TextDocumentIdentifier
  range              : Range
  context            : CodeActionContext
%runElab deriveJSON defaultOpts `{CodeActionParams}

namespace CodeAction
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
  public export
  record Disabled where
    constructor MkDisabled
    reason : String
  %runElab deriveJSON defaultOpts `{Disabled}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_codeAction
public export
record CodeAction where
  constructor MkCodeAction
  title       : String
  kind        : Maybe CodeActionKind
  diagnostics : Maybe (List Diagnostic)
  isPreferred : Maybe Bool
  disabled    : Maybe Disabled
  edit        : Maybe WorkspaceEdit
  command     : Maybe Command
  data_       : Maybe JSON
%runElab deriveJSON (record {renames = [("data_", "data")]} defaultOpts) `{CodeAction}

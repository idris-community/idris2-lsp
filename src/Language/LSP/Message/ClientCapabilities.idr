module Language.LSP.Message.ClientCapabilities

import Language.JSON
import Language.LSP.Message.CallHierarchy
import Language.LSP.Message.Cancel
import Language.LSP.Message.CodeAction
import Language.LSP.Message.CodeLens
import Language.LSP.Message.Command
import Language.LSP.Message.Completion
import Language.LSP.Message.Declaration
import Language.LSP.Message.Definition
import Language.LSP.Message.Derive
import Language.LSP.Message.Diagnostics
import Language.LSP.Message.DocumentColor
import Language.LSP.Message.DocumentFormatting
import Language.LSP.Message.DocumentHighlight
import Language.LSP.Message.DocumentLink
import Language.LSP.Message.DocumentSymbols
import Language.LSP.Message.FoldingRange
import Language.LSP.Message.Hover
import Language.LSP.Message.Implementation
import Language.LSP.Message.LinkedEditingRange
import Language.LSP.Message.Location
import Language.LSP.Message.Markup
import Language.LSP.Message.Method
import Language.LSP.Message.Moniker
import Language.LSP.Message.Progress
import Language.LSP.Message.References
import Language.LSP.Message.Registration
import Language.LSP.Message.RegularExpressions
import Language.LSP.Message.Rename
import Language.LSP.Message.SelectionRange
import Language.LSP.Message.SemanticTokens
import Language.LSP.Message.SignatureHelp
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Trace
import Language.LSP.Message.Utils
import Language.LSP.Message.Window
import Language.LSP.Message.Workspace
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record TextDocumentClientCapabilities where
  constructor MkTextDocumentClientCapabilities
  synchronization    : Maybe TextDocumentSyncClientCapabilities
  completion         : Maybe CompletionClientCapabilities
  hover              : Maybe HoverClientCapabilities
  signatureHelp      : Maybe SignatureHelpClientCapabilities
  declaration        : Maybe DeclarationClientCapabilities
  definition         : Maybe DefinitionClientCapabilities
  typeDefinition     : Maybe TypeDefinitionClientCapabilities
  implementation_    : Maybe ImplementationClientCapabilities
  references         : Maybe ReferenceClientCapabilities
  documentHighlight  : Maybe DocumentHighlightClientCapabilities
  documentSymbol     : Maybe DocumentSymbolClientCapabilities
  codeAction         : Maybe CodeActionClientCapabilities
  codeLens           : Maybe CodeLensClientCapabilities
  documentLink       : Maybe DocumentLinkClientCapabilities
  colorProvider      : Maybe DocumentColorClientCapabilities
  formatting         : Maybe DocumentFormattingClientCapabilities
  rangeFormatting    : Maybe DocumentRangeFormattingClientCapabilities
  onTypeFormatting   : Maybe DocumentOnTypeFormattingClientCapabilities
  rename             : Maybe RenameClientCapabilities
  publishDiagnostics : Maybe PublishDiagnosticsClientCapabilities
  foldingRange       : Maybe FoldingRangeClientCapabilities
  selectionRange     : Maybe SelectionRangeClientCapabilities
  linkedEditingRange : Maybe LinkedEditingRangeClientCapabilities
  callHierarchy      : Maybe CallHierarchyClientCapabilities
  semanticTokens     : Maybe SemanticTokensClientCapabilities
  moniker            : Maybe MonikerClientCapabilities
%runElab deriveJSON ({renames := [("implementation_", "implementation")]} defaultOpts) `{TextDocumentClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record FileOperationsWorkspaceClientCapabilities where
  constructor MkFileOperationsWorkspaceClientCapabilities
  dynamicRegistration : Maybe Bool
  didCreate           : Maybe Bool
  willCreate          : Maybe Bool
  didRename           : Maybe Bool
  willRename          : Maybe Bool
  didDelete           : Maybe Bool
  willDelete          : Maybe Bool
%runElab deriveJSON defaultOpts `{FileOperationsWorkspaceClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record WorkspaceClientCapabilities where
  constructor MkWorkspaceClientCapabilities
  applyEdit              : Maybe Bool
  workspaceEdit          : Maybe WorkspaceEditClientCapabilities
  didChangeConfiguration : Maybe DidChangeConfigurationClientCapabilities
  didChangeWatchedFiles  : Maybe DidChangeWatchedFilesClientCapabilities
  symbol                 : Maybe WorkspaceSymbolClientCapabilities
  executeCommand         : Maybe ExecuteCommandClientCapabilities
  workspaceFolders       : Maybe Bool
  configuration          : Maybe Bool
  semanticTokens         : Maybe SemanticTokensWorkspaceClientCapabilities
  codeLens               : Maybe CodeLensWorkspaceClientCapabilities
  fileOperations         : Maybe FileOperationsWorkspaceClientCapabilities
%runElab deriveJSON defaultOpts `{WorkspaceClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record WindowClientCapabilities where
  constructor MkWindowClientCapabilities
  workDoneProgress : Maybe Bool
  showMessage      : Maybe ShowMessageRequestClientCapabilities
  showDocument     : Maybe ShowDocumentClientCapabilities
%runElab deriveJSON defaultOpts `{WindowClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record GeneralClientCapabilities where
  constructor MkGeneralClientCapabilities
  regularExpressions : Maybe RegularExpressionsClientCapabilities
  markdown           : Maybe MarkdownClientCapabilities
%runElab deriveJSON defaultOpts `{GeneralClientCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record ClientCapabilities where
  constructor MkClientCapabilities
  workspace    : Maybe WorkspaceClientCapabilities
  textDocument : Maybe TextDocumentClientCapabilities
  window       : Maybe WindowClientCapabilities
  general      : Maybe GeneralClientCapabilities
  experimental : Maybe JSON
%runElab deriveJSON defaultOpts `{ClientCapabilities}

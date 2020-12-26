module Language.LSP.Message.ServerCapabilities

import Data.SortedMap
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
record FileOperationsServerCapabilities where
  constructor MkFileOperationsServerCapabilities
  didCreate  : Maybe FileOperationRegistrationOptions
  willCreate : Maybe FileOperationRegistrationOptions
  didRename  : Maybe FileOperationRegistrationOptions
  willRename : Maybe FileOperationRegistrationOptions
  didDelete  : Maybe FileOperationRegistrationOptions
  willDelete : Maybe FileOperationRegistrationOptions
%runElab deriveJSON defaultOpts `{{FileOperationsServerCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record WorkspaceServerCapabilities where
  constructor MkWorkspaceServerCapabilities
  workspaceFolders : Maybe WorkspaceFoldersServerCapabilities
  fileOperations   : Maybe FileOperationsServerCapabilities
%runElab deriveJSON defaultOpts `{{WorkspaceServerCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record ServerCapabilities where
  textDocumentSync                 : Maybe (TextDocumentSyncOptions .+. TextDocumentSyncKind)
  completionProvider               : Maybe CompletionOptions
  hoverProvider                    : Maybe (Bool .+. HoverOptions)
  signatureHelpProvider            : Maybe SignatureHelpOptions
  declarationProvider              : Maybe (Bool .+. DefinitionOptions)
  typeDefinitionProvider           : Maybe (Bool .+. TypeDefinitionOptions .+. TypeDefinitionRegistrationOptions)
  implementationProvider           : Maybe (Bool .+. ImplementationOptions .+. ImplementationRegistrationOptions)
  referencesProvider               : Maybe (Bool .+. ReferenceOptions)
  documentHighlightProvider        : Maybe (Bool .+. DocumentHighlightOptions)
  documentSymbolProvider           : Maybe (Bool .+. DocumentSymbolOptions)
  codeActionProvider               : Maybe (Bool .+. CodeActionOptions)
  codeLensProvider                 : Maybe CodeLensOptions
  documentLinkProvider             : Maybe DocumentLinkOptions
  colorProvider                    : Maybe (Bool .+. DocumentColorOptions .+. DocumentColorRegistrationOptions)
  documentFormattingProvider       : Maybe (Bool .+. DocumentFormattingOptions)
  documentRangeFormattingProvider  : Maybe (Bool .+. DocumentRangeFormattingOptions)
  documentOnTypeFormattingProvider : Maybe DocumentOnTypeFormattingOptions
  renameProvider                   : Maybe (Bool .+. RenameOptions)
  foldingRangeProvider             : Maybe (Bool .+. FoldingRangeOptions .+. FoldingRangeRegistrationOptions)
  executeCommandProvider           : Maybe ExecuteCommandOptions
  selectionRangeProvider           : Maybe (Bool .+. SelectionRangeOptions .+. SelectionRangeRegistrationOptions)
  linkedEditingRangeProvider       : Maybe (Bool .+. LinkedEditingRangeOptions .+. LinkedEditingRangeRegistrationOptions)
  callHierarchyProvider            : Maybe (Bool .+. CallHierarchyOptions .+. CallHierarchyRegistrationOptions)
  semanticTokensProvider           : Maybe (SemanticTokensOptions .+. SemanticTokensRegistrationOptions)
  monikerProvider                  : Maybe (Bool .+. MonikerOptions .+. MonikerRegistrationOptions)
  workspaceSymbolProvider          : Maybe (Bool .+. WorkspaceSymbolOptions)
  workspace                        : Maybe WorkspaceServerCapabilities
  experimental                     : Maybe JSON
%runElab deriveJSON defaultOpts `{{ServerCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record ServerInfo where
  constructor MkServerInfo
  name    : String
  version : Maybe String
%runElab deriveJSON defaultOpts `{{ServerInfo}}

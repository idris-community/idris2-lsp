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
%runElab deriveJSON defaultOpts `{FileOperationsServerCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record WorkspaceServerCapabilities where
  constructor MkWorkspaceServerCapabilities
  workspaceFolders : Maybe WorkspaceFoldersServerCapabilities
  fileOperations   : Maybe FileOperationsServerCapabilities
%runElab deriveJSON defaultOpts `{WorkspaceServerCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record ServerCapabilities where
  constructor MkServerCapabilities
  textDocumentSync                 : Maybe (OneOf [TextDocumentSyncOptions, TextDocumentSyncKind])
  completionProvider               : Maybe CompletionOptions
  hoverProvider                    : Maybe (OneOf [Bool, HoverOptions])
  signatureHelpProvider            : Maybe SignatureHelpOptions
  declarationProvider              : Maybe (OneOf [Bool, DeclarationOptions, DeclarationRegistrationOptions])
  definitionProvider               : Maybe (OneOf [Bool, DefinitionOptions, DefinitionRegistrationOptions])
  typeDefinitionProvider           : Maybe (OneOf [Bool, TypeDefinitionOptions, TypeDefinitionRegistrationOptions])
  implementationProvider           : Maybe (OneOf [Bool, ImplementationOptions, ImplementationRegistrationOptions])
  referencesProvider               : Maybe (OneOf [Bool, ReferenceOptions])
  documentHighlightProvider        : Maybe (OneOf [Bool, DocumentHighlightOptions])
  documentSymbolProvider           : Maybe (OneOf [Bool, DocumentSymbolOptions])
  codeActionProvider               : Maybe (OneOf [Bool, CodeActionOptions])
  codeLensProvider                 : Maybe CodeLensOptions
  documentLinkProvider             : Maybe DocumentLinkOptions
  colorProvider                    : Maybe (OneOf [Bool, DocumentColorOptions, DocumentColorRegistrationOptions])
  documentFormattingProvider       : Maybe (OneOf [Bool, DocumentFormattingOptions])
  documentRangeFormattingProvider  : Maybe (OneOf [Bool, DocumentRangeFormattingOptions])
  documentOnTypeFormattingProvider : Maybe DocumentOnTypeFormattingOptions
  renameProvider                   : Maybe (OneOf [Bool, RenameOptions])
  foldingRangeProvider             : Maybe (OneOf [Bool, FoldingRangeOptions, FoldingRangeRegistrationOptions])
  executeCommandProvider           : Maybe ExecuteCommandOptions
  selectionRangeProvider           : Maybe (OneOf [Bool, SelectionRangeOptions, SelectionRangeRegistrationOptions])
  linkedEditingRangeProvider       : Maybe (OneOf [Bool, LinkedEditingRangeOptions, LinkedEditingRangeRegistrationOptions])
  callHierarchyProvider            : Maybe (OneOf [Bool, CallHierarchyOptions, CallHierarchyRegistrationOptions])
  semanticTokensProvider           : Maybe (OneOf [SemanticTokensOptions, SemanticTokensRegistrationOptions])
  monikerProvider                  : Maybe (OneOf [Bool, MonikerOptions, MonikerRegistrationOptions])
  workspaceSymbolProvider          : Maybe (OneOf [Bool, WorkspaceSymbolOptions])
  workspace                        : Maybe WorkspaceServerCapabilities
  experimental                     : Maybe JSON
%runElab deriveJSON defaultOpts `{ServerCapabilities}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#initialize
public export
record ServerInfo where
  constructor MkServerInfo
  name    : String
  version : Maybe String
%runElab deriveJSON defaultOpts `{ServerInfo}

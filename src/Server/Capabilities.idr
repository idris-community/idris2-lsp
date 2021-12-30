||| Default configurations for the LSP server implementation capabilities.
|||
||| (C) The Idris Community, 2021
module Server.Capabilities

import Core.Metadata

import Language.JSON
import Language.LSP.Message

%default total

decor : List Decoration
decor = [Typ, Function, Data, Bound, Keyword, Namespace, Postulate, Module, Comment]

encodeDecorAsString : Decoration -> String
encodeDecorAsString Typ       = "type"        -- Type constructors
encodeDecorAsString Function  = "function"    -- Functions
encodeDecorAsString Data      = "enumMember"  -- Data constructors
encodeDecorAsString Bound     = "variable"    -- Bound variables
encodeDecorAsString Keyword   = "keyword"     -- Keywords
encodeDecorAsString Namespace = "namespace"   -- Explicit namespaces
encodeDecorAsString Postulate = "postulate"   -- Postulates (assert_total, believe_me, ...)
encodeDecorAsString Module    = "module"      -- Explicit module names
encodeDecorAsString Comment   = "comment"     -- Comments

||| Convert Decoration to legend index
export
encodeDecorAsNum : Decoration -> Int
encodeDecorAsNum Typ       = 0
encodeDecorAsNum Function  = 1
encodeDecorAsNum Data      = 2
encodeDecorAsNum Bound     = 3
encodeDecorAsNum Keyword   = 4
encodeDecorAsNum Namespace = 5
encodeDecorAsNum Postulate = 6
encodeDecorAsNum Module    = 7
encodeDecorAsNum Comment   = 8

syncOptions = MkTextDocumentSyncOptions
  { openClose         = Just True
  , change            = Just Incremental
  , willSave          = Nothing
  , willSaveWaitUntil = Nothing
  , save              = Just $ make $ MkSaveOptions (Just True)
  }

semanticTokensLegend = MkSemanticTokensLegend
  { tokenTypes = map encodeDecorAsString decor
  , tokenModifiers = []
  }

semanticTokensOptions = MkSemanticTokensOptions
  { legend = semanticTokensLegend
  , range = Just $ make False
  , full = Just $ make True
  }

completionOptions = MkCompletionOptions
  { workDoneProgress    = Nothing
  , triggerCharacters   = Just []
  , allCommitCharacters = Just []
  , resolveProvider     = Nothing
  }

signatureHelpOptions = MkSignatureHelpOptions
  { workDoneProgress    = Nothing
  , triggerCharacters   = Nothing
  , retriggerCharacters = Nothing
  }

definitionOptions = MkDefinitionOptions
  { workDoneProgress = Just False
  }

documentSymbolOptions = MkDocumentSymbolOptions
  { workDoneProgress = Nothing
  , label            = Nothing
  }

codeActionOptions = MkCodeActionOptions
  { workDoneProgress = Nothing
  , codeActionKinds  = Nothing
  , resolveProvider  = Just False
  }

codeLensOptions = MkCodeLensOptions
  { workDoneProgress = Nothing
  , resolveProvider  = Nothing
  }

documentLinkOptions = MkDocumentLinkOptions
  { workDoneProgress = Nothing
  , resolveProvider  = Nothing
  }

executeCommandOptions = MkExecuteCommandOptions
  { workDoneProgress = Nothing
  , commands         = ["repl", "exprSearchWithHints", "refineHoleWithHints", "metavars"]
  }

workspaceFoldersServerCapabilities = MkWorkspaceFoldersServerCapabilities
  { supported           = Just False
  , changeNotifications = Nothing
  }

fileOperationServerCapabilities = MkFileOperationsServerCapabilities
  { didCreate  = Nothing
  , willCreate = Nothing
  , didRename  = Nothing
  , willRename = Nothing
  , didDelete  = Nothing
  , willDelete = Nothing
  }

workspaceServerCapabilities = MkWorkspaceServerCapabilities
  { workspaceFolders = Just workspaceFoldersServerCapabilities
  , fileOperations   = Just fileOperationServerCapabilities
  }

||| Default server capabilities to be sent to clients during the initialization protocol.
export
serverCapabilities : ServerCapabilities
serverCapabilities = MkServerCapabilities
  { textDocumentSync                 = Just $ make syncOptions
  , completionProvider               = Just completionOptions
  , hoverProvider                    = Just $ make True
  , signatureHelpProvider            = Just signatureHelpOptions
  , definitionProvider               = Just $ make definitionOptions
  , declarationProvider              = Just $ make False
  , typeDefinitionProvider           = Just $ make False
  , implementationProvider           = Just $ make False
  , referencesProvider               = Just $ make False
  , documentHighlightProvider        = Just $ make True
  , documentSymbolProvider           = Just $ make documentSymbolOptions
  , codeActionProvider               = Just $ make codeActionOptions
  , codeLensProvider                 = Just codeLensOptions
  , documentLinkProvider             = Just documentLinkOptions
  , colorProvider                    = Just $ make False
  , documentFormattingProvider       = Just $ make False
  , documentRangeFormattingProvider  = Just $ make False
  , documentOnTypeFormattingProvider = Nothing
  , renameProvider                   = Just $ make False
  , foldingRangeProvider             = Just $ make False
  , executeCommandProvider           = Just executeCommandOptions
  , selectionRangeProvider           = Just $ make False
  , linkedEditingRangeProvider       = Just $ make False
  , callHierarchyProvider            = Just $ make False
  , semanticTokensProvider           = Just $ make semanticTokensOptions
  , monikerProvider                  = Just $ make False
  , workspaceSymbolProvider          = Just $ make False
  , workspace                        = Just workspaceServerCapabilities
  , experimental                     = Nothing
  }

||| Server information to be sent to clients during the initialization protocol.
export
serverInfo : ServerInfo
serverInfo = MkServerInfo { name = "idris2-lsp", version = Just "0.1" }

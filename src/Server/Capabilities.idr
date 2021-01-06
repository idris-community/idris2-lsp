||| Default configurations for the LSP server implementation capabilities.
|||
||| (C) The Idris Community, 2021
module Server.Capabilities

import Language.JSON
import Language.LSP.Message

%default total

syncOptions : TextDocumentSyncOptions
syncOptions = MkTextDocumentSyncOptions { openClose = Just True
                                        , change = Just Full
                                        , willSave = Nothing
                                        , willSaveWaitUntil = Nothing
                                        , save = Nothing
                                        }

||| Default server capabilities to be sent to clients during the initialization protocol.
export
serverCapabilities : ServerCapabilities
serverCapabilities =
  MkServerCapabilities { textDocumentSync                 = Just (Left syncOptions)
                       , completionProvider               = Nothing -- CompletionOptions
                       , hoverProvider                    = Nothing -- HoverOptions
                       , signatureHelpProvider            = Nothing -- SignatureHelpOptions
                       , declarationProvider              = Nothing -- (Bool .+. DefinitionOptions)
                       , typeDefinitionProvider           = Nothing -- (Bool .+. TypeDefinitionOptions .+. TypeDefinitionRegistrationOptions)
                       , implementationProvider           = Nothing -- (Bool .+. ImplementationOptions .+. ImplementationRegistrationOptions)
                       , referencesProvider               = Nothing -- (Bool .+. ReferenceOptions)
                       , documentHighlightProvider        = Nothing -- (Bool .+. DocumentHighlightOptions)
                       , documentSymbolProvider           = Nothing -- (Bool .+. DocumentSymbolOptions)
                       , codeActionProvider               = Nothing -- (Bool .+. CodeActionOptions)
                       , codeLensProvider                 = Nothing -- CodeLensOptions
                       , documentLinkProvider             = Nothing -- DocumentLinkOptions
                       , colorProvider                    = Nothing -- (Bool .+. DocumentColorOptions .+. DocumentColorRegistrationOptions)
                       , documentFormattingProvider       = Nothing -- (Bool .+. DocumentFormattingOptions)
                       , documentRangeFormattingProvider  = Nothing -- (Bool .+. DocumentRangeFormattingOptions)
                       , documentOnTypeFormattingProvider = Nothing -- DocumentOnTypeFormattingOptions
                       , renameProvider                   = Nothing -- (Bool .+. RenameOptions)
                       , foldingRangeProvider             = Nothing -- (Bool .+. FoldingRangeOptions .+. FoldingRangeRegistrationOptions)
                       , executeCommandProvider           = Nothing -- ExecuteCommandOptions
                       , selectionRangeProvider           = Nothing -- (Bool .+. SelectionRangeOptions .+. SelectionRangeRegistrationOptions)
                       , linkedEditingRangeProvider       = Nothing -- (Bool .+. LinkedEditingRangeOptions .+. LinkedEditingRangeRegistrationOptions)
                       , callHierarchyProvider            = Nothing -- (Bool .+. CallHierarchyOptions .+. CallHierarchyRegistrationOptions)
                       , semanticTokensProvider           = Nothing -- (SemanticTokensOptions .+. SemanticTokensRegistrationOptions)
                       , monikerProvider                  = Nothing -- (Bool .+. MonikerOptions .+. MonikerRegistrationOptions)
                       , workspaceSymbolProvider          = Nothing -- (Bool .+. WorkspaceSymbolOptions)
                       , workspace                        = Nothing -- WorkspaceServerCapabilities
                       , experimental                     = Nothing -- JSON
                       }

||| Server information to be sent to clients during the initialization protocol.
export
serverInfo : ServerInfo
serverInfo = MkServerInfo { name = "idris2-lsp", version = Just "0.1" }

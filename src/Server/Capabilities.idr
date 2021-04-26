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
                                        , save = Just (Right (MkSaveOptions (Just True)))
                                        }

||| Default server capabilities to be sent to clients during the initialization protocol.
export
serverCapabilities : ServerCapabilities
serverCapabilities =
  MkServerCapabilities { textDocumentSync                 = Just (Left syncOptions)
                       , completionProvider               = Just (MkCompletionOptions Nothing (Just []) (Just []) Nothing)
                       , hoverProvider                    = Just (Left True)
                       , signatureHelpProvider            = Just (MkSignatureHelpOptions Nothing Nothing Nothing)
                       , declarationProvider              = Just (Left False)
                       , typeDefinitionProvider           = Just (Left False)
                       , implementationProvider           = Just (Left False)
                       , referencesProvider               = Just (Left False)
                       , documentHighlightProvider        = Just (Left False)
                       , documentSymbolProvider           = Just (Left False)
                       , codeActionProvider               = Just (Left False)
                       , codeLensProvider                 = Just (MkCodeLensOptions Nothing Nothing)
                       , documentLinkProvider             = Just (MkDocumentLinkOptions Nothing Nothing)
                       , colorProvider                    = Just (Left False)
                       , documentFormattingProvider       = Just (Left False)
                       , documentRangeFormattingProvider  = Just (Left False)
                       , documentOnTypeFormattingProvider = Nothing
                       , renameProvider                   = Just (Left False)
                       , foldingRangeProvider             = Just (Left False)
                       , executeCommandProvider           = Just (MkExecuteCommandOptions Nothing [])
                       , selectionRangeProvider           = Just (Left False)
                       , linkedEditingRangeProvider       = Just (Left False)
                       , callHierarchyProvider            = Just (Left False)
                       , semanticTokensProvider           = Nothing
                       , monikerProvider                  = Just (Left False)
                       , workspaceSymbolProvider          = Just (Left False)
                       , workspace                        = Just (MkWorkspaceServerCapabilities
                                                                 (Just (MkWorkspaceFoldersServerCapabilities (Just False) Nothing))
                                                                 (Just (MkFileOperationsServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing)))
                       , experimental                     = Nothing
                       }

||| Server information to be sent to clients during the initialization protocol.
export
serverInfo : ServerInfo
serverInfo = MkServerInfo { name = "idris2-lsp", version = Just "0.1" }

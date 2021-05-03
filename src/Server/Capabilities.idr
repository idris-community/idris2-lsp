||| Default configurations for the LSP server implementation capabilities.
|||
||| (C) The Idris Community, 2021
module Server.Capabilities

import Language.JSON
import Language.LSP.Message
import Language.LSP.Completion

%default total

syncOptions : TextDocumentSyncOptions
syncOptions = MkTextDocumentSyncOptions { openClose = Just True
                                        , change = Just Full
                                        , willSave = Nothing
                                        , willSaveWaitUntil = Nothing
                                        , save = Just (make (MkSaveOptions (Just True)))
                                        }

||| Default server capabilities to be sent to clients during the initialization protocol.
export
serverCapabilities : ServerCapabilities
serverCapabilities =
  MkServerCapabilities { textDocumentSync                 = Just (make syncOptions)
                       , completionProvider               = Just completionProvider
                       , hoverProvider                    = Just (make True)
                       , signatureHelpProvider            = Just (MkSignatureHelpOptions Nothing Nothing Nothing)
                       , definitionProvider               = Just (make (MkDefinitionOptions
                                                                          (Just False)))
                       , declarationProvider              = Just (make False)
                       , typeDefinitionProvider           = Just (make False)
                       , implementationProvider           = Just (make False)
                       , referencesProvider               = Just (make False)
                       , documentHighlightProvider        = Just (make False)
                       , documentSymbolProvider           = Just (make (MkDocumentSymbolOptions
                                                                          Nothing
                                                                          Nothing))
                       , codeActionProvider               = Just (make (MkCodeActionOptions
                                                                          Nothing
                                                                          (Just
                                                                            [ Other "case-split"
                                                                            , Other "generate-def"
                                                                            , Other "make-case"
                                                                            ])
                                                                          (Just False)))
                       , codeLensProvider                 = Just (MkCodeLensOptions Nothing Nothing)
                       , documentLinkProvider             = Just (MkDocumentLinkOptions Nothing Nothing)
                       , colorProvider                    = Just (make False)
                       , documentFormattingProvider       = Just (make False)
                       , documentRangeFormattingProvider  = Just (make False)
                       , documentOnTypeFormattingProvider = Nothing
                       , renameProvider                   = Just (make False)
                       , foldingRangeProvider             = Just (make False)
                       , executeCommandProvider           = Just (MkExecuteCommandOptions Nothing [])
                       , selectionRangeProvider           = Just (make False)
                       , linkedEditingRangeProvider       = Just (make False)
                       , callHierarchyProvider            = Just (make False)
                       , semanticTokensProvider           = Nothing
                       , monikerProvider                  = Just (make False)
                       , workspaceSymbolProvider          = Just (make False)
                       , workspace                        = Just (MkWorkspaceServerCapabilities
                                                                 (Just (MkWorkspaceFoldersServerCapabilities (Just False) Nothing))
                                                                 (Just (MkFileOperationsServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing)))
                       , experimental                     = Nothing
                       }

||| Server information to be sent to clients during the initialization protocol.
export
serverInfo : ServerInfo
serverInfo = MkServerInfo { name = "idris2-lsp", version = Just "0.1" }

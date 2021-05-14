||| Default configurations for the LSP server implementation capabilities.
|||
||| (C) The Idris Community, 2021
module Server.Capabilities

import Language.JSON
import Language.LSP.Message

%default total

syncOptions : TextDocumentSyncOptions
syncOptions = MkTextDocumentSyncOptions { openClose = Just True
                                        , change = Just Incremental
                                        , willSave = Nothing
                                        , willSaveWaitUntil = Nothing
                                        , save = Just (make (MkSaveOptions (Just True)))
                                        }

semanticTokensLegend : SemanticTokensLegend
semanticTokensLegend = MkSemanticTokensLegend
  -- ``type``: type constructors       ==> ``type``
  -- ``function``: defined functions   ==> ``function``
  -- ``data``: data constructors       ==> ``struct``
  -- ``bound``: bound variables, or    ==> ``variable``
  -- ``keyword``                       ==> ``keyword``
  ["type", "function", "struct", "variable", "keyword"]
  []

semanticTokensOptions : SemanticTokensOptions
semanticTokensOptions = MkSemanticTokensOptions
  semanticTokensLegend
  (Just (make False))
  (Just (make True))


||| Default server capabilities to be sent to clients during the initialization protocol.
export
serverCapabilities : ServerCapabilities
serverCapabilities =
  MkServerCapabilities { textDocumentSync                 = Just (make syncOptions)
                       , completionProvider               = Just (MkCompletionOptions Nothing (Just []) (Just []) Nothing)
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
                                                                          Nothing
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
                       , semanticTokensProvider           = Just (make semanticTokensOptions)
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

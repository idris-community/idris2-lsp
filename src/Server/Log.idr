||| Logging utilities for the LSP server implementation.
|||
||| (C) The Idris Community, 2021
module Server.Log

import Core.Context.Context
import Core.Core
import Core.Directory
import Server.Configuration
import Server.Severity
import Server.Utils
import System.Directory
import System.File
import System.Path

%default total

public export
data Topic
  = AddClause
  | Browse
  | CaseSplit
  | Channel
  | CodeAction
  | Completion
  | Configuration
  | Diagnostic
  | DocumentHighlight
  | DocumentSymbol
  | ExprSearch
  | GenerateDef
  | GotoDefinition
  | Hover
  | Intro
  | MakeCase
  | MakeLemma
  | MakeWith
  | Metavars
  | QuickFix
  | RefineHole
  | SemanticTokens
  | Server
  | SignatureHelp

export
Show Topic where
  show AddClause = "Request.CodeAction.AddClause"
  show Browse = "Request.Command.Browse"
  show CaseSplit = "Request.CodeAction.CaseSplit"
  show Channel = "Communication.Channel"
  show CodeAction = "CodeAction"
  show Completion = "Request.Completion"
  show Configuration = "Request.Configuration"
  show Diagnostic = "Notification.Diagnostic"
  show DocumentHighlight = "Request.DocumentHighlight"
  show DocumentSymbol = "Request.DocumentSymbol"
  show ExprSearch = "Request.CodeAction.ExprSearch"
  show GenerateDef = "Request.CodeAction.GenerateDef"
  show GotoDefinition = "Request.GotoDefinition"
  show Hover = "Request.Hover"
  show Intro = "Request.CodeAction.Intro"
  show MakeCase = "Request.CodeAction.MakeCase"
  show MakeLemma = "Request.CodeAction.MakeLemma"
  show MakeWith = "Request.CodeAction.MakeWith"
  show Metavars = "Request.Command.Metavars"
  show QuickFix = "Request.CodeAction.QuickFix"
  show RefineHole = "Request.CodeAction.RefineHole"
  show SemanticTokens = "Notification.SemanticTokens"
  show Server = "Server"
  show SignatureHelp = "Request.SignatureHelp"

||| Logs a string with the provided severity level.
export
log : Ref LSPConf LSPConfiguration => Severity -> Topic -> Lazy String -> Core ()
log severity topic msg = do
  logHandle <- gets LSPConf logHandle
  logSeverity <- gets LSPConf logSeverity
  when (severity >= logSeverity) $ do
    coreLift_ $ fPutStrLn logHandle "LOG \{show severity}:\{show topic}: \{msg}"
    coreLift $ fflush logHandle

export
logD : Ref LSPConf LSPConfiguration => Topic -> Lazy String -> Core ()
logD = log Debug

export
logI : Ref LSPConf LSPConfiguration => Topic -> Lazy String -> Core ()
logI = log Info

export
logW : Ref LSPConf LSPConfiguration => Topic -> Lazy String -> Core ()
logW = log Warning

export
logE : Ref LSPConf LSPConfiguration => Topic -> Lazy String -> Core ()
logE = log Error

export
logC : Ref LSPConf LSPConfiguration => Topic -> Lazy String -> Core ()
logC = log Critical

||| Changes the log file location, if possible.
export covering
changeLogFile : Ref LSPConf LSPConfiguration => Lazy String -> Core ()
changeLogFile fname = do
  let True = isAbsolute fname
    | False => logE Configuration "Unable to change log file location: \{fname} is not an absolute path"
  whenJust (parent fname) $ \dir => do
    Right _ <- coreLift $ mkdirAll dir
      | Left err => logE Configuration "Unable to create directory \{dir}: \{show err}"
    logD Configuration "Created new directory \{dir} for log file \{fname}"
  Right handle <- coreLift $ openFile fname Append
    | Left err => logE Configuration "Unable to updated log file location \{fname}: \{show err}"
  update LSPConf ({ logHandle := handle })
  logI Configuration "Log file location updated to \{fname}"

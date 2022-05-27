||| Logging utilities for the LSP server implementation.
|||
||| (C) The Idris Community, 2021
module Server.Log

import Core.Core
import Core.Context.Context
import Core.Directory
import Server.Configuration
import Server.Utils
import System.Directory
import System.File
import System.Path

%default total

||| Type for the severity of logging messages.
||| Levels are roughly categorised as follow:
||| - Debug    Messages targeted only for developing purposes
||| - Info     Messages for progress without unexpected behaviour or errors
||| - Warning  Messages for unsupported requests or wrong configurations 
||| - Error    Messages for either server or compiler error which are unexpected but recoverable
||| - Critical Messages for error that require immediate stopping of the server
public export
data Severity = Debug | Info | Warning | Error | Critical

Cast Severity Integer where
  cast Debug    = 0
  cast Info     = 1
  cast Warning  = 2
  cast Error    = 3
  cast Critical = 4

export
Eq Severity where
  Debug    == Debug    = True
  Info     == Info     = True
  Warning  == Warning  = True
  Error    == Error    = True
  Critical == Critical = True
  _ == _ = False

export
Show Severity where
  show Debug    = "DEBUG"
  show Info     = "INFO"
  show Warning  = "WARNING"
  show Error    = "ERROR"
  show Critical = "CRITICAL"

export
Ord Severity where
  compare x y = compare (cast {to = Integer} x) (cast y)

public export
data Topic
  = AddClause
  | Browse
  | CaseSplit
  | Channel
  | CodeAction
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
log : Ref LSPConf LSPConfiguration => Severity -> Topic -> String -> Core ()
log severity topic msg = do
  logHandle <- gets LSPConf logHandle
  coreLift_ $ fPutStrLn logHandle "LOG \{show severity}:\{show topic}: \{msg}"
  coreLift $ fflush logHandle

export
logD : Ref LSPConf LSPConfiguration => Topic -> String -> Core ()
logD = log Debug

export
logI : Ref LSPConf LSPConfiguration => Topic -> String -> Core ()
logI = log Info

export
logW : Ref LSPConf LSPConfiguration => Topic -> String -> Core ()
logW = log Warning

export
logE : Ref LSPConf LSPConfiguration => Topic -> String -> Core ()
logE = log Error

export
logC : Ref LSPConf LSPConfiguration => Topic -> String -> Core ()
logC = log Critical

||| Changes the log file location, if possible.
export covering
changeLogFile : Ref LSPConf LSPConfiguration => String -> Core ()
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

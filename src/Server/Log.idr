||| Logging utilities for the LSP server implementation.
|||
||| (C) The Idris Community, 2021
module Server.Log

import Core.Core
import Core.Directory
import Server.Configuration
import Server.Utils
import System.Directory
import System.File
import System.Path

%default total

-- TODO: Very crude logging, we should probably adopt a more flexible logging mechanism.
--       Maybe comonads, à la Haskell co-log package?

||| Type for the severity of logging messages.
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

||| Logs a string with the provided severity level.
export
logString : Ref LSPConf LSPConfiguration => Severity -> String -> Core ()
logString severity msg = do
  logHandle <- gets LSPConf logHandle
  ignore $ coreLift $ fPutStrLn logHandle ("LOG " ++ show severity ++ ": " ++ msg)
  coreLift $ fflush logHandle

||| Logs a showable value with the provided severity level.
export
logShow : Ref LSPConf LSPConfiguration => Show a => Severity -> a -> Core ()
logShow severity = logString severity . show

||| Changes the log file location, if possible.
export covering
changeLogFile : Ref LSPConf LSPConfiguration => String -> Core ()
changeLogFile fname = do
  let True = isAbsolute fname
    | False => logString Error "Unable to change log file location: \{fname} is not an absolute path"
  case parent fname of
       Just dir => do Right _ <- coreLift $ mkdirAll dir
                        | Left err => logString Error "Unable to create directory \{dir}: \{show err}"
                      logString Debug "Created new log directory \{dir}"
       Nothing => pure ()
  Right handle <- coreLift $ openFile fname Append
    | Left err => logString Error "Unable to updated log file location \{fname}: \{show err}"
  modify LSPConf (record { logHandle = handle })
  logString Debug "Log file location updated to \{fname}"

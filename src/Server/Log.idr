||| Logging utilities for the LSP server implementation.
|||
||| (C) The Idris Community, 2021
module Server.Log

import Core.Core
import System.File

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
logString : Severity -> String -> Core ()
logString severity msg = coreLift $
  fPutStrLn stderr ("LOG " ++ show severity ++ ": " ++ msg) *> pure ()

||| Logs a showable value with the provided severity level.
export
logShow : Show a => Severity -> a -> Core ()
logShow severity = logString severity . show

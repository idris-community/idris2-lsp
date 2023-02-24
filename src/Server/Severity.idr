||| Logging utilities for the LSP server implementation.
|||
||| (C) The Idris Community, 2023
module Server.Severity

import Data.String

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

export
parseSeverity : String -> Maybe Severity
parseSeverity str = case toUpper str of
  "DEBUG"    => Just Debug
  "INFO"     => Just Info
  "WARNING"  => Just Warning
  "ERROR"    => Just Error
  "CRITICAL" => Just Critical
  _          => Nothing

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

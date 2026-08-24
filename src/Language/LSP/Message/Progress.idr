module Language.LSP.Message.Progress

import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#progress
public export
ProgressToken : Type
ProgressToken = OneOf [Int, String]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#clientInitiatedProgress
public export
record WorkDoneProgressOptions where
  constructor MkWorkDoneProgressOptions
  workDoneProgress : Maybe Bool
%runElab derive "WorkDoneProgressOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#clientInitiatedProgress
public export
record WorkDoneProgressParams where
  constructor MkWorkDoneProgressParams
  workDoneToken : Maybe ProgressToken
%runElab derive "WorkDoneProgressParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#partialResultParams
public export
record PartialResultParams where
  constructor MkPartialResultParams
  partialResultToken : Maybe ProgressToken
%runElab derive "PartialResultParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workDoneProgressBegin
public export
record WorkDoneProgressBegin where
  constructor MkWorkDoneProgressBegin
  title : String
  cancellable : Maybe Bool
  message : Maybe String
  percentage : Maybe Int
  kind : Only (JString "begin")
%runElab derive "WorkDoneProgressBegin" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workDoneProgressReport
public export
record WorkDoneProgressReport where
  constructor MkWorkDoneProgressReport
  cancellable : Maybe Bool
  message : Maybe String
  percentage : Maybe Int
  kind : Only (JString "report")
%runElab derive "WorkDoneProgressReport" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workDoneProgressEnd
public export
record WorkDoneProgressEnd where
  constructor MkWorkDoneProgressEnd
  message : Maybe String
  kind : Only (JString "end")
%runElab derive "WorkDoneProgressEnd" [FromJSON, ToJSON]

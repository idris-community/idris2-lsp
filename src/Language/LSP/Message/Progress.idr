module Language.LSP.Message.Progress

import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#progress
public export
ProgressToken : Type
ProgressToken = Int .+. String

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#clientInitiatedProgress
public export
record WorkDoneProgressOptions where
  constructor MkWorkDoneProgressOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{WorkDoneProgressOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#clientInitiatedProgress
public export
record WorkDoneProgressParams where
  constructor MkWorkDoneProgressParams
  workDoneToken : Maybe ProgressToken
%runElab deriveJSON defaultOpts `{{WorkDoneProgressParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#partialResultParams
public export
record PartialResultParams where
  constructor MkPartialResultParams
  partialResultToken : Maybe ProgressToken
%runElab deriveJSON defaultOpts `{{PartialResultParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workDoneProgressBegin
public export
record WorkDoneProgressBegin where
  constructor MkWorkDoneProgressBegin
  title : String
  cancellable : Maybe Bool
  message : Maybe String
  percentage : Maybe Int
%runElab deriveJSON (record {staticFields = [("kind", JString "begin")]} defaultOpts) `{{WorkDoneProgressBegin}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workDoneProgressReport
public export
record WorkDoneProgressReport where
  constructor MkWorkDoneProgressReport
  cancellable : Maybe Bool
  message : Maybe String
  percentage : Maybe Int
%runElab deriveJSON (record {staticFields = [("kind", JString "report")]} defaultOpts) `{{WorkDoneProgressReport}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workDoneProgressEnd
public export
record WorkDoneProgressEnd where
  constructor MkWorkDoneProgressEnd
  message : Maybe String
%runElab deriveJSON (record {staticFields = [("kind", JString "end")]} defaultOpts) `{{WorkDoneProgressEnd}}

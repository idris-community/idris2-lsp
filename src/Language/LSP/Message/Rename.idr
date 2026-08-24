module Language.LSP.Message.Rename

import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

namespace PrepareSupportDefaultBehaviour
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rename
  public export
  data PrepareSupportDefaultBehaviour = Identifier

export
ToJSON PrepareSupportDefaultBehaviour where
  toJSON Identifier = JNumber 1

export
FromJSON PrepareSupportDefaultBehaviour where
  fromJSON (JNumber 1) = pure Identifier
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rename
public export
record RenameClientCapabilities where
  constructor MkRenameClientCapabilities
  dynamicRegistration            : Maybe Bool
  prepareSupport                 : Maybe Bool
  prepareSupportDefaultBehaviour : Maybe PrepareSupportDefaultBehaviour
  honorsChangeAnnotation         : Maybe Bool
%runElab derive "RenameClientCapabilities" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rename
public export
record RenameOptions where
  constructor MkRenameOptions
  workDoneProgress : Maybe Bool
  prepareProvider  : Maybe Bool
%runElab derive "RenameOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rename
public export
record RenameRegistrationOptions where
  constructor MkRenameRegistrationOptions
  workDoneProgress : Maybe Bool
  prepareProvider  : Maybe Bool
  documentSelector : OneOf [DocumentSelector, Null]
%runElab derive "RenameRegistrationOptions" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_rename
public export
record RenameParams where
  constructor MkRenameParams
  workDoneToken : Maybe ProgressToken
  textDocument  : TextDocumentIdentifier
  newName       : String
%runElab derive "RenameParams" [FromJSON, ToJSON]

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareRename
public export
record PrepareRenameParams where
  constructor MkPrepareRenameParams
  textDocument : TextDocumentIdentifier
  position     : Position
%runElab derive "PrepareRenameParams" [FromJSON, ToJSON]

namespace PrepareRenameResponse
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareRename
  public export
  record PrepareRenameDefaultResponse where
    constructor MkPrepareRenameDefaultResponse
    defaultBehaviour : Bool
  %runElab derive "PrepareRenameDefaultResponse" [FromJSON, ToJSON]

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_prepareRename
  public export
  record PrepareRenamePlaceholderResponse where
    constructor MkPrepareRenamePlaceholderResponse
    range       : Range
    placeholder : String
  %runElab derive "PrepareRenamePlaceholderResponse" [FromJSON, ToJSON]

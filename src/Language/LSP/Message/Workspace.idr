module Language.LSP.Message.Workspace

import Data.SortedMap
import Language.JSON
import Language.LSP.Message.Derive
import Language.LSP.Message.DocumentSymbols
import Language.LSP.Message.Location
import Language.LSP.Message.Progress
import Language.LSP.Message.TextDocument
import Language.LSP.Message.URI
import Language.LSP.Message.Utils
import Language.Reflection

%language ElabReflection
%default total

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textEdit
public export
record TextEdit where
  constructor MkTextEdit
  range   : Range
  newText : String
%runElab deriveJSON defaultOpts `{{TextEdit}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textEdit
public export
record ChangeAnnotation where
  constructor MkChangeAnnotation
  label             : String
  needsConfirmation : Maybe Bool
  description       : Maybe String
%runElab deriveJSON defaultOpts `{{ChangeAnnotation}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textEdit
public export
ChangeAnnotationIdentifier : Type
ChangeAnnotationIdentifier = String

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textEdit
public export
record AnnotatedTextEdit where
  constructor MkAnnotatedTextEdit
  range        : Range
  newText      : String
  annotationId : ChangeAnnotationIdentifier
%runElab deriveJSON defaultOpts `{{AnnotatedTextEdit}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocumentEdit
public export
record TextDocumentEdit where
  constructor MkTextDocumentEdit
  textDocument : OptionalVersionedTextDocumentIdentifier
  edits        : List (OneOf [TextEdit, AnnotatedTextEdit])
%runElab deriveJSON defaultOpts `{{TextDocumentEdit}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#textDocument_completion
public export
record InsertReplaceEdit where
  constructor MkInsertReplaceEdit
  newText : String
  insert  : Range
  replace : Range
%runElab deriveJSON defaultOpts `{{InsertReplaceEdit}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#resourceChanges
public export
record CreateFileOptions where
  constructor MkCreateFileOptions
  overwrite      : Maybe Bool
  ignoreIfExists : Maybe Bool
%runElab deriveJSON defaultOpts `{{CreateFileOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#resourceChanges
public export
record RenameFileOptions where
  constructor MkRenameFileOptions
  overwrite      : Maybe Bool
  ignoreIfExists : Maybe Bool
%runElab deriveJSON defaultOpts `{{RenameFileOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#resourceChanges
public export
record DeleteFileOptions where
  constructor MkDeleteFileOptions
  recursive         : Maybe Bool
  ignoreIfNotExists : Maybe Bool
%runElab deriveJSON defaultOpts `{{DeleteFileOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#resourceChanges
public export
record CreateFile where
  constructor MkCreateFile
  uri          : DocumentURI
  options      : Maybe CreateFileOptions
  annotationId : ChangeAnnotationIdentifier
%runElab deriveJSON (record {staticFields = [("kind", JString "create")]} defaultOpts) `{{CreateFile}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#resourceChanges
public export
record RenameFile where
  constructor MkRenameFile
  oldUri       : DocumentURI
  newUri       : DocumentURI
  options      : Maybe RenameFileOptions
  annotationId : ChangeAnnotationIdentifier
%runElab deriveJSON (record {staticFields = [("kind", JString "rename")]} defaultOpts) `{{RenameFile}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#resourceChanges
public export
record DeleteFile where
  constructor MkDeleteFile
  uri          : DocumentURI
  options      : Maybe DeleteFileOptions
  annotationId : ChangeAnnotationIdentifier
%runElab deriveJSON (record {staticFields = [("kind", JString "delete")]} defaultOpts) `{{DeleteFile}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspaceEdit
public export
record WorkspaceEdit where
  constructor MkWorkspaceEdit
  changes           : Maybe (SortedMap DocumentURI (List TextEdit))
  documentChanges   : Maybe (List (OneOf [TextDocumentEdit, CreateFile, RenameFile, DeleteFile]))
  changeAnnotations : Maybe (SortedMap String ChangeAnnotation)
%runElab deriveJSON defaultOpts `{{WorkspaceEdit}}

namespace ResourceOperationKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspaceEditClientCapabilities
  public export
  data ResourceOperationKind = Create | Rename | Delete

export
ToJSON ResourceOperationKind where
  toJSON Create = JString "create"
  toJSON Rename = JString "rename"
  toJSON Delete = JString "delete"

export
FromJSON ResourceOperationKind where
  fromJSON (JString "create") = pure Create
  fromJSON (JString "rename") = pure Rename
  fromJSON (JString "delete") = pure Delete
  fromJSON _ = neutral

namespace FailureHandlingKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspaceEditClientCapabilities
  public export
  data FailureHandlingKind = Abort | Transactional | Undo | TextOnlyTransactional

export
ToJSON FailureHandlingKind where
  toJSON Abort                 = JString "abort"
  toJSON Transactional         = JString "transactional"
  toJSON Undo                  = JString "undo"
  toJSON TextOnlyTransactional = JString "textOnlyTransactional"

export
FromJSON FailureHandlingKind where
  fromJSON (JString "abort")                 = pure Abort
  fromJSON (JString "transactional")         = pure Transactional
  fromJSON (JString "undo")                  = pure Undo
  fromJSON (JString "textOnlyTransactional") = pure TextOnlyTransactional
  fromJSON _ = neutral

namespace WorkspaceEditClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspaceEditClientCapabilities
  public export
  record ChangeAnnotationSupport where
    constructor MkChangeAnnotationSupport
    groupsOnLabel : Maybe Bool
  %runElab deriveJSON defaultOpts `{{ChangeAnnotationSupport}}


||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspaceEditClientCapabilities
public export
record WorkspaceEditClientCapabilities where
  constructor MkWorkspaceEditClientCapabilities
  documentChanges         : Maybe Bool
  resourceOperations      : Maybe (List ResourceOperationKind)
  failureHandling         : Maybe FailureHandlingKind
  normalizesLineEndings   : Maybe Bool
  changeAnnotationSupport : Maybe ChangeAnnotationSupport
%runElab deriveJSON defaultOpts `{{WorkspaceEditClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_workspaceFolders
public export
record WorkspaceFoldersServerCapabilities where
  constructor MkWorkspaceFoldersServerCapabilities
  supported           : Maybe Bool
  changeNotifications : Maybe (OneOf [String, Bool])
%runElab deriveJSON defaultOpts `{{WorkspaceFoldersServerCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_workspaceFolders
public export
record WorkspaceFolder where
  constructor MkWorkspaceFolder
  uri  : DocumentURI
  name : String
%runElab deriveJSON defaultOpts `{{WorkspaceFolder}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeWorkspaceFolders
public export
interface WorkspaceFoldersChangeEvent where
  added   : List WorkspaceFolder;
  removed : List WorkspaceFolder;
%runElab deriveJSON defaultOpts `{{WorkspaceFoldersChangeEvent}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeWorkspaceFolders
public export
record DidChangeWorkspaceFoldersParams where
  constructor MkDidChangeWorkspaceFoldersParams
  event : WorkspaceFoldersChangeEvent
%runElab deriveJSON defaultOpts `{{DidChangeWorkspaceFoldersParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeConfiguration
public export
record DidChangeConfigurationClientCapabilities where
  constructor MkDidChangeConfigurationClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab deriveJSON defaultOpts `{{DidChangeConfigurationClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeConfiguration
public export
record DidChangeConfigurationParams where
  constructor MkDidChangeConfigurationParams
  settings : JSON
%runElab deriveJSON defaultOpts `{{DidChangeConfigurationParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_configuration
public export
record ConfigurationItem where
  constructor MkConfigurationItem
  scopeUri : Maybe DocumentURI;
  section  : Maybe String
%runElab deriveJSON defaultOpts `{{ConfigurationItem}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_configuration
public export
record ConfigurationParams where
  constructor MkConfigurationParams
  items : List ConfigurationItem
%runElab deriveJSON defaultOpts `{{ConfigurationParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeWatchedFiles
public export
record DidChangeWatchedFilesClientCapabilities where
  constructor MkDidChangeWatchedFilesClientCapabilities
  dynamicRegistration : Maybe Bool
%runElab deriveJSON defaultOpts `{{DidChangeWatchedFilesClientCapabilities}}

namespace WatchKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeWatchedFiles
  public export
  data WatchKind = Create | Change | Delete

watchKindToBits8 : WatchKind -> Bits8
watchKindToBits8 Create = 1
watchKindToBits8 Change = 2
watchKindToBits8 Delete = 4

export
ToJSON (List WatchKind) where
  toJSON = toJSON . foldr (prim__or_Bits8 . watchKindToBits8) 0

export
FromJSON (List WatchKind) where
  fromJSON (JNumber x) = pure $ filter ((/=) 0 . prim__and_Bits8 (cast $ cast {to = Integer} x) . watchKindToBits8) [Create, Change, Delete]
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeWatchedFiles
public export
record FileSystemWatcher where
  constructor MkFileSystemWatcher
  globPattern : String
  kind        : Maybe (List WatchKind)
%runElab deriveJSON defaultOpts `{{FileSystemWatcher}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeWatchedFiles
public export
record DidChangeWatchedFilesRegistrationOptions where
  constructor MkDidChangeWatchedFilesRegistrationOptions
  watchers : List FileSystemWatcher
%runElab deriveJSON defaultOpts `{{DidChangeWatchedFilesRegistrationOptions}}

namespace FileChangeType
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeWatchedFiles
  public export
  data FileChangeType = Created | Changed | Deleted

export
ToJSON FileChangeType where
  toJSON Created = JNumber 1
  toJSON Changed = JNumber 2
  toJSON Deleted = JNumber 3

export
FromJSON FileChangeType where
  fromJSON (JNumber 1) = pure Created
  fromJSON (JNumber 2) = pure Changed
  fromJSON (JNumber 3) = pure Deleted
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeWatchedFiles
public export
record FileEvent where
  constructor MkFileEvent
  uri : DocumentURI
  type : FileChangeType
%runElab deriveJSON defaultOpts `{{FileEvent}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didChangeWatchedFiles
public export
record DidChangeWatchedFilesParams where
  constructor MkDidChangeWatchedFilesParams
  changes : List FileEvent
%runElab deriveJSON defaultOpts `{{DidChangeWatchedFilesParams}}

namespace WorkspaceSymbolClientCapabilities
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_symbol
  public export
  record SymbolKindClientCapabilities where
    constructor MkSymbolKindClientCapabilities
    valueSet : Maybe (List SymbolKind)
  %runElab deriveJSON defaultOpts `{{SymbolKindClientCapabilities}}

  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_symbol
  public export
  record TagSupportClientCapabilities where
    constructor MkTagSupportClientCapabilities
    valueSet : List SymbolTag
  %runElab deriveJSON defaultOpts `{{TagSupportClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_symbol
public export
record WorkspaceSymbolClientCapabilities where
  constructor MkWorkspaceSymbolClientCapabilities
  dynamicRegistration : Maybe Bool
  symbolKind          : Maybe SymbolKindClientCapabilities
  tagSupport          : TagSupportClientCapabilities
%runElab deriveJSON defaultOpts `{{WorkspaceSymbolClientCapabilities}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_symbol
public export
record WorkspaceSymbolOptions where
  constructor MkWorkspaceSymbolOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{WorkspaceSymbolOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_symbol
public export
record WorkspaceSymbolRegistrationOptions where
  constructor MkWorkspaceSymbolRegistrationOptions
  workDoneProgress : Maybe Bool
%runElab deriveJSON defaultOpts `{{WorkspaceSymbolRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_symbol
public export
record WorkspaceSymbolParams where
  constructor MkWorkspaceSymbolParams
  partialResultToken : Maybe ProgressToken
  query              : String
%runElab deriveJSON defaultOpts `{{WorkspaceSymbolParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_applyEdit
public export
record ApplyWorkspaceEditParams where
  constructor MkApplyWorkspaceEditParams
  label : Maybe String
  edit  : WorkspaceEdit
%runElab deriveJSON defaultOpts `{{ApplyWorkspaceEditParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_applyEdit
public export
record ApplyWorkspaceEditResponse where
  constructor MkApplyWorkspaceEditResponse
  applied       : Bool
  failureReason : Maybe String
  failedChange  : Maybe Integer
%runElab deriveJSON defaultOpts `{{ApplyWorkspaceEditResponse}}

namespace FileOperationPatternKind
  ||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_willCreateFiles
  public export
  data FileOperationPatternKind = FileKind | FolderKind

export
ToJSON FileOperationPatternKind where
  toJSON FileKind   = JString "file"
  toJSON FolderKind = JString "folder"

export
FromJSON FileOperationPatternKind where
  fromJSON (JString "file")   = pure FileKind
  fromJSON (JString "folder") = pure FolderKind
  fromJSON _ = neutral

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_willCreateFiles
public export
record FileOperationPatternOptions where
  constructor MkFileOperationPatternOptions
  ignoreCase : Maybe Bool
%runElab deriveJSON defaultOpts `{{FileOperationPatternOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_willCreateFiles
public export
record FileOperationPattern where
  constructor MkFileOperationPattern
  glob    : String
  matches : Maybe FileOperationPatternKind
  options : FileOperationPatternOptions
%runElab deriveJSON defaultOpts `{{FileOperationPattern}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_willCreateFiles
public export
record FileOperationFilter where
  constructor MkFileOperationFilter
  scheme  : Maybe String
  pattern : FileOperationPattern
%runElab deriveJSON defaultOpts `{{FileOperationFilter}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_willCreateFiles
public export
record FileOperationRegistrationOptions where
  constructor MkFileOperationRegistrationOptions
  filters : List FileOperationFilter
%runElab deriveJSON defaultOpts `{{FileOperationRegistrationOptions}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_willCreateFiles
public export
record FileCreate where
  constructor MkFileCreate
  uri : URI
%runElab deriveJSON defaultOpts `{{FileCreate}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_willCreateFiles
public export
record CreateFilesParams where
  constructor MkCreateFilesParams
  files : List FileCreate
%runElab deriveJSON defaultOpts `{{CreateFilesParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_willRenameFiles
public export
record FileRename where
  constructor MkFileRename
  oldUri : URI
  newUri : URI
%runElab deriveJSON defaultOpts `{{FileRename}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_willRenameFiles
public export
record RenameFilesParams where
  constructor MkRenameFilesParams
  files : List FileRename
%runElab deriveJSON defaultOpts `{{RenameFilesParams}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didRenameFiles
public export
record FileDelete where
  constructor MkFileDelete
  uri : URI
%runElab deriveJSON defaultOpts `{{FileDelete}}

||| Refer to https://microsoft.github.io/language-server-protocol/specification.html#workspace_didRenameFiles
public export
record DeleteFilesParams where
  constructor MkDeleteFilesParams
  files : List FileDelete
%runElab deriveJSON defaultOpts `{{DeleteFilesParams}}

# Supported Commands

The following sections will be organized in the same manner as the LSP
specification, with the same syntax. For each command we will provide the
structure of the params and both valid and error responses.

## Repl Request

The repl request is sent from the client to the server to run the provided
argument as if it was in a REPL session. The result is a string containing the
REPL response.
This is useful when clients want to evaluate expressions or invoke
REPL-specific commands. Note that this command is __not stateless__ and further
invokation may be influenced by previous results, furthermore saving files in
the workspace causes the context to be reset.

_Request_:
  - method: 'workspace/executeCommand'
  - params: `ReplParams` defined as follows:
```typescript
export interface ReplParams {
  /**
   * The identifier of the actual command handler.
   */
  command: 'repl';
  /**
   * Arguments to be passed to the REPL, currently only one argument is supported.
   */
  arguments: string[];
}
```

_Response_:
  - result: `string`
  - error: code and message set in case an exception happens during the request.

## Metavars Request

The metavars request is sent from the client to the server to return the list
of metavariables found in the workspace. Each result is annotated with its
location in the workspace.

_Request_:
  - method: 'workspace/executeCommand'
  - params: `MetavarsParams` defined as follows:
```typescript
export interface MetavarsParams {
  /**
   * The identifier of the actual command handler.
   */
  command: 'metavars';
  /**
   * No arguments required.
   */
  arguments?: null;
}
```

_Response_:
  - result: `Metavar[]` defined as below.
  - error: code and message set in case an exception happens during the request.

```typescript
export interface Premise {
  /**
   * The location of this premise.
   */
  location: Location | null;
  /**
   * The name of this premise.
   */
  name: string;
  /**
   * The type of this premise.
   */
  type: string;
  /**
   * A flag which indicates whether the premise is an implicit argument.
   */
  isImplicit: boolean;
}

export interface Metavar {
  /**
   * The location of this metavariable.
   */
  location: Location | null;
  /**
   * Name of this metavariable.
   */
  name: string;
  /**
   * The type of this metavariable.
   */
  type: string;
  /**
   * The list of premises of this metavariable.
   */
  premises: Premise[];
}
```

## ExprSearchWithHints Request

The expression search with hints request is sent from the client to the server
to compute an expression search with a custom list of hints. These requests are
handled by the same library that handles the expression search code action
potentially sent by the server in response to a `textDocument/codeAction`
request on a metavariable, thus the response is in the same format. Even with
hints the list of `CodeAction` as response could be the same as without hints.

_Request_:
  - method: 'workspace/executeCommand'
  - params: `ExprSearchParams` defined as follows:
```typescript
export interface ExprSearchParams {
  /**
   * The identifier of the actual command handler.
   */
  command: 'exprSearchWithHints';
  /**
   * Context and list of hints. Currently supports a single object.
   */
  arguments: ExprSearchHints[];
}

export interface ExprSearchHints {
  /**
   * Location and context of this search.
   */
  codeAction: CodeActionParams;
  /**
   * List of names to be hinted for this search.
   */
  hints: string[];
}
```

_Response_:
  - result: `CodeAction[] | null`.
  - error: code and message set in case an exception happens during the request.

## RefineHoleWithHints Request

The refine hole with hints request is sent from the client to the server
to compute a refine hole with a custom list of hints. These requests are
handled by the same library that handles the refine hole code action
potentially sent by the server in response to a `textDocument/codeAction`
request on a metavariable, thus the response is in the same format.
Response edits in the `CodeAction`s must contain the hinted names.

_Request_:
  - method: 'workspace/executeCommand'
  - params: `RefineHoleParams` defined as follows:
```typescript
export interface RefineHoleParams {
  /**
   * The identifier of the actual command handler.
   */
  command: 'refineHoleWithHints';
  /**
   * Context and list of hints. Currently supports a single object.
   */
  arguments: RefineHoleHints[];
}

export interface RefineHoleHints {
  /**
   * Location and context of this search.
   */
  codeAction: CodeActionParams;
  /**
   * List of names to be hinted for this search.
   */
  hints: string[];
}
```

_Response_:
  - result: `CodeAction[] | null`.
  - error: code and message set in case an exception happens during the request.

## BrowseNamespace Request

The browseNamespace request is sent from the client to the server to return the list
of visible names found in a given namespace.

_Request_:
  - method: 'workspace/executeCommand'
  - params: `BrowseNamespaceParams` defined as follows:
```typescript
export interface BrowseNamespaceParams {
  /**
   * The identifier of the actual command handler.
   */
  command: 'browseNamespace';
  /**
   * List of namespaces. Currently supports a single object.
   */
  arguments: string[];
}
```

_Response_:
  - result: `SymbolInformation[]`.
  - error: code and message set in case an exception happens during the request.

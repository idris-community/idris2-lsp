# idris2-lsp
Language Server for Idris2.

**NOTE: At this stage the LSP server requires an ipkg file to work correctly, for reference material about packages look [here](https://idris2.readthedocs.io/en/latest/tutorial/packages.html) and [here](https://idris2.readthedocs.io/en/latest/reference/packages.html). To start a new project with an ipkg, even for a single file, you can issue the `idris2 --init` command, which provides an interactive interface for package creation.**

## Installation with [Pack](https://github.com/stefan-hoeck/idris2-pack) (Recommended)
```bash
pack install-app idris2-lsp
```

## Manual Installation
```bash
# If you already have idris2, uninstall idris2 unless you installed it from source
git clone https://github.com/idris-community/idris2-lsp.git # Clone this repository
cd idris2-lsp
git submodule update --init Idris2 # Get the associated Idris commit
cd Idris2 # Change into the Idris2 directory
make bootstrap SCHEME=chez # Boostrap Idris
make install # Install Idris
# If needed, modify your shell files to ensure ~/.idris2/bin is in your PATH
make clean # Clean Idris
make all # Build Idris
make install # Install Idris
make install-with-src-libs # Install sources for libraries
make install-with-src-api # Install the API with sources
cd .. # Go back to the idris2-lsp directory
git submodule update --init LSP-lib # Get the associated LSP-lib commit
cd LSP-lib
idris2 --install-with-src # Install the LSP library
cd .. # Go back to the idris2-lsp directory
make install # Install idris2-lsp
```

Can be installed on Windows via MSYS2.

## Editor Plugins
- VSCode: [idris2-lsp-vscode](https://github.com/bamboo/idris2-lsp-vscode)
- Neovim: [idris2-nvim](https://github.com/ShinKage/idris2-nvim)

Refer to the [project wiki](https://github.com/idris-community/idris2-lsp/wiki) for editor-specific configurations.

## Compile
To compile `idris2-lsp` you need a working installation of the Idris2 compiler (available [here](https://github.com/idris-lang/Idris2)) and you also need the `idris2api` package. See the [install guide](https://github.com/idris-lang/Idris2/blob/master/INSTALL.md) how to build the `idris2api` package.

**NOTE: The version of the Idris2 compiler available as submodule is the only tested version, we will try to keep in sync with the latest master. For specific releases of the compiler refer to the branches with the same name as the release.**

## Install
Run `make install` to install the server, by default it will be placed in the same default directory of the Idris2 compiler, i.e. `~/.idris2/bin`.

## Go to commands and package dependencies
The server provides support for some go to commands, e.g. go to definition, however to reach modules declared in other packages you must install packages with `idris2 --install-with-src` instead of `idris2 --install`. To access the standard library run `make install-with-src-libs` after building the compiler and `make install-with-src-api` if you also want to access to the `idris2api` package.

## Configuration options
Server options that can be set via the `initializationOptions` object in the initialization message or via `workspace/didChangeConfiguration`:

|Option key|Type|Description|
|----------|----|-----------|
|`logFile`|`string`|Absolute location of the log file for the server (default: stderr)|
|`logSeverity`|`string`|Logs a string with the provided severity level (default: Debug)|
|`longActionTimeout`|`number`|Timeout in ms for long actions, e.g. expression search (default: 5000)|
|`maxCodeActionResults`|`number`|Maximum number of multiple code actions for a single command, e.g. expression search (default: 5)|
|`showImplicits`|`boolean`|Show implicits in hovers|
|`showMachineNames`|`boolean`|Show machine names in hovers|
|`fullNamespace`|`boolean`|Show full namespace in hovers|
|`briefCompletions`|`boolean`|Insert only the function name on completion (default: false)|
|`perLineFormatting`|`boolean`|Enable per-line formatting: spacing, trailing whitespace, etc. (default: true)|
|`structuralFormatting`|`boolean`|Enable structural formatting: blank lines, import sorting, sig+def cohesion (default: true)|
|`alignmentFormatting`|`boolean`|Enable alignment formatting: case arms, def `=`, record fields, data `\|` (default: true)|
|`operatorSpacingOps`|`string[]` or `null`|Operators to space around (default: `["$"]`). Set to `null` or `[]` to disable. See below.|

### Operator spacing

The formatter adds spaces around a configurable list of operators. The default is `["$"]` (the function application operator), which is safe for all Idris2 code.

The `operatorSpacingOps` option accepts a JSON array of operator strings. Operators are matched with word-boundary checks — a match is skipped when the character immediately before or after the operator is itself an operator character. This prevents `<$>`, `$=`, `$>`, `*>`, `</>` from being broken when `$` or `/` are in the list.

**Disable operator spacing entirely:**
```json
{ "operatorSpacingOps": null }
```
or
```json
{ "operatorSpacingOps": [] }
```

**Use only the default `$` spacing (explicit):**
```json
{ "operatorSpacingOps": ["$"] }
```

**Add arithmetic and comparison operators:**
```json
{ "operatorSpacingOps": ["$", "+", "*", "/", "<=", ">="] }
```
> **Note:** Arithmetic operators like `+` can produce false positives (e.g. `(+1)` → `( + 1)`). Enable them only if your codebase benefits.

**Neovim (`idris2-nvim`) example:**
```lua
require('idris2').setup({
  server = {
    init_options = {
      operatorSpacingOps = { "$" }  -- default; use {} to disable
    }
  }
})
```

**VSCode (`idris2-lsp-vscode`) example** — add to your `settings.json`:
```json
"idris2.initializationOptions": {
  "operatorSpacingOps": ["$"]
}
```

### `idris2-fmt` command-line formatter

The standalone formatter binary (`~/.idris2/bin/idris2-fmt`) accepts the same operator list via flags:

```bash
# Default (only $)
idris2-fmt < MyFile.idr

# Custom operator list
idris2-fmt --ops "$,+,*,/" < MyFile.idr

# Disable all operator spacing
idris2-fmt --no-ops < MyFile.idr

# Other flags
idris2-fmt --no-structural --no-alignment --no-per-line < MyFile.idr
idris2-fmt --range < fragment.idr    # range/on-type mode (no structural)
idris2-fmt --literate < MyFile.lidr
```

## Code Actions Filters
As per specification, client can filter requested code actions with the `context.only` field in the request parameters.
The following table specifies the list of keys that enable each code actions. A `null` field enables all actions, while an empty array disables all actions.

|Code Action|Allowed Keys|
|-----------|------------|
|`CaseSplit`|`refactor.rewrite`,`refactor.rewrite.CaseSplit`|
|`ExprSearch`|`refactor.rewrite`,`refactor.rewrite.ExprSearch`|
|`GenerateDef`|`refactor.rewrite`,`refactor.rewrite.GenerateDef`|
|`Intro`|`refactor.rewrite`,`refactor.rewrite.Intro`|
|`MakeCase`|`refactor.rewrite`,`refactor.rewrite.MakeCase`|
|`MakeClause`|`refactor.rewrite`,`refactor.rewrite.MakeClause`|
|`MakeLemma`|`refactor.extract`,`refactor.extract.MakeLemma`|
|`MakeWith`|`refactor.rewrite`,`refactor.rewrite.MakeWith`|
|`RefineHole`|`refactor.rewrite`,`refactor.rewrite.RefineHole`|

## Examples

Some examples can be found in the `test/lsp/example` directory. Many of the existing functionality still needs to be documented.

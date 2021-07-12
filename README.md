# [WIP] idris2-lsp
Language Server for Idris2.

Refer to the [project wiki](https://github.com/idris-community/idris2-lsp/wiki) for editor-specific configurations.

**NOTE: At this stage the LSP server requires an ipkg file to work correctly, for reference material about packages look [here](https://idris2.readthedocs.io/en/latest/tutorial/packages.html) and [here](https://idris2.readthedocs.io/en/latest/tutorial/packages.html). To start a new project with an ipkg, even for a single file, you can issue the `idris2 --init` command, which provides an interactive interface for package creation.**

## Compile
To compile `idris2-lsp` you need a working installation of the Idris2 compiler (available [here](https://github.com/idris-lang/Idris2)) and you also need the `idris2api` package. See the [install guide](https://github.com/idris-lang/Idris2/blob/master/INSTALL.md) how to build the `idris2api` package.

**NOTE: The version of the Idris2 compiler available as submodule is the only tested version, we will try to keep in sync with the latest master. For specific releases of the compiler refer to the branches with the same name as the release.**

## Install
Run `make install` to install the server, by default it will be placed in the same default directory of the Idris2 compiler, i.e. `~/.idris2/bin`.

## Go to commands and package dependencies
The server provides support for some go to commands, e.g. go to definition, however to reach modules declared in other packages you must install packages with `idris2 --install-with-src` instead of `idris2 --install`. To access the standard library run `make install-with-src-libs` after building the compiler and `make install-with-src-api` if you also want to access to the `idris2api` package.

## Configuration options
Server options that can be set via the `initializationOptions` object in the initialization message:

|Option key|Type|Description|
|----------|----|-----------|
|`logFile`|`string`|Absolute location of the log file for the server (default: stderr)|
|`longActionTimeout`|`number`|Timeout in ms for long actions, e.g. expression search (default: 5000)|
|`maxCodeActionResults`|`number`|Maximum number of multiple code actions for a single command, e.g. expression search (default: 5)|

## Examples

Some examples can be found in the `test/lsp/example` directory. Many of the existing functionality still needs to be documented.

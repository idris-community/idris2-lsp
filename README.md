# [WIP] idris2-lsp
Language Server for Idris2.

Refer to the [project wiki](https://github.com/idris-community/idris2-lsp/wiki) for editor-specific configurations.

## Compile
To compile `idris2-lsp` you need `Idris2` from the [master](https://github.com/idris-lang/Idris2) branch and you also need the `idris2api`. See the [install guide](https://github.com/idris-lang/Idris2/blob/master/INSTALL.md) how to build the `idris2api`.

**NOTE: The version of the Idris2 compiler available as submodule the only tested version. Other versions are not likely to work as the Idris2 API evolves rapidly.**

## Configuration options
Server options that can be set via the `initializationOptions` object in the initialization message:

|Option key|Type|Description|
|----------|----|-----------|
|`logFile`|`string`|Absolute location of the log file for the server (default: stderr)|
|`longActionTimeout`|`number`|Timeout in ms for long actions, e.g. expression search (default: 5000)|

## Examples

Some examples can be found in the `test/lsp/example` directory. Many of the existing functionality still needs to be documented.

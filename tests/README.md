# Formatting Tests

Standalone tests for the `formatIdrisSource` function.

## Running Tests

```bash
cd tests
idris2 --exec main FormatTest.idr
```

## What is Tested

The `formatIdrisSource` function implements basic Idris2 source formatting:

1. **Trim trailing whitespace** - Removes trailing spaces from each line
2. **Preserve trailing newlines** - Maintains proper newline at end of file
3. **Handle empty input** - Gracefully handles empty strings
4. **Idris code formatting** - Formats typical Idris source code

## Implementation Notes

The formatter uses `lines` and `unlines` for line handling:
- `lines` splits on newlines, producing empty string for trailing newline
- `unlines` joins with newlines, always adding trailing newline

This matches the LSP `insertFinalNewline` option behavior.

## Future Improvements

The current implementation is basic. Future enhancements could include:
- Tab/space conversion based on `tabSize` and `insertSpaces` options
- Indentation normalization
- Operator spacing normalization
- Literate code handling (`.lidr` files with `>` prefix)

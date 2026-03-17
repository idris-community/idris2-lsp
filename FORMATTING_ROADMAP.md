# Formatter Roadmap

## Done

### Per-line transforms
- [x] Trailing whitespace trimming
- [x] Tab → space conversion
- [x] Line ending normalization (CRLF/CR → LF)
- [x] Interior space collapsing
- [x] Comma spacing (`a,b` → `a, b`)
- [x] Colon spacing (`x:Nat` → `x : Nat`)
- [x] Equals / `==` / `/=` / `:=` spacing
- [x] Brace spacing (`{x}` → `{ x }`)
- [x] Pipe `|` / `||` / `&&` / `++` / `<-` spacing
- [x] Arrow `->` / `=>` spacing
- [x] Configurable operator spacing (default: `$`; arithmetic ops opt-in via `operatorSpacingOps`)
- [x] Comment spacing (`--x` → `-- x`)
- [x] Trailing comma removal in records

### Structural (multi-line)
- [x] Blank line normalization (collapse multiples / strip leading + trailing)
- [x] Blank line after `module` declaration
- [x] Import block: sorting + deduplication + blank after block
- [x] Type signature + definition cohesion (no blank between them)
- [x] Doc comment (`|||`) attachment to following declaration
- [x] Blank line between top-level definitions
- [x] Case arm `=>` alignment within a case block
- [x] Definition `=` alignment across function clause groups

### LSP handlers
- [x] `textDocument/formatting` — full-file formatting
- [x] `textDocument/rangeFormatting` — per-line transforms only, no structural normalization
- [x] `textDocument/onTypeFormatting` — same as range formatting, triggered on keystroke

---

## Remaining

### Lower risk
- [x] **Record field `=` alignment** — align `=` across fields in multi-line record literals
- [x] **Data constructor `|` alignment** — align `|` with `=` in hanging-style `data` declarations
- [ ] **Import grouping** — group by top-level namespace (reverted: triggers Chez GC regression via reLine in sort)

### Medium risk
- [ ] **`where` block blank lines** — strip consecutive blanks inside `where`; ensure a blank line before `where`
- [ ] **Trailing `where`** — move a lone `where` on its own line to the end of the preceding line
- [ ] **Multi-line list / tuple indentation** — align elements in wrapped `[a, b, c]` or tuples

### Higher risk / probably out of scope
- [ ] **Unary `-` spacing** — can't reliably distinguish from binary `-` without a parse tree
- [ ] **Operator precedence parentheses** — requires full parse tree
- [ ] **`mutual` / `namespace` block formatting** — complex nesting rules
- [ ] **Hole renaming** — semantic, not syntactic; belongs in code actions

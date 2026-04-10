# Roadmap

## v1.0 (Interpreter + Typechecker Release)

### Scope
- Lexer + parser working end-to-end
- Interpreter for all parsed constructs
- Typechecker for all parsed constructs
- Immutability documented and enforced
- No modules, no ADTs, no pattern matching, no LLVM backend

### Must‑have features
- Literals: int, float, string, bool, null
- Expressions: prefix/infix, if/else, function calls
- Statements: let, return, block
- Structs: declaration, initialization, field access
- Arrays: literals, indexing
- Functions: parameters + return types
- Union types (current system)

### Quality bar
- No partial functions or `error` in parser/typechecker
- Consistent positioned errors (expected/got)
- Tests for parser, typechecker, and eval
- `docs/semantics.md` matches behavior

### Release checklist
- [ ] Parser handles all constructs with tests
- [ ] Typechecker handles all constructs with tests
- [ ] Interpreter handles all constructs with tests
- [ ] `docs/semantics.md` reviewed and up to date
- [ ] `cabal test` passes
- [ ] Tag `v1.0.0`

---

## v2.0 (ADT + Pattern Matching)
- Add `enum`/ADT declarations
- Add pattern AST + `match` expression
- Typecheck exhaustiveness + pattern typing
- Interpreter support for ADTs + match

---

## v3.0 (Compiler + LLVM)
- Core IR and lowering pass
- Runtime + allocation API
- LLVM backend + linking

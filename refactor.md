# Refactor Ideas + Architecture Notes

## Ideas to Implement
- tighten parser error messages to include expected/got token types consistently
- unify lexer/token naming (e.g., `identifierAndKeywordsLexer` typo) and remove dead/duplicated helpers
- split parser into expression/statement modules to reduce file size and improve test focus
- extract precedence table into data structure to avoid long `case` chains
- make `Expression`/`TExpression` `token` fields consistent (`token` vs `tToken`) and align record field names
- avoid `error` in parser/typechecker; return structured errors instead
- normalize `UnionT` handling (e.g., merge/simplify in one place) and add tests for union inclusion
- add a dedicated `Env` module for both eval and typechecker to remove Map duplication
- add a `Positioned` helper to attach positions to errors uniformly across lexer/parser/eval/typechecker
- improve `IfExpression` typing to return union of branch types rather than `VoidT`
- add a safe `popRetTy` that handles empty stack (avoid partial `tail`)
- use newtype wrappers for `StructName`, `FieldName` for stronger typing
- ensure `TypeChecker` handles arrays, function calls, and index expressions (currently missing)
- align `Null` token vs `NullT` naming and literal casing (`Null` vs `null`)
- consolidate duplicated `initialState` helpers in tests
- add property tests for lexer/parser round-trips and precedence stability
- consider a single `Builtin` registry used by both eval and typechecker

## Architecture Notes
- Pipeline is: lexer (`Token.hs`) -> parser (`Parser.hs`) -> evaluator (`Eval.hs`) and typechecker (`TypeChecker.hs`) in parallel.
- AST lives in `Ast.hs` with both untyped and typed variants; parser produces untyped AST, typechecker produces typed AST.
- Parser uses a Pratt-style precedence loop with `parseNud` + `continueInfix` and `Precedence` in `Ast.hs`.
- Lexer uses a custom `SimpleParser` combinator monad; parser reuses the same combinator core.
- Types are modeled in `Types.hs` with `UnionT`, `StructT`, `FnT` and env tracking in `TypecheckEnv`.
- Struct declarations are parsed into statements, then filtered and moved into `typeDefs` during `structResolutionPhase`.
- Eval is a stateful interpreter with `Enviroment` mapping identifiers to runtime `Object` values; functions capture env.
- Tests are split by area (token/parser/eval/typechecker) using Hspec; no top-level test runner yet.

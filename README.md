# monkeyLang

`monkeyLang` is a small statically checked programming language implemented in Haskell.

I built this project to explore language implementation and static analysis from the ground up: lexing, parsing, evaluation, scope handling, and type checking.

## What it includes

- hand-written lexer
- Pratt parser
- tree-walking interpreter
- static typechecker
- source-positioned diagnostics
- automated tests for lexer, parser, evaluator, and typechecker

## Current language features

- `let` bindings
- mutable bindings with `mut`
- `return`
- `if` expressions
- `while` loops
- functions and closures
- integers, floats, strings, booleans, and `Null`
- arrays and indexing
- structs and field access
- union types
- basic flow-sensitive type narrowing

## Project structure

- `src/Token.hs` — lexer, tokens, and position tracking
- `src/Parser.hs` — parser
- `src/Eval.hs` — interpreter
- `src/TypeChecker.hs` — typechecker
- `src/Ast.hs` — AST definitions
- `src/SimpleParser.hs` — parser combinator core
- `docs/semantics.md` — language behavior notes
- `docs/roadmap.md` — planned work
- `test/Programs/` — sample programs

## Build

```bash
cabal build
```

## Run

Evaluate a program:

```bash
cabal run monkeyLang -- path/to/program.mol
```

Typecheck a program:

```bash
cabal run monkeyLang -- --check path/to/program.mol
```

## Test

Run everything:

```bash
cabal test
```

Run individual suites:

```bash
cabal test token
cabal test parser
cabal test eval
cabal test typechecker
```

## Example

```monkey
Struct Point{
  x : Int
  y : Int
}

let addPoint = fn(a: Point, b: Point): Point {
  return Point{x: a.x + b.x; y: a.y + b.y };
};

let p1 = Point{x: 1; y: 2};
let p2 = Point{x: 3; y: 4};
addPoint(p1, p2);
```

## Notes

This is an active personal project, not a finished language release. The implementation is still evolving, and some semantics and syntax may change as the typechecker and runtime grow.

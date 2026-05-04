# monkeyLang

A small experimental programming language implemented in Haskell.

`monkeyLang` currently includes:
- a lexer and Pratt parser
- an interpreter
- a static typechecker
- source-positioned error reporting
- tests for tokenizing, parsing, evaluation, and typechecking

## Language features

Current supported features include:
- `let` bindings
- mutable bindings with `mut`
- `return`
- `if` expressions
- `while` loops
- functions and closures
- integers, floats, strings, booleans, and `Null`
- arrays and indexing
- structs and field access
- union types and basic flow-sensitive narrowing

See:
- `docs/semantics.md`
- `docs/roadmap.md`
- `test/Programs/`

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

Run all tests:

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

## Status

This is an active personal language project and the implementation is still evolving. Expect rough edges, incomplete features, and breaking changes.

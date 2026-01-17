# AGENTS.md

Build, test, and style guidelines for agentic coding in this repository.

## Build, Lint, and Test Commands

### Build
```bash
cabal build                    # Build the library and executable
cabal build all               # Build all targets (same as above)
cabal clean                   # Clean build artifacts
```

### Run Tests
```bash
cabal test                    # Run all test suites
cabal test token              # Run only token lexer tests
cabal test parser             # Run only parser tests
cabal test --test-show-details=always  # Show detailed test output
```

### Run Executable
```bash
cabal run monkeyLang          # Run the main executable
```

### Notes
- No explicit lint command configured; use `cabal build` which runs with `-Wall` warnings
- Hspec doesn't support running individual tests via cabal; use `--test-show-details` for debugging
- Test suites are defined in `test/Token/` and `test/Parser/` directories

## Code Style Guidelines

### File Structure & Language
- Default language: GHC2021 (specified in cabal file)
- Put LANGUAGE pragmas at top of file, one per line alphabetically
- Use explicit module export lists to define API boundaries
- Module names match file names with capital first letter

### Imports
- Group imports: standard library first, then local modules
- Separate import groups with blank lines
- Use qualified imports when namespace clarity is needed: `import MyLib qualified`
- Keep import lists alphabetical and compact

### Formatting & Layout
- 2-space indentation throughout
- Type signatures above function definitions
- Use `where` clauses for helper functions and local bindings
- Align constructors in data declarations for readability
- Use `($)` instead of nested parentheses where appropriate

### Types & Data
- Use custom data types with record syntax for stateful data
- Prefix lens fields with underscore: `_tokenType`, `_errorMsg`, `getInput`
- Derive `Eq, Show` for data types by default
- Use `makeLenses` Template Haskell for generating lenses on record types
- Use `makeLensesFor` to create custom lens names when avoiding conflicts
- Enable `DuplicateRecordFields` when using same field names across types
- Use `MultiParamTypeClasses` for parser error typeclass flexibility

### Naming Conventions
- **Types:** PascalCase (`TokenType`, `LexerState`, `AstParserError`)
- **Constructors:** PascalCase, often matching the type (`IntLiteral`, `FunctionLit`)
- **Values/functions:** camelCase (`tokenizer`, `nextToken`, `parseLetStatement`)
- **Type variables:** single letters (`a`, `s`, `e`) with descriptive context
- **Parser functions:** prefix with `parse` (`parseExpression`, `parseStatement`)
- **Lexer functions:** suffix with `L` (`charL`, `intL`) to distinguish from other parsers
- **Helper functions:** descriptive names (`advancePosition`, `lookupIdent`)
- **State accessors:** descriptive names without prefix (`getInput`, `currentPosition`)

### Error Handling
- Define custom error types: `LexError`, `ParserError`, `AstParserError`
- Include position tracking in all error types (`_errorPosition`, `errorPosition`)
- Use `Either e a` for error propagation throughout parsers
- Implement `SimpleParserError` typeclass for error abstraction
- Pattern match on `Either` results, handle both Left and Right cases in tests
- Provide descriptive error messages with context

### Parser Combinator Patterns
- Use the custom `SimpleParser` monad for all parsing
- Combine parsers with applicative operators: `<*>`, `*>`, `<*`, `<$>`
- Use `choice` for trying multiple alternatives: `choice [parser1, parser2, parser3]`
- Use repetition combinators: `oneOrMore`, `zeroOrMore`, `optional`
- Implement `satisfy` predicates for character/token matching
- Use `peek` functions to look ahead without consuming input
- Track state (input string, position) through parser monad

### Testing Patterns (Hspec)
- Use nested structure: `describe "context" $ do` → `it "should do X" $ do`
- Define test helpers as separate functions returning `Spec`
- Use `shouldBe` for equality assertions
- Use `shouldSatisfy` for custom predicates on results
- Create helper predicates for expression matching: `isIntLitWithValue`, `isStringLit`
- Build mock state constructors: `initialState input` → `LexerState`
- Use `expectationFailure` for manual test failures in Left/Error cases
- Test both successful parsing and error cases

### Code Organization
- `src/Token.hs` - Lexer implementation, token types, position tracking
- `src/Ast.hs` - AST node definitions, lenses, precedence rules
- `src/Parser.hs` - Recursive descent parser, statement/expression parsing
- `src/SimpleParser.hs` - Core parser combinator library
- `test/Token/` - Token lexer tests
- `test/Parser/` - Parser tests

### Dependencies
Core: `base ^>=4.18.3.0`, `containers`, `lens`
Testing: `hspec ^>=2.10`, `QuickCheck ^>=2.14`
All dependencies listed in `monkeyLang.cabal`

### Template Haskell
- Use `{-# LANGUAGE TemplateHaskell #-}` at top of files using lenses
- Generate lenses with `makeLenses` after data type definitions
- Use `makeLensesFor` for customizing lens names to avoid conflicts

### Pragma Usage
- `{-# HLINT ignore "rule" #-}` to suppress specific hlint warnings
- `{-# OPTIONS_GHC -Wno-warning-name #-}` for per-file warning suppression
- Keep pragmas minimal and justified

### Comments & Documentation
- Code should be self-documenting through descriptive names
- Add comments only for complex logic or non-obvious behavior
- No inline comments for straightforward operations
- Document exported functions with type signatures

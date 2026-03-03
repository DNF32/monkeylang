**Phase 1 — Foundation (3-4 sessions)**
1. Add type annotation syntax to parser (`x: Int`, `: Int` on functions)
2. Define your `Type` data type
3. Define `TypeEnv` (just a `Map String Type`)
4. Write a `typecheck` function stub that walks AST and returns `Either EvalError ()`

---

**Phase 2 — Basic checking (4-5 sessions)**

5. Typecheck literals (`5` → `TInt`, `true` → `TBool`)
6. Typecheck infix expressions (`+` requires both sides `TInt`)
7. Typecheck let statements with annotations
8. Typecheck variable lookups
9. Wire into your pipeline, test with simple cases

---

**Phase 3 — Functions (4-5 sessions)**

10. Typecheck function params (add to `TypeEnv`)
11. Typecheck function body
12. Check return type annotation matches last expression
13. Typecheck function calls

---

**Phase 4 — Unions (3-4 sessions)**

14. Add `TUnion` to your `Type`
15. Add `mkUnion` and `typeMatches` helpers
16. Allow union return types on functions

---

**Phase 5 — Narrowing (4-5 sessions)**

17. Detect `x != null` / `x == null` in if conditions
18. Apply narrowing to then/else branch environments
19. Merge environments after branch

---

Each session has a clear deliverable. Phase 5 is the hardest — don't rush to it. What's your parser look like for annotations, do you have any syntax for them yet?



--------------

References for typecheker

**"Crafting Interpreters"** by Robert Nystrom
- Free online at craftinginterpreters.com
- Builds a full language from scratch, has a typechecking chapter
- Very practical, beginner friendly

**"Types and Programming Languages"** (TAPL) by Benjamin Pierce
- The bible for type theory
- Covers HM inference, unions, structs, everything
- Dense but very precise — use as reference not cover to cover

**"Programming Language Pragmatics"** by Scott
- More accessible than TAPL
- Good balance of theory and practice

---

**Recommended approach for you specifically:**

1. **Start with Crafting Interpreters** — you'll recognize a lot since you've already built an interpreter
2. **Use TAPL as a reference** — when you hit a specific problem (like union narrowing) look it up there
3. Don't read cover to cover, read when you hit a wall

---

TAPL is the one everyone in PL theory cites. If you read even the first 10 chapters you'll understand what most people spend years figuring out. It uses OCaml for examples which is close enough to Haskell that you'll follow it fine.

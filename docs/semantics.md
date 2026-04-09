# MonkeyLang Semantics (Draft)

## Scope and bindings
- `let` creates a new binding in the current scope.
- Scopes are modeled as a local stack; bindings created inside a block do not escape that block.
- If a name is redefined inside a block, it shadows the outer binding for that block only.

## If expressions
- `if` is an expression and returns the value of the last expression in the chosen branch.
- `if` with no `else` returns a union of the branch value and `Null`.
- Branch-local `let` bindings do not escape the `if` expression scope.

## Type narrowing
- Narrowing is flow-sensitive and applies only to bindings that exist before the branch.
- `if (x)` narrows `x` to its truthy/falsy types inside each branch.
- `x == Null` narrows `x` to `Null` in the true branch and removes `Null` in the false branch.
- `x != Null` narrows `x` to non-null in the true branch and to `Null` in the false branch.
- Narrowing does not propagate through aliases.

## Equality and comparison
- `==` and `!=` are allowed when the operand types overlap.
- Comparisons (`<`, `>`, etc.) are allowed only on numeric types.

## Branch merge rules
- After an `if` with `else`, only variables that exist in the outer scope are merged.
- Variables declared in only one branch do not exist after the `if`.
- Types of existing variables are merged by union when they differ.
- If one branch always returns, only the non-returning branch contributes to the merged env.

## Return behavior
- `return` exits the current function immediately.
- Return type inference collects all reachable return types and unions them.

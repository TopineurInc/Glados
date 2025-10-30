# Topineur + Lisp Roadmap (TODO)

This checklist tracks everything needed to parse and ultimately run both Lisp (.lisp) and Topineur (.top) in this repository. It’s split into phases so we can land value incrementally.

Status legend: [ ] not started · [~] in progress · [x] done

---

## Phase 0 — Planning and scope

- [ ] Formalize Topineur grammar and precedence/associativity
- [ ] Decide core lowering strategy (extend backend vs. desugar to existing Lisp core)
- [ ] Document runtime representation choices (lists, tuples, objects, floats)

---

## Phase 1 — Parsing only (no execution)

AST extensions (src/AST.hs):
- [ ] Add Type, Param, Field, Method, Annotation data types
- [ ] Extend Expr with Topineur constructs: EFloat, ETuple, EListLit, ELetTyped, EFuncDef, EObjectDef, EPackage, EWhile, EFor, EIfThenElse, EBlock, EReturn, EBinOp, EUnOp, EFieldAccess, EMethodCall, EIndex, EAssign, ESelf, EAnnotated
- [ ] Optional: carry source positions in Expr
- [ ] Add Constant CFloat (and others if needed later)

Parser (new: src/TopineurParser.hs):
- [ ] Implement lexer (idents, keywords, numbers int/float, strings, operators, punctuation)
- [x] Implement comments (lines starting with "|-" and regular whitespace)
- [x] Parse: `package` header
- [x] Parse: type annotations (primitives, List[T], Tuple[...], custom, TVar)
- [ ] Parse: `let name: Type = expr` (and mutable reassignment later)
- [ ] Parse: `def f(a: T, b: U): R { body }`
- [ ] Parse: `@annotation` before defs and lambdas; `fun (x: T): R -> expr`
- [ ] Parse: object type blocks `object type Name { fields; methods }`
- [ ] Parse: field defaults, `self`, method calls `obj.m(args)`, field access `obj.x`
- [ ] Parse: lists `[a, b, c]`, tuples `(a, b, ...)`, typed forms `List[T]`, `Tuple[T1,T2]`
- [ ] Parse: control flow `if ... then ... else if ... else ... end`
- [ ] Parse: `while cond do ... end` and `for i in a..b do ... end`
- [ ] Parse: operators (binary/unary) with precedence: + - * / %, == != < > <= >=, && ||, !, ++, range `..`
- [ ] Parse: index `arr[i]`, assignment `x = e`

Integration:
- [ ] Detect .top files in app/Main.hs and route to TopineurParser for AST dump modes
- [ ] Add golden test: parse examples/all.top → snapshot AST/pretty
- [x] Wire new Topineur parser test into test runner

---

## Phase 2 — Type system (Topineur)

- [ ] Implement Topineur type checker (nominal objects, generics for List[T], tuples, function types)
- [ ] Define builtin types: Int, Float, Bool, String, Unit, List, Tuple
- [ ] Type rules for all operators (including string `++`)
- [ ] Decide and enforce index semantics (0-based, bounds)
- [ ] Type-check annotations (e.g., @cache validity)
- [ ] Unit tests for typing (valid/invalid cases)

---

## Phase 3 — Lowering to core / backend

- [ ] Desugar if-else chains to nested if
- [ ] Desugar `for` + `a..b` to while with counters
- [ ] Desugar `{ ... }` blocks to `begin`-like sequencing in core
- [ ] Handle `top expr` returns inside functions
- [ ] Encode objects (records + closures) or extend runtime (decision from Phase 0)
- [ ] Map method calls and field access to chosen representation
- [ ] Map lists/tuples to runtime representation (native or encoded)

---

## Phase 4 — Runtime, Codegen, VM

Runtime/Value/Const:
- [ ] Add VFloat (and maybe VList/VTuple/VObject if going native)
- [ ] Add CFloat to constants

Builtins:
- [ ] Arithmetic and comparison for Float
- [ ] String concatenation `++`
- [ ] sqrt, abs
- [ ] List utilities: length, head, tail, map, filter, fold, range

Codegen/VM:
- [ ] Update CodeGen to handle new/lowered nodes
- [ ] Add missing bytecode ops if necessary (floats, index, field)
- [ ] Ensure tail calls still optimized

Integration:
- [ ] Update Compiler to support Topineur pipeline: parse → (macro N/A) → desugar → rename → closure conv → codegen
- [ ] CLI: update help to mention .top and new modes

---

## Phase 5 — Tests, docs, examples

Tests:
- [x] Unit tests: parser skeleton (package/comments)
- [x] Unit tests: type parser (primitives, generics, custom, errors)
- [ ] Unit tests: declarations/expressions/control flow/objects/operators/lists/tuples
- [ ] Golden tests for pretty-printed AST
- [ ] Typechecker tests
- [ ] Runtime E2E tests for Topineur programs (fibonacci, quicksort, geometry)

Docs:
- [ ] Write docs/TOPINEUR_LANG.md (grammar, typing, semantics, examples)
- [ ] Update README to document dual-language support
- [ ] Migration/coexistence guide (Lisp + Topineur side by side)

Examples/CI:
- [ ] Ensure examples/all.top parses cleanly (Phase 1)
- [ ] Add small runnable .top examples for CI once runtime is ready

---

## Open questions / decisions

- Representation of objects at runtime (native vs. encoded)
- Interop between Lisp and Topineur (shared builtins? cross-call?)
- REPL scope: Lisp-only or accept Topineur snippets?
- Indexing base (0 vs. 1) and error behavior on bounds

Build tooling:
- [ ] Decide on Hpack vs Cabal single source of truth (currently glados.cabal overrides package.yaml)
- [x] Ensure new modules/tests are added to glados.cabal when introduced

---

## References

- Examples to cover: `examples/all.top`
- Current Lisp pipeline: `SExprParser → MacroExpander → Desugar → AlphaRename → ClosureConversion → CodeGen → VM`

Keep this file in sync with the tracked checklist in the team’s task board.

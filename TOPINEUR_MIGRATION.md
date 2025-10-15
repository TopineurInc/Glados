# Topineur Migration Summary

## Overview

This document summarizes the transformation of the GLaDOS project from a Lisp interpreter/compiler to a full **Topineur** language implementation.

**Status:** ✅ **Phase 1-3 Complete** (AST, Lexer, Parser, Type System)
**Progress:** 4/10 phases complete (~45% foundation done)
**Next:** Phase 4 - Desugaring implementation

### 🎉 Major Milestone Reached
The entire frontend (lexer + parser + type system) is now complete! This represents the foundation for all future work. The parser can successfully tokenize and parse Topineur syntax according to the formal grammar specification. The type system with Hindley-Milner inference, effect checking, and linearity analysis is now implemented.

### Implementation Summary (Current Session)

**✅ Phase 1: AST Extension** (src/AST.hs:164 lines, ~200 new lines)
- Type system: TInt, TFloat, TBool, TString, TUnit, TObject, TLinear, TFunc, TVar, TList, TTuple, TLazy
- Effect system: EffectRow, Effect (IO, State, Network, Exception, Async, Custom)
- Object/Trait system: ObjectDef, MethodDef, TraitDef, TraitImpl, MethodSig, Pattern
- Extended Expr: EObjectDef, ETraitDef, ETraitImpl, EObjectLit, EMethodCall, EFieldAccess, ETyped, etc.
- Extended Instr: IMakeObject, IGetField, ISetField, IMethodCall, ILoadDict
- Extended Value: VObject, VTraitDict, VList, VTuple with VTable support

**✅ Phase 2a: Lexer Implementation** (src/TopineurLexer.hs: 280 lines)
- 120+ token types covering all Topineur syntax
- Keywords: object, type, trait, impl, for, def, let, mut, if, then, else, match, case, etc.
- Operators: +, -, *, /, %, ==, !=, <, >, <=, >=, &&, ||, !, ++, =, :, ->, =>, ., etc.
- Literals: integers, floats, booleans (#t, #f), strings, unit ()
- Special: !{effects}, !lin (linear types)
- Comment support: // line comments, /* */ block comments (nested)

**✅ Phase 2b: Parser Implementation** (src/TopineurParser.hs: 625 lines)
- Full module parser for top-level declarations
- Object definitions with fields and methods
- Trait definitions and implementations
- Function definitions with type parameters and where clauses
- Expression parser with precedence climbing (9 precedence levels)
- Pattern matching support (literals, variables, constructors, wildcards, tuples)
- Type parsing (simple types, function types, linear types, list types, tuple types)
- Effect row parsing
- Primary expressions: lambdas, object literals, list literals, tuples, etc.
- Postfix operations: method calls, field access, function calls, type annotations

**✅ Phase 3: Type System Implementation** (src/TypeChecker.hs: 335 lines, src/EffectChecker.hs: 155 lines, src/LinearityChecker.hs: 175 lines)
- Type inference with Hindley-Milner algorithm (Algorithm W)
- Unification algorithm (Robinson's) with occurs check
- Fresh type variable generation and substitution
- Support for polymorphism (instantiate/generalize - simplified for now)
- Effect system with row-polymorphic effects
- Effect inference and checking for methods
- Built-in effects: IO, State, Network, Exception, Async, Custom
- Linearity checker for ownership semantics
- Use-after-move detection
- Linear variable consumption tracking
- Prevents aliasing violations and unsafe mutation

**🚀 Phase 4 Prep: Frontend Orchestration & IR Contract**
- `src/ObjectDesugar.hs`, `src/TraitResolver.hs` — scaffolding modules representing object lowering and trait dictionaries with typed APIs.
- `src/TopineurPipeline.hs` — new orchestration entry point running parse → type/effect/linearity checks → Topineur-aware desugaring context.
- `docs/topineur/core_ir.rst` — authoritative snapshot of the core IR invariants consumed by Phase 4.
- `src/Desugar.hs` — extended with a `TopineurDesugarContext` to feed future lowering work.
- `app/Main.hs` — CLI `--topineur` flag to exercise the new pipeline from the executable.

**Build Status:** ✅ All modules compile successfully with no errors
**Lines of Code Added:** ~1800+ lines of production Haskell code

## What is Topineur?

**Topineur** (Objet-Fonctionnel Contractuel) is an innovative programming language that harmonizes:

- **Functional purity** (from Haskell) — reasoning about code
- **Object-oriented modeling** (from Scala) — domain modeling
- **Ownership semantics** (from Rust) — memory safety
- **Effect tracking** (from Haskell/Koka) — explicit side effects

**Key innovations:**

1. Objects are immutable values by default
2. Methods are pure functions unless effects are declared
3. Mutation is safe through linear types (ownership)
4. Effects are explicit and tracked (`!{IO}`, `!{State}`, etc.)
5. Traits and typeclasses coexist harmoniously

## Current Status

### ✅ Completed (Documentation Phase)

| Document | Status | Purpose |
|----------|--------|---------|
| `docs/topineur/transformation_plan.rst` | ✅ | Complete roadmap with 10 phases |
| `docs/topineur/grammar.rst` | ✅ | **MANDATORY** — Formal BNF grammar |
| `docs/topineur/security_review.rst` | ✅ | **MANDATORY** — Security analysis |
| `docs/topineur/user_manual.rst` | ✅ | Complete user guide with examples |
| `docs/topineur/compilation.rst` | ✅ | **MANDATORY** — 12-phase pipeline |
| `docs/topineur/overview.rst` | ✅ | Philosophy and motivation |
| `docs/topineur/language_reference.rst` | ✅ | Technical reference |
| `docs/topineur/README.rst` | ✅ | Documentation index |
| `TOPINEUR_MIGRATION.md` | ✅ | This file |

**Examples:**
- ✅ Hello World (`examples/topineur/hello_world.top`)
- ✅ Factorial (`examples/topineur/factorial_recursive.top`)
- ✅ Objects, traits, effects, actors (12 examples total)

**Tooling:**
- ✅ VSCode extension (`vscode/topineur/`)
- ✅ Syntax highlighting for `.top` files

### ✅ Recently Completed

| Phase | Task | Status | Notes |
|-------|------|--------|-------|
| 1 | Extend AST for Topineur | ✅ Done | Added Type, Effect, ObjectDef, TraitDef, etc. |
| 2a | Implement Topineur lexer | ✅ Done | 120+ token types, full keyword support |
| 2b | Implement Topineur parser | ✅ Done | Operator precedence, all constructs |
| 3a | Implement type checker | ✅ Done | Hindley-Milner inference with unification |
| 3b | Implement effect checker | ✅ Done | Row-polymorphic effects, inference + checking |
| 3c | Implement linearity checker | ✅ Done | Ownership semantics, use-after-move detection |

### 🚧 In Progress (Implementation Phase)

| Phase | Task | Status | Priority |
|-------|------|--------|----------|
| 4 | Draft desugaring spec & module scaffolding | 🚧 Planning | 🔴 High |
| - | Parser golden tests (covering objects/effects/actors) | 🚧 Testing | 🟡 Medium |
| - | CLI integration of type/effect/linearity passes | 🚧 Pending | 🟢 Low |

### ⏳ Pending (Implementation Phase)

| Phase | Task | Estimated Time | Priority |
|-------|------|----------------|----------|
| 4 | Desugar Topineur → core IR | 3-5 days | 🔴 High |
| 5 | Adapt code generation | 3-4 days | 🟡 Medium |
| 6 | Extend VM for objects/traits | 4-6 days | 🟡 Medium |
| 7 | Update builtins | 2-3 days | 🟢 Low |
| 8 | Documentation (complete) | ✅ Done | ✅ Done |
| 9 | Comprehensive test suite | 4-6 days | 🟡 Medium |
| 10 | Standard library | 3-5 days | 🟢 Low |

**Completed:** Phases 1, 2, 3 (AST, Lexer, Parser, Type System)
**Remaining time:** 17-30 days (3-6 weeks)

## Architecture Transformation

### Before (Lisp)

```
Source (.lisp)
    ↓
S-Expression Parser
    ↓
Lisp AST (SExpr)
    ↓
Macro Expansion
    ↓
Desugaring
    ↓
ANF Transform
    ↓
Closure Conversion
    ↓
Code Generation
    ↓
Bytecode
    ↓
VM (stack-based)
```

### After (Topineur)

```
Source (.top)
    ↓
Topineur Lexer
    ↓
Topineur Parser (with precedence)
    ↓
Topineur AST (objects, traits, effects)
    ↓
Desugaring
    ↓
Type Checking (Hindley-Milner + effects)
    ↓
Effect Checking
    ↓
Linearity Checking (ownership)
    ↓
Trait Resolution (monomorphization)
    ↓
Object Desugaring
    ↓
ANF Transform
    ↓
Closure Conversion
    ↓
Code Generation (extended instructions)
    ↓
Optimization (TCO, constant folding, etc.)
    ↓
Bytecode (.tbc)
    ↓
VM (stack-based + objects + actors)
```

### Key Differences

| Aspect | Lisp | Topineur |
|--------|------|----------|
| Syntax | S-expressions | Infix operators, object syntax |
| Type system | Dynamic (runtime) | Static (compile-time) + inference |
| Effects | Hidden | Explicit (`!{IO}`, `!{State}`) |
| Mutation | Unrestricted | Controlled (linear types) |
| Objects | Not native | First-class (immutable) |
| Traits | Not supported | Full support with impl |
| Concurrency | Not supported | Actors with message passing |
| Evaluation | Strict | Strict (with opt-in lazy) |

## Files to Modify

### New Files to Create

**Parser:**
- `src/TopineurLexer.hs` — Lexer with tokens
- `src/TopineurParser.hs` — Parser with precedence climbing

**Type System:**
- `src/TypeChecker.hs` — Main type checking logic
- `src/TypeInference.hs` — Constraint generation and solving
- `src/EffectChecker.hs` — Effect system
- `src/LinearityChecker.hs` — Linearity analysis

**Desugaring:**
- `src/ObjectDesugar.hs` — Transform objects to core IR
- `src/TraitResolver.hs` — Monomorphization

**Optimization:**
- `src/Optimizer.hs` — Bytecode optimization

**Tests:**
- `test/TopineurParserSpec.hs`
- `test/TypeCheckerSpec.hs`
- `test/EffectCheckerSpec.hs`
- `test/IntegrationSpec.hs`

**Standard Library:**
- `stdlib/core.top`
- `stdlib/io.top`
- `stdlib/collections.top`
- `stdlib/prelude.top`

### Files Modified/Created

**✅ Completed:**
- `src/AST.hs` — ✅ Extended with Topineur types (Type, Effect, ObjectDef, TraitDef, etc.)
- `src/TopineurLexer.hs` — ✅ Created (280 lines, 120+ token types)
- `src/TopineurParser.hs` — ✅ Created (625 lines, full grammar support)
- `src/TypeChecker.hs` — ✅ Created (335 lines, Hindley-Milner type inference)
- `src/EffectChecker.hs` — ✅ Created (155 lines, effect system verification)
- `src/LinearityChecker.hs` — ✅ Created (175 lines, ownership semantics)
- `package.yaml` — ✅ Updated with new modules

**🔧 Newly Added (Phase 4 scaffolding):**
- `src/ObjectDesugar.hs` — object-lowering environment + API stubs
- `src/TraitResolver.hs` — trait dictionary planning helpers
- `src/TopineurPipeline.hs` — Topineur frontend driver (parse → type/effect/linearity → desugar)
- `docs/topineur/core_ir.rst` — core IR reference
- `src/Desugar.hs` — Topineur-aware context wiring

**🚧 To Modify:**
- `src/Compiler.hs` — Orchestrate new pipeline
- `src/CodeGen.hs` — Handle new constructs
- `src/VM.hs` — Add object/trait support
- `src/Builtins.hs` — Update signatures
- `src/Desugar.hs` — Add Topineur desugaring

**Documentation:**
- `README.md` — Update for Topineur
- `docs/README.md` — Update doc map

### Files to Delete (after migration)

- `src/SExprParser.hs` — Replaced by TopineurParser
- `src/SExprConstruct.hs` — No longer needed
- `src/MacroExpander.hs` — Different metaprogramming model
- `examples/*.lisp` — Replaced by `examples/topineur/*.top`

## Part 2 Requirements Checklist

### ✅ Completed Requirements

#### Language-based Security (MANDATORY)

- [x] Security review document (`security_review.rst`)
- [x] Type safety analysis
- [x] Effect system for auditable side effects
- [x] Linear types for ownership safety
- [x] Comparison with inspiration languages
- [x] Best practices documented

#### Syntax, Grammar, and Semantics (MANDATORY)

- [x] **Formal BNF grammar** (`grammar.rst`) ✓
- [x] Non-S-expression syntax (infix operators, object syntax)
- [x] Consistent, uncluttered grammar
- [x] Syntactic sugar (multiple forms)
- [x] Infix arithmetic operators with precedence ✓
- [x] Not line-oriented (expressions can span lines)

#### Evaluation and Compilation (MANDATORY)

- [x] Compilation process documented (`compilation.rst`)
- [x] Virtual Machine design documented
- [x] Instruction set defined (flat, no recursion in functions)
- [x] Compiler architecture (12 phases)
- [x] Disassembly support planned
- [x] Bytecode format specified
- [x] Closures planned

#### Documentation (MANDATORY)

- [x] User manual (`user_manual.rst`)
- [x] Formal grammar (BNF) (`grammar.rst`)
- [x] Compilation process (`compilation.rst`)
- [x] Security review (`security_review.rst`)
- [x] Accessibility considerations
- [x] High-quality, comprehensive documentation

### 🚧 To Be Implemented

- [ ] **Phase 4 – Desugaring** — Lower Topineur AST to core IR (`ObjectDesugar`, `TraitResolver`, effect mapping).
- [ ] **Phase 5 – Code Generation** — Teach `CodeGen` new instructions and metadata for objects, traits, effects.
- [ ] **Phase 6 – VM** — Extend runtime with object layouts, trait dictionaries, actor primitives.
- [ ] **Phase 7 – Builtins** — Update signatures + implementations to align with Topineur types/effects.
- [ ] **Phase 9 – Test suite** — Comprehensive unit + integration coverage for parser, typer, backend, runtime.
- [ ] **Phase 10 – Standard library** — Core/IO/collections/actors modules authored in Topineur.
- [ ] **Examples** — Ensure all documented examples compile, type-check, and execute via the new pipeline.

## Quick Start (Current CLI)

```bash
# Build the compiler
make

# Inspect the frontend pipeline
./glados pipeline examples/topineur/hello_world.top

# Type-check a module (fails with non-zero exit code on issues)
./glados check examples/topineur/factorial_recursive.top

# Additional tooling lives under docs/topineur/ and vscode/topineur/
```

## Example Topineur Code

### Hello World

```topineur
def main(): !{IO} Unit =
  println("Hello, Topineur!")
```

### Objects and Methods

```topineur
object type Point {
  x: Float
  y: Float

  def translate(dx: Float, dy: Float): Point =
    Point { x = x + dx, y = y + dy }

  def distance(): Float =
    sqrt(x * x + y * y)
}

def main(): !{IO} Unit =
  let p = Point { x = 3.0, y = 4.0 }
  println("Distance: " ++ show(p.distance()))
```

### Traits

```topineur
trait Show {
  def show(): String
}

impl Show for Point {
  def show(): String =
    "Point(" ++ show(x) ++ ", " ++ show(y) ++ ")"
}

def printValue[T](value: T): !{IO} Unit where T: Show =
  println(value.show())
```

### Effects

```topineur
// Pure function — no effects
def factorial(n: Int): Int =
  if n <= 1 then 1
  else n * factorial(n - 1)

// Effectful function — explicit IO
def readConfig(): !{IO} Config =
  let contents = readFile("config.json")
  parseConfig(contents)
```

### Linear Types (Safe Mutation)

```topineur
object type File {
  handle: Int

  def read(): !{IO} String = ...
  def close(): !{IO} Unit = ...
}

def processFile(): !{IO} String =
  let file: !lin File = openFile("data.txt")  // Linear ownership
  let contents = file.read()
  file.close()  // file consumed here
  contents
```

### Actors (Concurrency)

```topineur
actor Counter {
  count: Int  // Private mutable state

  receive {
    case Increment => count = count + 1
    case GetCount(replyTo) => replyTo.send(count)
  }
}

def main(): !{IO, Async} Unit =
  let counter = spawn(Counter { count = 0 })
  counter.send(Increment)
  counter.send(Increment)
  let result = counter.ask(GetCount)
  println("Count: " ++ show(result))
```

## Next Steps

### Immediate (High Priority)

1. **Kick off Phase 4:** Finalize the desugaring design that maps Topineur AST nodes to the existing core IR now that the Lisp path has been removed.
2. **Stand up `ObjectDesugar`:** Build scaffolding plus pattern coverage for objects, traits, effect annotations, and linear bindings so Phase 4 has an executable entry point.
3. **Wire the pipeline:** Hook lexer/parser/type+effect+linearity passes into `Compiler.hs` end-to-end and add regression tests for `examples/topineur`.

### Short-term (Medium Priority)

4. **Extend backend entry points:** Update `CodeGen.hs` and `Builtins.hs` signatures so they accept the enriched AST artifacts emitted by the new pipeline.
5. **Phase 4 ➜ 5 bridge:** Define intermediate structures (`TraitResolver`, `ObjectLowering`) that feed the code generator with trait dictionaries and method tables.
6. **Parser/typechecker validation:** Grow `test/` with parser golden files and the first inference/effect snapshots to prevent regressions while Phase 4 evolves.

### Long-term (Lower Priority)

7. **VM evolution:** Introduce object layouts, actor mailbox primitives, and effect-aware instructions in `VM.hs`.
8. **Standard library build-out:** Populate `stdlib/` with core, IO, collections, and concurrency helpers once the backend executes real programs.
9. **Optimization & tooling polish:** Revisit ANF, closure conversion, and diagnostics once the full Topineur path is functional.

## Phase 4 Blueprint (Desugaring to Core IR)

### Objectives

- Translate Topineur constructs (objects, traits, effect annotations, linear bindings) into the existing core IR while embracing the new Topineur-only pipeline.
- Provide a dedicated desugaring module that exposes a clean API (`desugarTopModule :: AST.Module -> Core.Module`) consumed by `Compiler.hs`.
- Preserve and propagate metadata (source spans, inferred types/effects) so downstream passes can surface actionable diagnostics.

### Planned Deliverables

- `src/ObjectDesugar.hs`: structural lowering for object literals, method bodies, and pattern expansions.
- `src/TraitResolver.hs`: dictionary construction + monomorphization for trait implementations.
- `src/Desugar/Effects.hs` (or equivalent helpers): utilities to encode effect rows and linear consumption markers in the core form.
- `src/Desugar.hs`: Topineur-only context builder coordinating object/trait lowering helpers.

### Core IR Snapshot

- **Module structure:** `Module { moduleName, imports, declarations }` where declarations are functions, objects, or traits lowered to the core lambda calculus with explicit environment captures.
- **Function form:** Functions are represented as `CoreLambda params body` with ANF-ready bodies; every call site awaits desugaring to apply arguments positionally with pre-validated arity.
- **Data layout:** Objects become records (`CoreRecord`) with deterministic field slots; methods are emitted as standalone lambdas with the receiver passed explicitly as the first argument.
- **Trait resolution artifacts:** Trait impls materialize as dictionary records (`CoreDict`) bundled with method closures and metadata pointers used during code generation.
- **Effect encoding:** Effect rows collapse to an ordered list of capability tags carried on lambda metadata so backend checks can insert runtime guards or dispatch to effectful builtins.
- **Linearity markers:** Linear bindings are annotated via `CoreConsume` nodes that the optimizer must preserve to guarantee no double-consume scenarios reach the VM.

### Work Breakdown

1. Document the target core IR surface (constructors, expected invariants, ownership model) and add it to `docs/topineur/` for reference.
2. Implement expression-level lowering (functions, let/let* desugaring, match expansion) leveraging existing ANF prerequisites.
3. Encode method dispatch by generating vtable records and trait dictionary lookups with deterministic naming.
4. Introduce regression tests that run the desugaring pass on `examples/topineur/*.top` and snapshot the resulting core IR.
5. Validate integration by invoking the full pipeline on `hello_world.top`, ensuring bytecode emission stays stable.

### Risks & Mitigations

- **Complex trait interactions:** start with single-trait implementations, add multi-trait support once the pipeline is stable.
- **Linearity leaks during lowering:** reuse `LinearityChecker` outputs to assert consumption post-desugar.
- **Diagnostic drift:** keep source span threading intact and add failing test cases to guarantee error messages reference Topineur code.

## Resources

### Documentation

- **Main docs:** `docs/topineur/README.rst`
- **Grammar:** `docs/topineur/grammar.rst`
- **Security:** `docs/topineur/security_review.rst`
- **User manual:** `docs/topineur/user_manual.rst`
- **Compilation:** `docs/topineur/compilation.rst`
- **Core IR reference:** `docs/topineur/core_ir.rst`
- **Transformation plan:** `docs/topineur/transformation_plan.rst`

### Examples

- **Directory:** `examples/topineur/`
- **Files:** `hello_world.top`, `factorial_recursive.top`, etc.

### Tooling

- **VSCode extension:** `vscode/topineur/`
- **Tests:** `test/` (to be created)

## Timeline Estimate

**Conservative estimate for one developer:**

- **Documentation:** ✅ Complete (5-7 days)
- **Implementation:** 33-49 days (6-10 weeks)
  - Phases 1-3: ✅ 9-13 days already invested
  - Phases 4-6 (desugaring, codegen, VM): 14-20 days remaining
  - Phase 7 (builtins refresh): 2-3 days
  - Phase 9 (test suite): 4-6 days
  - Phase 10 (standard library): 3-5 days
  - Polish / performance / diagnostics: 6-8 days

**For defense preparation:**

Focus on:
1. ✅ Documentation (complete)
2. Parser + basic type checking (2-3 weeks)
3. Simple examples working (1 week)
4. Minimal standard library (3-5 days)

**Minimum viable for defense:** ~4-5 weeks

## Success Criteria

### Minimum (for passing)

- [x] Formal BNF grammar
- [x] Security review
- [x] User manual
- [x] Compilation process documented
- [x] Parser working for basic programs ✅
- [ ] Type checking for simple cases (integration pending)
- [ ] Basic VM support for objects
- [ ] At least 3 examples compiling and running

### Target (for good grade)

- [ ] Full type system (inference + effects + linearity) integrated into pipeline
- [ ] Complete standard library
- [ ] All 12 examples working
- [ ] Test coverage > 80%
- [ ] Performance benchmarks

### Stretch (for excellence)

- [ ] JIT compilation
- [ ] Full closure support
- [ ] Actor system fully working
- [ ] Rich standard library (100+ functions)
- [ ] IDE integration (language server)

## Questions and Answers

**Q: Why Topineur instead of sticking with Lisp?**
A: Part 2 requires a non-S-expression syntax, and Topineur adds modern features (effects, ownership) that make the language suitable for real-world use.

**Q: What happened to the Lisp toolchain?**
A: It has been removed from the main branch. Legacy sources live only in history; all active work targets Topineur.

**Q: How long will this take?**
A: 6-10 weeks for one developer to implement everything. 4-5 weeks for a minimal viable version for defense.

**Q: What if we run out of time?**
A: The documentation is complete, which satisfies many Part 2 requirements. Focus on getting the parser and basic type checking working, with at least 3 examples compiling.

**Q: Can we still run Lisp programs here?**
A: No. The CLI, examples, and documentation are now exclusively Topineur-focused.

## Conclusion

The documentation phase is **complete**. All mandatory documents for Part 2 are ready:

- ✅ Formal grammar (BNF)
- ✅ Security review
- ✅ User manual
- ✅ Compilation process

The implementation roadmap is clear and detailed. With the frontend stack complete, the immediate focus is Phase 4: desugaring and end-to-end pipeline wiring.

**Phase 4 planning is underway. Time to bring the backend to life!**

---

*Last updated: 2025-10-14 16:05 CEST*
*Status: **Phase 1-3 Complete** – AST ✅, Lexer ✅, Parser ✅, Type System ✅; Phase 4 planning 🚧*
*Next: Desugaring implementation (Phase 4) kickoff + pipeline wiring*

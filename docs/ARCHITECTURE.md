# Architecture Overview (Topineur)

The legacy Lisp toolchain has been retired. GLaDOS now centres on the Topineur language, following the phased pipeline described in `docs/topineur/transformation_plan.rst`. This document summarises how the current codebase is organised and where the remaining work fits.

## 1. Frontend Pipeline

```
.top source
    │
    ▼
Tokens (TopineurLexer)
    │
    ▼
AST (TopineurParser)
    │
    ▼
Typed AST + Effect Row + Linearity info
    │
    ▼
Desugared Topineur AST (Object/Trait scaffolding)
    │
    ▼
Core IR (planned) → Backend passes → VM
```

Current modules:

| Stage             | Module(s)                           | Notes |
|-------------------|--------------------------------------|-------|
| Lexing            | `src/TopineurLexer.hs`               | 120+ tokens, comments, linear annotations. |
| Parsing           | `src/TopineurParser.hs`              | Full grammar with precedence climbing, object/trait syntax. |
| Type inference    | `src/TypeChecker.hs`                 | Hindley-Milner inference with simplified polymorphism. |
| Effect analysis   | `src/EffectChecker.hs`               | Row-polymorphic effect rows; default environment for builtins. |
| Linearity checks  | `src/LinearityChecker.hs`            | Ownership tracking, single-consume enforcement. |
| Desugaring scaffold | `src/Desugar.hs`, `src/ObjectDesugar.hs`, `src/TraitResolver.hs` | Builds the context that will lower into the core IR. |
| Orchestration     | `src/TopineurPipeline.hs`, `src/Compiler.hs` | Unified entry points used by the CLI. |

## 2. Command-Line Interface

`app/Main.hs` keeps the CLI intentionally small while the backend stabilises:

- `glados pipeline <file.top>` — run the frontend and display parsed/desugared forms, inferred types, and effects.
- `glados check <file.top>` — run the same pipeline but exit cleanly on success/failure.

Future switches (code generation, bytecode dumps, etc.) will return once the backend is Topineur-aware.

## 3. Intermediate Representation

`docs/topineur/core_ir.rst` captures the invariants expected from the forthcoming lowering stages. The plan is to reuse the existing ANF/Closure/CodeGen/VM layers after they are rewritten to consume the Topineur core IR instead of Lisp-specific constructs.

Key upcoming tasks:

1. Implement `ObjectDesugar`/`TraitResolver` so object literals, methods, and trait dictionaries map cleanly to the core IR.
2. Rebuild closure conversion and code generation around Topineur effects and linearity guarantees.
3. Extend the VM (`src/VM.hs`) with object layouts, trait dictionaries, and actor primitives.

## 4. Repositories & Tooling

- Examples now live under `examples/topineur/`.
- Legacy test suites and Lisp examples were removed; automated tests will return after the backend migration.
- The VS Code extension (`vscode/topineur/`) provides syntax highlighting for `.top` files and will evolve into a richer IDE experience alongside the compiler.

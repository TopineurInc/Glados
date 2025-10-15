# Topineur Compiler (GLaDOS)

GLaDOS has transitioned from a Lisp experiment into the official compiler toolchain for **Topineur**, an object-functional contractual language that blends Hindley-Milner typing, explicit effects, and linear ownership.

## What’s Included
- **Frontend pipeline**: lexer → parser → type inference → effect & linearity analysis → desugaring scaffold.
- **Command-line tooling**: `glados pipeline <file.top>` to inspect compilation stages, `glados check <file.top>` for fast verification.
- **Documentation set**: philosophy, grammar, compilation roadmap, and security review under `docs/topineur/`.
- **VS Code extension**: syntax highlighting and snippets in `vscode/topineur/`.
- **Examples**: runnable `.top` programs in `examples/topineur/`.

## Quick Start
```bash
# Build the project
make

# Inspect the pipeline for a Topineur file
./glados pipeline examples/topineur/hello_world.top

# Type-check a module (non-zero exit code on failure)
./glados check examples/topineur/factorial_recursive.top
```

## Project Layout
```
.
├── app/                 -- CLI entrypoint focused on Topineur
├── docs/topineur/       -- Language reference, grammar, compilation plan
├── examples/topineur/   -- Sample Topineur programs
├── src/                 -- Frontend passes (lexer, parser, type/effect/linearity, desugaring)
├── vscode/topineur/     -- VS Code extension
└── Makefile             -- Convenience wrapper around stack
```

Core modules:
- `src/TopineurLexer.hs`, `src/TopineurParser.hs` — surface syntax.
- `src/TypeChecker.hs`, `src/EffectChecker.hs`, `src/LinearityChecker.hs` — verification passes.
- `src/TopineurPipeline.hs` — orchestrates the frontend.
- `src/Desugar.hs`, `src/ObjectDesugar.hs`, `src/TraitResolver.hs` — desugaring scaffolding for upcoming backend integration.

## Status & Next Steps
- ✅ Frontend passes complete and exposed through the CLI.
- 🚧 Backend lowering, code generation, and VM integration are being redesigned around the Topineur core IR (`docs/topineur/core_ir.rst`).
- 🔬 Automated tests will return once the new backend stabilises.

## License
Released under the MIT License (see `LICENSE`).

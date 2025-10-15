# Core IR & Runtime Roadmap

The bytecode and VM reference that previously described the Lisp runtime no longer applies. As the Topineur migration continues, this document will track the reborn backend stack once the new core IR lands.

## Current Status
- The frontend produces typed, effect-annotated, and linearity-checked ASTs.
- Desugaring scaffolding (`src/Desugar.hs`, `src/ObjectDesugar.hs`, `src/TraitResolver.hs`) is in place to feed a forthcoming core IR.
- Backend modules (`CodeGen`, `VM`, `Builtins`) are being redesigned. Their Lisp-specific behaviour should be considered legacy until the new IR is wired through.

## Planned Sections
1. Core IR constructors and invariants (mirroring `docs/topineur/core_ir.rst`).
2. Bytecode instruction set adapted for objects, traits, and actors.
3. VM runtime model with effect-aware execution and linear resource tracking.
4. Builtin catalogue in the Topineur ecosystem.

Updates will follow once the desugaring phase emits the canonical core IR.

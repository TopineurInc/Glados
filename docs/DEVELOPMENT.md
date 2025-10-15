# Developer Notes (Topineur Edition)

This guide reflects the post-migration workflow. All legacy Lisp instructions have been removed; every command below assumes Topineur sources (`*.top`).

## Tooling
- **Stack** is still the canonical build tool (wrapped by the `Makefile`).
- **GHC** is pinned via `stack.yaml`; stack bootstraps the correct compiler automatically.
- **Python tooling** is currently unused — the old regression suite was retired alongside the Lisp frontend.

## Build Commands

| Task | Command | Notes |
|------|---------|-------|
| Build binary | `make` | Produces `./glados` (Topineur CLI). |
| Fast rebuild | `stack build --fast` | Useful during frontend work. |
| Clean | `make clean` | Removes the binary and runs `stack clean`. |
| Full clean | `make fclean` | Drops `.stack-work/` and rebuilds dependencies. |

## CLI Usage
```bash
# Show detailed pipeline output
./glados pipeline examples/topineur/hello_world.top

# Type-check only
./glados check examples/topineur/factorial_recursive.top
```

Backend-oriented commands (bytecode dump, VM execution, etc.) are temporarily unavailable while the new core IR is wired up.

## Working on the Frontend
1. **Lexer/Parser** lives in `src/TopineurLexer.hs` and `src/TopineurParser.hs`.
2. **Types & Effects** are handled by `src/TypeChecker.hs` and `src/EffectChecker.hs`.
3. **Linearity** is enforced via `src/LinearityChecker.hs`.
4. **Desugaring scaffolding** resides in `src/Desugar.hs`, `src/ObjectDesugar.hs`, and `src/TraitResolver.hs`.
5. **Pipeline orchestration** is wrapped by `src/TopineurPipeline.hs` and re-exported through `src/Compiler.hs`.

When modifying the pipeline, run `./glados pipeline <file.top>` to confirm each stage still behaves as expected.

## Tests
Automated test suites were removed during the migration. They will return once the backend is integrated with the Topineur core IR. Until then, rely on manual runs of the pipeline over the examples in `examples/topineur/`.

## Style Guidelines
- Keep modules self-contained and pure where possible.
- Follow the existing comment style (short, functional descriptions above complex sections).
- Use `TopineurPipeline.compileTopineurModule` in new tooling instead of the old Lisp-era helpers.
- Update `docs/topineur/` whenever you add language features or adjust the IR contract.

## Future Work Checklist
1. Implement the desugaring bridge into the documented core IR.
2. Re-enable code generation and VM support for objects, traits, and actors.
3. Restore automated tests (unit + integration) targeting `.top` programs.
4. Expand the VS Code extension into a full language companion (diagnostics, code actions).

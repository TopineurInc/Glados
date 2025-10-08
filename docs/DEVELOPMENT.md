# Developer Notes

This document collects the practical information we keep forgetting: how to build the project, run tests, and extend the compiler/VM without breaking things. Treat it as the onboarding cheat-sheet.

---

## 1. Tooling

- **Stack** is the canonical build tool. The `Makefile` wraps it for convenience.
- **GHC** version is pinned via `stack.yaml`. No need to install it globally; stack downloads the right compiler.
- **Python 3** is required for the regression suite in `tests/`.
- On macOS/Linux the provided scripts work out of the box. Windows support is untested.

---

## 2. Build commands

| Task | Command | Notes |
|------|---------|-------|
| Build binary | `make` | Produces `./glados` at the repo root (moves stack’s `glados-exe`). |
| Build with stack | `stack build` | Leaves the executable under `.stack-work`. |
| Clean artifacts | `make clean` | Deletes `./glados` and runs `stack clean`. |
| Full clean | `make fclean` | Nukes `.stack-work/`, `dist`, and the binary. |
| Rebuild from scratch | `make re` | Full clean + build. |

CLI usage:
```bash
./glados program.lisp          # compile + run
./glados --disasm program.lisp # just dump bytecode
```

---

## 3. Tests

### Python regression suite

```bash
python3 tests/tester.py            # default run (36 test cases)
python3 tests/tester_detailed.py   # prints inputs/outputs per case
```

The suite feeds Lisp programs from `tests/assets/` to the compiler and checks stdout/stderr. When adding a feature, include a new asset + entry in the JSON manifest if necessary.

### Haskell tests

See the `test/` directory for HSpec/Tasty style tests. Run them with:

```bash
stack test
```

The coverage build (`make tests_run`) writes `.tix` files under `test/coverage/`.

---

## 4. Debugging workflow

1. **Rebuild quickly**: `stack build --fast` is handy during development.
2. **Inspect bytecode**: `./glados --disasm file.lisp` gives you constants, instructions, and tail-call annotations.
3. **Trace passes**: Use `Debug.Trace` sparingly (remove it before committing). Good spots: after macro expansion, after desugaring, before code generation.
4. **VM tracing**: Temporarily log inside `executeInstr` to follow the stack and program counter.
5. **Shrinking failures**: If a test fails, reduce the Lisp program manually until the bug is obvious—macros often hide the minimal repro.

---

## 5. Adding features

| Goal | Where to look |
|------|---------------|
| New syntax | Parser (`SExprParser.hs`) + desugarer (`Desugar.hs`). |
| New macro | Add to `defaultMacroEnv` in `MacroExpander.hs`. |
| New builtin | `Builtins.hs` + `builtinArity` in `VM.hs`. |
| New instruction | Update `Instr` (in `AST.hs`), teach `CodeGen.hs`, and handle it in `VM.executeInstr`. |
| Optimisation | Mostly in `CodeGen.hs`; make sure tests still pass. |
| Error messages | Carry `SourcePos` information so the CLI can print nice diagnostics. |

When touching multiple passes, work top-down: add parsing support, adjust desugaring, compile, and only then touch codegen/VM.

---

## 6. Style & conventions

- Haskell files already include the EPITECH headers—keep them.
- Prefer pure functions; defer IO to the edges (CLI, builtins, VM printing).
- Name fresh bindings with the `foo#0` convention when generating names (see `AlphaRename.hs`).
- For documentation comments, keep them short and focused (one sentence explaining the unusual bits).
- Tests go under `tests/` (Python) or `test/` (Haskell). Mirror existing patterns.

---

## 7. Common pitfalls

- Forgetting to register a new builtin in `builtinArity` results in `StackUnderflow`.
- Not updating `coMaxLocals` when changing codegen leads to uninitialised local errors.
- Macro errors bubble up as `SyntaxError`. Make sure to include helpful messages (`when` and `unless` are good templates).
- Tail-call optimisation depends on the last expression being a function application. If you wrap it in `begin` or extra layers, the optimiser won’t trigger.

---

## 8. Release checklist

1. `make` (clean build).
2. `python3 tests/tester.py`.
3. `stack test` (optional but recommended).
4. `./glados --disasm examples/factorial_test.lisp` to ensure disassembly works.
5. Update `CHANGELOG.md` with user-visible changes.

That’s it. If you discover new tricks or recurring issues, append them here so future-you doesn’t have to rediscover the fix.

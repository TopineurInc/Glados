# Architecture

This document explains how GLaDOS is organised: the compiler pipeline, the data that flows through it, and how the virtual machine runs the result. You should be able to read this and immediately know where to add a new feature or debug a regression.

---

## 1. Bird's-eye view

```
.lisp source
    │
    ▼
SExpr tree (parser)
    │
    ▼
Core Expr AST (macros + desugar)
    │
    ▼
ANF & closure info (alpha/closure)
    │
    ▼
Bytecode (codegen)
    │
    ▼
Stack VM (runtime)
```

1. `SExprParser.hs` reads text and produces annotated S-expressions.
2. `MacroExpander.hs`, `Desugar.hs`, `AlphaRename.hs`, and `ClosureConversion.hs` progressively rewrite the program to a lower-level, closure-friendly representation.
3. `CodeGen.hs` emits bytecode instructions wrapped in `CodeObject`s.
4. `VM.hs` executes those code objects on a stack machine that knows about the builtins defined in `Builtins.hs`.

The orchestration of these stages lives in `Compiler.hs`; the CLI (in `app/`) mainly exposes flags and I/O.

---

## 2. Compiler pipeline

### 2.1 Parsing (`SExprParser.hs`)
- Uses Megaparsec to recognise integers, booleans (`#t`, `#f`), strings, and symbols.
- Keeps `SourcePos` information on every node (`SAtom`/`SList`) so later errors can point to the original line/column.
- Outputs `SExpr`, which mirrors the textual structure one-to-one.

### 2.2 Macro expansion (`MacroExpander.hs`)
- A `MacroEnv` maps symbols to expander functions (`SExpr -> Either CompileError SExpr`).
- Default macros: `when`, `unless`, `cond`. They expand into plain core forms (`if`, booleans, nested lists).
- Expansion is recursive: we keep expanding until the tree stops changing, then we dive inside lists to expand their children.
- Adding a new macro means inserting it into `defaultMacroEnv`.

### 2.3 Desugaring (`Desugar.hs`)
- Converts surface constructs to the core `Expr` AST.
- Examples:
  - `(define (f x) body)` → `(define f (lambda (x) body))`
  - `(let ((x v)) body)` → `((lambda (x) body) v)`
  - `(quote (1 2 3))` → `EQuote` over the original S-expression.
- After this pass, only a small set of forms remains: `EVar`, `ELambda`, `EDefine`, `EIf`, `EApp`, `EQuote`, literals, and lists.

### 2.4 Alpha-renaming (`AlphaRename.hs`)
- Generates fresh names (e.g. `loop#0`, `loop#1`) for every binding, avoiding accidental capture later.
- Rewrites `letrec` blocks so mutually recursive functions get consistent treatment.
- The output preserves program structure but guarantees uniqueness for all identifiers.

### 2.5 Closure conversion (`ClosureConversion.hs`)
- Computes free variables for each lambda.
- Rewrites lambdas so captured variables become explicit environment slots.
- Produces an admin-normal-form (ANF) intermediary where each function body is a sequence of simple instructions.
- At this point, all closures are first-class values that can be stored or returned safely.

### 2.6 Code generation (`CodeGen.hs`)
- Walks the closure-converted representation and emits `CodeObject`s.
- Builds constant pools, records label positions, and writes `Instr` sequences (`IConst`, `ILoad`, `ICall`, etc.).
- Detects tail calls and emits `ITailCall` when the callee result is immediately returned.
- Returns the main code object plus a map of nested definitions (`generateCodeWithDefs`).

### 2.7 Wrapper API (`Compiler.hs`)
- `compile` compiles a whole program and returns a single `CodeObject`.
- `compileWithDefs` exposes all compiled functions (used by the VM loader).
- `compileProgram` compiles a list of top-level `define`s and returns a `Map Name CodeObject`.
- `transformProgram` wraps top-level `define`s in an implicit `letrec` so mutually recursive functions are handled without special cases in later passes.

---

## 3. Data structures

| Stage              | Type (from `AST.hs`) | Purpose |
|--------------------|----------------------|---------|
| Surface syntax     | `SExpr`, `Atom`      | Faithful representation of the parsed file, with source positions. |
| Core AST           | `Expr`               | After macros + desugaring; the compiler works exclusively on this. |
| Normal form        | `ANF`                | Used by closure conversion and codegen to sequence operations. |
| Constants          | `Constant`           | Ints, bools, strings, and function references stored in code objects. |
| Bytecode           | `Instr`              | Operations executed by the VM (`IConst`, `ICall`, `IMakeClosure`, …). |
| Executable unit    | `CodeObject`         | Metadata + constants + instructions for a single function. |
| Runtime values     | `Value`              | Integers, booleans, strings, closures, and builtins. |

`CodeObject` summarises everything the VM needs: a unique name, arity, the number of locals, constant pool, instruction vector, and a label map for jumps.

---

## 4. Virtual machine

### 4.1 Global state
- `VMState` keeps:
  - `vFrames`: call stack (`[Frame]`), head is the current frame.
  - `vGlobals`: mutable map for top-level definitions (currently unused but ready for REPL mode).
  - `vCodeObjects`: compiled functions keyed by name.
  - `vBuiltins`: builtin table coming from `Builtins.hs`.
- `initVMState` seeds the runtime with the builtin map and an empty stack.

### 4.2 Frame layout
- `Frame` contains:
  - `fLocals`: vector of `Maybe Value` (size `coMaxLocals`).
  - `fStack`: operand stack (list, head = top).
  - `fCode`: code object being executed.
  - `fPC`: program counter.
- On entry, formal parameters are written into the first slots of `fLocals`.
- The VM stores locals by index, so alpha-renaming is required to avoid name clashes.

### 4.3 Instruction execution (`VM.executeInstr`)
- Fetch-decode-execute loop:
  1. Read `instr = coInstrs ! fPC`.
  2. Execute it and produce an updated VM state (and optionally a return value).
  3. Either continue (tail-recursive call) or return when the stack becomes empty.
- Stack discipline: left-to-right argument order, but lists store the newest value at the head.
- Jumps rewrite the `fPC`. Conditional jumps (`IJumpIfFalse`) also pop their condition.

### 4.4 Calls and returns
- `ICall`:
  1. Pop arguments from the caller stack.
  2. If the callee is a builtin: run the Haskell function and push the result.
  3. If it is user code: allocate a new frame, seed locals, push it on the frame stack.
- `ITailCall` reuses the caller frame slot instead of pushing a new one.
- `IReturn` pops the current frame, pushes the return value onto the caller stack, and resumes execution there.

### 4.5 Closures
- `IMakeClosure name [slots]` captures the listed local slots into a `VClosure`.
- `ILoadClosure`/`IStoreClosure` access the captured environment.
- At runtime, closures are stored alongside compiled code in `vCodeObjects`; the `name` acts as the link between both worlds.

### 4.6 Builtins
- Implemented in `Builtins.hs` as functions `[Value] -> IO Value`.
- Registered in the `builtins :: Map Name Value` map (`VBuiltin name func`).
- `VM.executePrim` looks them up and handles arity/argument extraction before calling into Haskell.
- Provided operations include arithmetic, comparisons, boolean logic, string helpers, I/O (`print`, `display`, `read-line`, `input`), and formatting (`format`).

---

## 5. Module map

| Module                  | Responsibility |
|-------------------------|----------------|
| `AST.hs`                | Definitions for all shared types (AST nodes, bytecode, runtime values). |
| `SExprParser.hs`        | Parser entry points (`parseFromString`, `parseMultipleSExprs`). |
| `MacroExpander.hs`      | Macro environment and recursive expansion logic. |
| `Desugar.hs`            | Turns surface syntax into the core expression language. |
| `AlphaRename.hs`        | Fresh name generation and `letrec` rewriting. |
| `ClosureConversion.hs`  | Free variable analysis and closure rewriting. |
| `CodeGen.hs`            | Emits bytecode, manages constant pools and labels. |
| `Compiler.hs`           | Glue code wiring all passes together. |
| `VM.hs`                 | Runtime loop, instruction semantics, call-stack management. |
| `Builtins.hs`           | Primitive library exposed to user programs. |
| `Disasm.hs`             | Bytecode disassembler used by `--disasm`. |

---

## 6. Extensibility notes

- **Adding syntax**: update `SExprParser` if it introduces new tokens, then desugar it in `Desugar.hs`.
- **New macro**: extend `defaultMacroEnv` with an expander. Keep error messages descriptive; they bubble up to the CLI.
- **New builtin**: implement a Haskell function, insert it in the `builtins` map, and update `builtinArity` in `VM.hs`.
- **New instruction**: add the constructor to `Instr` (`AST.hs`), teach `CodeGen` how to emit it, and handle it in `VM.executeInstr`.
- **Optimisations**: `CodeGen.hs` is the best place for peephole optimisations; the VM intentionally stays dumb.

---

## 7. Debugging checklist

- `./glados --disasm program.lisp` dumps the generated `CodeObject`s, constants, and instructions.
- The Python regression suite (`python3 tests/tester.py`) is quick to run and covers 36 canonical examples.
- For macro bugs, sprinkle `traceShow` in `MacroExpander`; for codegen issues, dump the ANF just before emission.
- A `RuntimeError "PC out of bounds"` usually means a missing label patch; inspect `coLabelMap`.

---

## 8. Error surfaces

- Compile-time: every stage returns `Either CompileError`. Preserve the `SourcePos` if you create new errors.
- Runtime: `runVM` yields `Either VMError Value`. Typical failures are `StackUnderflow`, `UndefinedFunction`, or custom `TypeError`s from builtins.

Armed with this map, you should be able to dive into any pass or runtime component without getting lost. Cross-reference the language guide for the syntax the compiler expects.

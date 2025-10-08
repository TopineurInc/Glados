# Bytecode & Runtime Reference

This document is the reference for the VM instruction set, the constant pool, and the calling convention used by GLaDOS. It complements the architecture overview with the low-level details you need when working on the backend or debugging generated code.

---

## 1. Code objects at a glance

Every function is compiled into a `CodeObject` (`AST.hs`):

| Field           | Meaning |
|-----------------|---------|
| `coName`        | Unique identifier (e.g. `main`, `fact#0`). |
| `coArity`       | Number of positional arguments expected. |
| `coMaxLocals`   | Size of the local slot array (including arguments). |
| `coConsts`      | Vector of `Constant` (ints, bools, strings, function refs). |
| `coInstrs`      | Vector of `Instr` (bytecode instructions). |
| `coLabelMap`    | Mapping from label names to instruction indices (for jumps). |

Constants are referenced by index (`IConst 0` pulls `coConsts ! 0`). Function references appear as `CFuncRef name` so code can refer to other compiled objects.

---

## 2. Runtime conventions

- **Operand stack**: Implemented as a Haskell list. The newest value sits at the head. Stack diagrams below follow the `[top, …, bottom]` convention.
- **Locals**: Stored in a `Vector (Maybe Value)` inside the frame. Slots `0 .. coArity - 1` contain the arguments when the frame is created.
- **Arguments**: The caller pushes arguments left-to-right, so the callee reads them in-order after the VM reverses them.
- **Booleans**: Lisp booleans are represented as `VBool` (`#t`, `#f`). The VM treats `#f` as false and everything else as true.
- **Nil**: We reuse `VBool False` for `nil` in the builtin map (mirroring Common Lisp tradition).

---

## 3. Instruction set

### Legend
- **Stack effect** column shows what the stack looks like before and after the instruction.
- `[...]` represents the rest of the stack.

### 3.1 Stack and locals

| Instruction | Stack effect | Description |
|-------------|--------------|-------------|
| `IConst idx` | `[...] → [const, ...]` | Pushes `coConsts[idx]`. `CFuncRef` resolves to a builtin if present. |
| `ILoad slot` | `[...] → [value, ...]` | Pushes the value stored at `locals[slot]`. Errors on uninitialised slots. |
| `IStore slot` | `[value, ...] → [...]` | Pops and writes to `locals[slot]`. |
| `IPop` | `[value, ...] → [...]` | Drops the top value (used to discard intermediate results). |
| `INop` | `[...] → [...]` | Placeholder; advances the PC by one. |

### 3.2 Control flow

| Instruction | Stack effect | Description |
|-------------|--------------|-------------|
| `IJump target` | `[...] → [...]` | Sets `fPC = target`. |
| `IJumpIfFalse target` | `[cond, ...] → [...]` | Pops `cond`; jumps if it is exactly `VBool False`. |
| `IReturn` | `[value, ...] → (return value)` | Pops the current frame, pushes `value` on the caller stack, resumes there. Returning with an empty stack is an error. |

### 3.3 Function calls

| Instruction | Stack effect | Description |
|-------------|--------------|-------------|
| `ICall arity name` | `[..., arg₀, …, argₙ₋₁] → [...]` | Pops `arity` arguments, looks up `name` in builtins or compiled code, and either executes it (builtin) or pushes a new frame. |
| `ITailCall arity name` | `[..., arg₀, …, argₙ₋₁] → [...]` | Same as `ICall`, but reuses the current frame slot to avoid growing the call stack (tail-call optimisation). |
| `IPrim op` | `[..., arg₀, …] → [result, ...]` | Runs a builtin operation directly. `builtinArity` in `VM.hs` determines how many values to pop. Mainly used for primitive arithmetic/logic. |

### 3.4 Closures

| Instruction | Stack effect | Description |
|-------------|--------------|-------------|
| `IMakeClosure name slots` | `[...] → [closure, ...]` | Captures `locals[slot]` for each index in `slots`, wraps them in `VClosure`. Assumes the code object `name` exists. |
| `ILoadClosure idx` | `[...] → [value, ...]` | Reads the `idx`-th captured value from the current closure environment. |
| `IStoreClosure idx` | `[value, ...] → [...]` | Pops and writes into the captured environment. Rarely emitted (mutating closures). |

---

## 4. Builtin operations

`IPrim` and `ICall` both interact with the builtin table defined in `Builtins.hs`. The following identifiers are registered out of the box:

- Arithmetic: `+`, `-`, `*`, `div`, `mod`
- Comparison: `eq?`, `<`, `>`
- Boolean: `not`, `and`, `or`
- I/O: `print`, `display`, `input`, `read-line`, `format`
- Strings: `string->number`, `number->string`, `string-length`, `string-append`, `substring`
- Constants: `t`, `nil`

Each builtin runs in IO and returns a `Value`. Errors are raised with descriptive messages (e.g. `"Type error: + expects two integers"`). When you add a builtin, remember to update `builtinArity` in `VM.hs` so the VM pops the correct number of arguments.

---

## 5. Calling sequence walkthrough

Example: `(fact 5)` compiled to bytecode.

1. **Caller pushes arguments**
   ```
   [...]                   -- initial stack
   IConst 0  ; 5           -- stack: [5]
   ICall 1 fact#0
   ```
2. **VM handles `ICall`**
   - Pops `[5]`, reverses -> `[5]`.
   - Creates a new frame for `fact#0`, writes `locals[0] = 5`.
   - Pushes the frame: `frames = [fact#0, caller]`.
3. **Callee runs**
   - Loads `n` from slot 0 (`ILoad 0`).
   - Compares with zero, branches, etc.
4. **Return**
   - `IReturn` pops `fact#0` frame, pushes the result onto caller stack.
   - Caller continues at `fPC + 1`.

Tail-recursive calls compile to `ITailCall`, which skips the “push new frame” step and directly rewrites the current frame.

---

## 6. Constant pool format

`Constant` is defined in `AST.hs`:

```haskell
data Constant
  = CInt Integer
  | CBool Bool
  | CString String
  | CFuncRef Name
```

- `CInt`, `CBool`, `CString` map directly to `VInt`, `VBool`, and `VString`.
- `CFuncRef` is special-cased in the VM: it first checks `vBuiltins` (so `"t"` and `"nil"` resolve to booleans) and falls back to wrapping it in a stub builtin (`VBuiltin`) if nothing is found yet.

---

## 7. Tips for debugging bytecode

- Use `./glados --disasm program.lisp` to inspect constants, instructions, and jump targets.
- When an `IConst` crashes with “constant index out of bounds”, re-check `coConsts` and ensure the index matches.
- `StackUnderflow` usually means `builtinArity` is wrong or codegen emitted the wrong arity in `ICall`.
- To trace execution, temporarily add `traceShow (instr, frame)` inside `executeInstr` in `VM.hs` (don’t forget to import `Debug.Trace` locally while debugging).

That’s the contract between the compiler and the VM. Keep this nearby when you tweak code generation or introduce new primitives—the details here are what keep everything aligned.

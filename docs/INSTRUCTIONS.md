# GLaDOS Instruction Set Reference

## Overview

GLaDOS uses a stack-based bytecode instruction set. Each function is compiled to a `CodeObject` containing instructions, constants, and metadata.

## CodeObject Structure

```haskell
CodeObject {
  coName      :: String        -- Function name
  coArity     :: Int          -- Number of parameters
  coMaxLocals :: Int          -- Local variable slots needed
  coConsts    :: [Constant]   -- Constant pool
  coInstrs    :: [Instr]      -- Bytecode instructions
  coLabelMap  :: Map Int Int  -- Jump target resolution
}
```

## Instruction Format

Instructions are represented as algebraic data types:

```haskell
data Instr
  = IConst Int              -- Push constant from pool
  | ILoad Int               -- Load local variable
  | IStore Int              -- Store to local variable
  | IPrim String            -- Primitive operation
  | ICall Int Name          -- Function call
  | ITailCall Int Name      -- Tail call
  | IReturn                 -- Return from function
  | IJump Int               -- Unconditional jump
  | IJumpIfFalse Int        -- Conditional jump
  | IPop                    -- Pop stack top
  | INop                    -- No operation
  | IMakeClosure Name [Int] -- Create closure
  | ILoadClosure Int        -- Load closure variable
  | IStoreClosure Int       -- Store closure variable
```

## Instructions

### Stack Operations

#### `IConst idx`
Push constant from constant pool onto stack.

**Stack:** `[...] → [..., value]`

**Example:**
```
Constants: [42, "hello", true]
IConst 0  ; Pushes 42
```

#### `IPop`
Discard the top value from stack.

**Stack:** `[..., value] → [...]`

**Usage:** Discard intermediate results in sequences.

### Variable Access

#### `ILoad slot`
Load value from local variable slot and push to stack.

**Stack:** `[...] → [..., value]`

**Example:**
```
ILoad 0  ; Load first parameter/local
```

#### `IStore slot`
Pop value from stack and store in local variable slot.

**Stack:** `[..., value] → [...]`

**Example:**
```
IStore 2  ; Store to local slot 2
```

### Arithmetic & Logic

#### `IPrim operation`
Execute primitive operation. Pops operands, pushes result.

**Binary operations** (2 args):
- `+`, `-`, `*`, `div`, `mod` - Arithmetic
- `<`, `>`, `eq?` - Comparisons

**Stack:** `[..., arg1, arg2] → [..., result]`

**Examples:**
```
IConst 0  ; Push 5
IConst 1  ; Push 3
IPrim "+" ; 5 + 3 = 8
```

**Unary operations** (1 arg):
- `print` - Output value

**Stack:** `[..., value] → [..., value]`

### Control Flow

#### `IJump target`
Unconditional jump to instruction at `target`.

**Stack:** `[...] → [...]` (unchanged)

**Example:**
```
0: IConst 0
1: IJump 5
2: IConst 1   ; Skipped
3: IConst 2   ; Skipped
4: IConst 3   ; Skipped
5: IReturn    ; Jump here
```

#### `IJumpIfFalse target`
Pop value; jump to `target` if value is `false`.

**Stack:** `[..., bool] → [...]`

**Example:**
```
0: ILoad 0         ; Load condition
1: IJumpIfFalse 4  ; Jump to else branch
2: IConst 0        ; Then branch
3: IJump 5         ; Skip else
4: IConst 1        ; Else branch
5: IReturn
```

### Function Calls

#### `ICall arity name`
Call function with `arity` arguments.

**Stack:** `[..., arg1, ..., argN] → [..., result]`

**Process:**
1. Pop N arguments from stack
2. Look up function by name
3. Create new call frame with arguments as locals
4. Execute function
5. Push return value to caller's stack

**Example:**
```
; Call (fact 5)
IConst 0      ; Push 5
ICall 1 "fact#0"  ; Call fact with 1 arg
```

#### `ITailCall arity name`
Tail call optimization - replace current frame instead of creating new one.

**Stack:** `[..., arg1, ..., argN] → [..., result]`

**Benefit:** Prevents stack growth in recursive calls.

#### `IReturn`
Return from function with top of stack as return value.

**Stack:** `[..., value] → [...]` (pops from current frame, pushes to caller)

**Process:**
1. Pop return value from current frame
2. Remove current frame from call stack
3. Push return value to caller's stack
4. Increment caller's PC

### Closure Operations

#### `IMakeClosure name slots`
Create closure capturing specified local slots.

**Stack:** `[...] → [..., closure]`

**Example:**
```
IMakeClosure "inner#0" [0, 1]  ; Capture locals 0 and 1
```

#### `ILoadClosure idx`
Load captured variable from closure environment.

**Stack:** `[...] → [..., value]`

#### `IStoreClosure idx`
Store to captured variable in closure environment.

**Stack:** `[..., value] → [...]`

### Utility

#### `INop`
No operation. Used for alignment or as placeholder.

**Stack:** `[...] → [...]`

## Constant Pool

Constants referenced by `IConst` instruction:

```haskell
data Constant
  = CInt Integer
  | CBool Bool
  | CString String
  | CFuncRef Name  -- Reference to function
```

## Example Compilation

### Source
```lisp
(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))
```

### Bytecode
```
CodeObject: fact#0
  Arity: 1
  MaxLocals: 1
  Constants:
    0: INT 0
    1: INT 1

  Instructions:
       0: LOAD 0          ; Load n
       1: CONST 0         ; Push 0
       2: PRIM eq?        ; n == 0?
       3: JUMP_IF_FALSE 6 ; If false, go to else
       4: CONST 1         ; Push 1 (base case)
       5: JUMP 12         ; Skip else branch
       6: LOAD 0          ; Load n
       7: LOAD 0          ; Load n again
       8: CONST 1         ; Push 1
       9: PRIM -          ; n - 1
      10: CALL 1 fact#0   ; Recursive call
      11: PRIM *          ; n * fact(n-1)
      12: RETURN
```

## VM Execution Model

### Frame Structure
```haskell
Frame {
  fLocals :: [Maybe Value]  -- Local variables (indexed)
  fStack  :: [Value]        -- Operand stack
  fCode   :: CodeObject     -- Current function
  fPC     :: Int            -- Program counter
}
```

### Execution Loop
```
1. Fetch instruction at PC
2. Execute instruction
3. Update stack/locals/PC
4. If RETURN, pop frame and continue caller
5. Repeat until no frames remain
```

### Stack Convention
- **List head = stack top**: `[newest, ..., oldest]`
- Arguments pushed left-to-right
- Callee pops arguments during CALL
- Return value pushed to caller's stack

## Debugging

Use the disassembler to view compiled bytecode:

```bash
./glados --disasm program.lisp
```

Output shows:
- Function name and arity
- Constant pool contents
- Instructions with addresses
- Jump targets resolved

## Performance Notes

- `IConst` is O(1) - direct vector access
- `ILoad/IStore` are O(1) - indexed array access
- `ICall` overhead is O(k) where k = argument count
- `IJump` is O(1) - direct PC update
- Tail calls prevent O(n) stack growth in recursion

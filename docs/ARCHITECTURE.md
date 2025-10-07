# GLaDOS Architecture

## Overview

GLaDOS is a complete Lisp compiler and virtual machine with a multi-pass compilation pipeline and stack-based VM.

## Compilation Pipeline

```
Source Code (S-expressions)
    ↓
[1] Parse → SExpr
    ↓
[2] Macro Expansion
    ↓
[3] Desugar → AST (Expr)
    ↓
[4] Alpha Rename (variable uniqueness)
    ↓
[5] Closure Conversion
    ↓
[6] Code Generation → CodeObject
    ↓
[7] VM Execution
```

## Data Structures

### SExpr (Surface Syntax)
```haskell
data SExpr
  = SAtom Atom (Maybe SourcePos)
  | SList [SExpr] (Maybe SourcePos)

data Atom
  = AInteger Integer
  | ABool Bool
  | AString String
  | ASymbol String
```

### Expr (AST)
```haskell
data Expr
  = EInt Integer
  | EBool Bool
  | EString String
  | EVar Name
  | ELambda [Name] Expr
  | EDefine Name Expr
  | EIf Expr Expr Expr
  | EApp Expr [Expr]
  | EQuote SExpr
  | EList [Expr]
```

### CodeObject (Bytecode)
```haskell
data CodeObject = CodeObject
  { coName      :: Name
  , coArity     :: Int
  , coMaxLocals :: Int
  , coConsts    :: Vector Constant
  , coInstrs    :: Vector Instr
  , coLabelMap  :: Map Label Int
  }
```

### VM State
```haskell
data VMState = VMState
  { vFrames      :: [Frame]
  , vGlobals     :: Map Name Value
  , vCodeObjects :: Map Name CodeObject
  , vBuiltins    :: Map Name Value
  }

data Frame = Frame
  { fLocals :: Vector (Maybe Value)
  , fStack  :: [Value]
  , fCode   :: CodeObject
  , fPC     :: Int
  }
```

## Compiler Passes

### 1. Parser (`SExprParser.hs`)
- Converts text to S-expressions
- Tracks source positions for error reporting
- Handles atoms, lists, and nested structures

### 2. Macro Expander (`MacroExpander.hs`)
- Expands syntactic macros
- Built-in macros: `when`, `unless`, `cond`
- User-defined macros support

### 3. Desugarer (`Desugar.hs`)
Transforms high-level constructs to core forms:
- `(let ((x v)) body)` → `((lambda (x) body) v)`
- `(letrec ((f λ)) body)` → Sequential defines in begin block
- `(define (f x) body)` → `(define f (lambda (x) body))`

### 4. Alpha Renamer (`AlphaRename.hs`)
- Makes all variable names unique
- Prevents shadowing issues
- Uses gensym for fresh names: `fact` → `fact#0`
- Handles letrec semantics for mutually recursive definitions

### 5. Closure Converter (`ClosureConversion.hs`)
- Identifies free variables in lambda expressions
- Converts to closures with explicit environment capture
- Prepares for bytecode generation

### 6. Code Generator (`CodeGen.hs`)
- Compiles AST to bytecode instructions
- Manages constant pool
- Generates jump targets and patches them
- Creates CodeObject for each function

### 7. Virtual Machine (`VM.hs`)
- Stack-based execution model
- Frame management for function calls
- Proper stack cleanup after calls
- Built-in function dispatch

## Key Design Decisions

### Stack Management
- **Top of stack = head of list**: `[newest, ..., oldest]`
- Function arguments popped before creating new frame
- Return values pushed to caller's stack
- Critical fix: Caller frame stack updated to remove arguments

### Function Calls
1. Arguments pushed to stack (left to right)
2. CALL instruction:
   - Pop N arguments from caller stack
   - Create new frame with arguments as locals
   - Push new frame onto frame stack
3. RETURN instruction:
   - Pop return value from current frame
   - Pop current frame
   - Push return value to caller's stack

### Variable Scoping
- Local variables stored in frame's local array
- Accessed by index (not name) for efficiency
- Alpha renaming ensures unique names across scopes

### Tail Call Optimization
- `ITailCall` instruction replaces current frame
- Prevents stack growth in recursive calls
- Preserves constant stack space

## Instruction Set

### Stack Operations
- `IConst idx` - Push constant from pool
- `IPop` - Discard top of stack

### Variables
- `ILoad slot` - Load local variable
- `IStore slot` - Store to local variable

### Control Flow
- `IJump target` - Unconditional jump
- `IJumpIfFalse target` - Jump if top of stack is false
- `IReturn` - Return from function

### Function Calls
- `ICall arity name` - Call function
- `ITailCall arity name` - Tail call
- `IPrim op` - Call primitive operation

### Closure Operations
- `IMakeClosure name [slots]` - Create closure
- `ILoadClosure idx` - Load from closure environment
- `IStoreClosure idx` - Store to closure environment

## Module Descriptions

- **AST.hs** - Core data type definitions
- **SExprParser.hs** - Parser combinator-based S-expression parser
- **MacroExpander.hs** - Compile-time macro expansion
- **Desugar.hs** - Syntactic sugar removal
- **AlphaRename.hs** - Variable uniqueness via renaming
- **ClosureConversion.hs** - Lambda lifting and closure creation
- **CodeGen.hs** - Bytecode generation with constant pooling
- **VM.hs** - Stack-based virtual machine
- **Builtins.hs** - Primitive operations (+, -, *, etc.)
- **Compiler.hs** - Compilation pipeline orchestration
- **Disasm.hs** - Bytecode disassembler for debugging

## Error Handling

Current error types:
- `ParseError` - Syntax errors during parsing
- `SyntaxError` - Invalid S-expression structure
- `CompileError` - General compilation failures
- `VMError` - Runtime errors (stack underflow, undefined functions, etc.)

## Performance Characteristics

- **Parse**: O(n) where n = source size
- **Compile**: O(n * d) where d = max nesting depth
- **Execute**: O(1) per instruction
- **Function call overhead**: O(k) where k = number of arguments
- **Memory**: O(f * l) where f = max call depth, l = locals per frame

## Testing

Comprehensive test suite with 36 tests covering:
- Basic arithmetic and logic
- Recursive functions (factorial, fibonacci, gcd)
- Mutually recursive functions
- Higher-order functions
- Complex nested expressions

Current status: **100% tests passing** ✅

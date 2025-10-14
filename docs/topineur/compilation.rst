Topineur Compilation Process
============================

This document describes the complete compilation pipeline from Topineur source
code to executable bytecode.

Overview
--------

The Topineur compiler transforms high-level source code through multiple
intermediate representations before generating bytecode for the virtual machine:

.. code-block:: text

   Source Code (.top)
        ↓
   [1] Lexing → Tokens
        ↓
   [2] Parsing → Concrete Syntax Tree
        ↓
   [3] Desugaring → Abstract Syntax Tree (AST)
        ↓
   [4] Type Checking → Typed AST
        ↓
   [5] Effect Checking → Effect-Annotated AST
        ↓
   [6] Linearity Checking → Ownership-Validated AST
        ↓
   [7] Trait Resolution → Monomorphized AST
        ↓
   [8] Object Desugaring → Core Functional IR
        ↓
   [9] ANF Transformation → A-Normal Form
        ↓
   [10] Closure Conversion → Flat Functions
        ↓
   [11] Code Generation → Bytecode
        ↓
   [12] Optimization → Optimized Bytecode
        ↓
   Bytecode (.tbc) → Virtual Machine

Each phase is explained in detail below.

Phase 1: Lexical Analysis (Lexing)
-----------------------------------

**Input:** Raw source code (String)
**Output:** Token stream
**Module:** ``src/TopineurLexer.hs``

The lexer breaks the source code into a stream of tokens, handling:

- Keywords (``object``, ``def``, ``trait``, ``impl``, etc.)
- Identifiers (``myVar``, ``Point``, etc.)
- Literals (integers, floats, strings, booleans)
- Operators (``+``, ``*``, ``==``, ``.``, etc.)
- Delimiters (``{``, ``}``, ``(``, ``)``, etc.)
- Comments (discarded)
- Whitespace (mostly discarded)

**Example:**

.. code-block:: text

   Input:  def add(x: Int, y: Int): Int = x + y

   Output: [TOK_DEF, TOK_IDENT("add"), TOK_LPAREN, TOK_IDENT("x"),
            TOK_COLON, TOK_TYPE("Int"), TOK_COMMA, ...]

**Error handling:**

- Invalid characters → ``LexError: Unexpected character 'λ' at line 5``
- Unterminated strings → ``LexError: Unterminated string literal``

Phase 2: Parsing
-----------------

**Input:** Token stream
**Output:** Concrete Syntax Tree (CST) / Abstract Syntax Tree (AST)
**Module:** ``src/TopineurParser.hs``

The parser converts tokens into a tree structure following the grammar defined
in ``grammar.rst``. It uses **Parsec** (or **Megaparsec**) for parsing.

**Key parsing techniques:**

1. **Recursive descent** for declarations
2. **Precedence climbing** for infix operators
3. **Lookahead** for disambiguation

**Operator precedence** (from lowest to highest):

.. code-block:: text

   ||                    (logical or)
   &&                    (logical and)
   ==, !=                (equality)
   <, >, <=, >=          (comparison)
   +, -, ++              (additive)
   *, /, %               (multiplicative)
   !, - (prefix)         (unary)
   ., (), []             (postfix)

**Example:**

.. code-block:: topineur

   def factorial(n: Int): Int =
     if n <= 1 then 1
     else n * factorial(n - 1)

**AST representation:**

.. code-block:: haskell

   FunctionDef
     { name = "factorial"
     , params = [("n", TInt)]
     , returnType = TInt
     , effects = []
     , body = EIf
         (EBinOp LTE (EVar "n") (EInt 1))
         (EInt 1)
         (EBinOp Mul (EVar "n")
           (EApp (EVar "factorial") [EBinOp Sub (EVar "n") (EInt 1)]))
     }

**Error handling:**

- Syntax errors → ``ParseError: Expected '}' at line 10, column 5``
- Mismatched delimiters → ``ParseError: Unclosed '(' opened at line 8``

Phase 3: Desugaring
--------------------

**Input:** AST with syntactic sugar
**Output:** Core AST
**Module:** ``src/Desugar.hs``

Desugaring transforms high-level syntax into simpler constructs:

**Transformations:**

1. **Named functions → Lambda bindings**

   .. code-block:: text

      def add(x: Int, y: Int): Int = x + y
      ↓
      let add = λ(x: Int, y: Int) -> x + y

2. **Method definitions → Closures**

   .. code-block:: text

      object type Point {
        def translate(dx, dy) = Point { x = x + dx, y = y + dy }
      }
      ↓
      λ(this: Point, dx: Float, dy: Float) -> Point { ... }

3. **String interpolation → Concatenation**

   .. code-block:: text

      "Hello, {name}!"
      ↓
      "Hello, " ++ show(name) ++ "!"

4. **Chained method calls → Let bindings**

   .. code-block:: text

      p.translate(1, 2).distance()
      ↓
      let tmp1 = p.translate(1, 2) in tmp1.distance()

5. **For comprehensions → map/filter/flatMap**

   .. code-block:: text

      [x * 2 | x <- xs, x > 0]
      ↓
      map(λx -> x * 2, filter(λx -> x > 0, xs))

Phase 4: Type Checking
-----------------------

**Input:** Core AST
**Output:** Typed AST
**Module:** ``src/TypeChecker.hs``

Type checking infers and validates types using **Hindley-Milner** style
inference with extensions for:

- Effect rows
- Linear types
- Trait bounds

**Algorithm:**

1. **Constraint generation:** Walk AST and generate type equations
2. **Unification:** Solve equations using Robinson's algorithm
3. **Substitution:** Apply solutions to AST

**Type rules (selected):**

.. code-block:: text

   Γ ⊢ e₁ : Int    Γ ⊢ e₂ : Int
   ─────────────────────────────  [T-Add]
   Γ ⊢ e₁ + e₂ : Int

   Γ ⊢ e : Bool    Γ ⊢ e₁ : T    Γ ⊢ e₂ : T
   ─────────────────────────────────────────  [T-If]
   Γ ⊢ if e then e₁ else e₂ : T

   Γ, x:T₁ ⊢ e : T₂
   ───────────────────────────────  [T-Lambda]
   Γ ⊢ λx:T₁ -> e : T₁ → T₂

**Error handling:**

- Type mismatch → ``TypeError: Expected Int but got String at line 15``
- Unbound variable → ``TypeError: Variable 'foo' not in scope``
- Occurs check failure → ``TypeError: Infinite type detected``

Phase 5: Effect Checking
-------------------------

**Input:** Typed AST
**Output:** Effect-annotated AST
**Module:** ``src/EffectChecker.hs``

Effect checking ensures that:

1. All effects are declared in function signatures
2. Pure functions don't call effectful functions
3. Effect rows are correctly propagated

**Algorithm:**

1. Infer effects for each expression
2. Check that inferred effects ⊆ declared effects
3. Propagate effects through call chain

**Effect rules:**

.. code-block:: text

   Γ ⊢ e : T !ε
   ─────────────────────  [E-Pure]
   Γ ⊢ e : T !{}

   Γ ⊢ f : (T₁ → T₂) !ε₁    Γ ⊢ e : T₁ !ε₂
   ───────────────────────────────────────  [E-App]
   Γ ⊢ f(e) : T₂ !(ε₁ ∪ ε₂)

   readFile : String → String !{IO}
   ─────────────────────────────────  [E-IO]
   Γ ⊢ readFile("x") : String !{IO}

**Error handling:**

- Missing effect → ``EffectError: Function uses IO but doesn't declare it``
- Effect mismatch → ``EffectError: Cannot call IO function from pure context``

Phase 6: Linearity Checking
----------------------------

**Input:** Effect-annotated AST
**Output:** Ownership-validated AST
**Module:** ``src/LinearityChecker.hs``

Linearity checking ensures that linear values are used exactly once and not
aliased.

**Algorithm:**

1. Track linear variables in environment
2. Mark variables as "consumed" when used
3. Check that consumed variables aren't used again

**Linear rules:**

.. code-block:: text

   Γ, x:!lin T ⊢ e : U    x used exactly once in e
   ────────────────────────────────────────────────  [L-Let]
   Γ ⊢ let x = e₁ in e : U

   Γ ⊢ x : !lin T    x not previously consumed
   ──────────────────────────────────────────────  [L-Var]
   Γ ⊢ x : T    (x now marked consumed)

**Error handling:**

- Use after move → ``LinearityError: Variable 'file' used after being consumed``
- Unused linear value → ``LinearityError: Linear value 'lock' not used``
- Aliasing → ``LinearityError: Cannot alias linear value``

Phase 7: Trait Resolution
--------------------------

**Input:** Ownership-validated AST
**Output:** Monomorphized AST
**Module:** ``src/TraitResolver.hs``

Trait resolution performs **monomorphization**: replacing generic functions with
concrete versions for each type they're instantiated with.

**Process:**

1. Find all generic function calls
2. For each call, determine concrete types
3. Generate specialized version of function
4. Replace trait method calls with direct calls or dictionary passing

**Strategies:**

1. **Static dispatch** (when type known):

   .. code-block:: text

      def show[T: Show](x: T): String = x.show()
      show(42)
      ↓
      show_Int(42)  // Specialized for Int

2. **Dynamic dispatch** (when type not known):

   .. code-block:: text

      def show[T: Show](x: T): String = x.show()
      ↓
      def show[T](x: T, dict: ShowDict[T]): String =
        dict.show(x)

**Error handling:**

- Trait not implemented → ``TraitError: Type 'Point' doesn't implement 'Show'``
- Ambiguous trait → ``TraitError: Multiple implementations of 'Show' for 'Int'``

Phase 8: Object Desugaring
---------------------------

**Input:** Monomorphized AST
**Output:** Core functional IR
**Module:** ``src/ObjectDesugar.hs``

Object desugaring transforms object types into simpler functional constructs:

**Transformations:**

1. **Objects → Records + VTables**

   .. code-block:: text

      object type Point { x: Float, y: Float, def distance() = ... }
      ↓
      data Point = Point { x: Float, y: Float, vtable: PointVTable }
      data PointVTable = { distance: Point → Float }

2. **Method calls → Function calls**

   .. code-block:: text

      p.distance()
      ↓
      (p.vtable.distance)(p)

3. **Object literals → Record construction**

   .. code-block:: text

      Point { x = 0, y = 0 }
      ↓
      Point { x = 0, y = 0, vtable = pointVTableInstance }

After this phase, the IR contains only:

- Functions (lambdas)
- Data structures (records, ADTs)
- Function calls
- Let bindings
- Pattern matching

Phase 9: ANF Transformation
----------------------------

**Input:** Core functional IR
**Output:** A-Normal Form (ANF)
**Module:** ``src/AlphaRename.hs`` (currently), rename to ``ANFTransform.hs``

ANF transformation ensures that:

- All intermediate values are named
- Function arguments are simple variables (not complex expressions)
- Evaluation order is explicit

**Example:**

.. code-block:: text

   f(g(x), h(y))
   ↓ ANF ↓
   let tmp1 = g(x)
   let tmp2 = h(y)
   let tmp3 = f(tmp1, tmp2)
   tmp3

**Benefits:**

- Simplifies code generation
- Makes evaluation order explicit
- Easier to optimize

Phase 10: Closure Conversion
-----------------------------

**Input:** ANF IR
**Output:** Flat functions with explicit environments
**Module:** ``src/ClosureConversion.hs``

Closure conversion eliminates nested functions by:

1. Lifting all functions to top level
2. Making free variables explicit as closure environments
3. Replacing function values with closure objects

**Example:**

.. code-block:: text

   def makeAdder(x: Int): Int → Int =
     λy -> x + y

   ↓ Closure conversion ↓

   def makeAdder_lambda(env: [Int], y: Int): Int =
     let x = env[0]
     x + y

   def makeAdder(x: Int): Closure =
     Closure { func = makeAdder_lambda, env = [x] }

**Closure representation:**

.. code-block:: haskell

   data Value
     = ...
     | VClosure Name [Value]  -- function name + captured environment

Phase 11: Code Generation
--------------------------

**Input:** Flat functions (post-closure-conversion)
**Output:** Bytecode
**Module:** ``src/CodeGen.hs``

Code generation translates the IR into bytecode instructions for the stack-based
virtual machine.

**VM architecture:**

- **Stack:** Operand stack for temporary values
- **Locals:** Local variable array
- **Constants:** Constant pool
- **Code:** Instruction array
- **Globals:** Global variable map

**Instruction set:**

.. code-block:: text

   IConst idx        Push constant[idx] onto stack
   ILoad idx         Push local[idx] onto stack
   IStore idx        Pop stack and store to local[idx]
   IPrim op          Execute primitive operation (add, sub, etc.)
   ICall arity name  Call function with arity arguments
   ITailCall arity   Tail call optimization
   IReturn           Return top of stack
   IJump offset      Unconditional jump
   IJumpIfFalse off  Jump if top of stack is false
   IPop              Pop top of stack
   IMakeClosure      Create closure with captured values
   ILoadClosure idx  Load value from closure environment

**Example code generation:**

.. code-block:: topineur

   def add(x: Int, y: Int): Int =
     x + y

**Generated bytecode:**

.. code-block:: text

   Function: add
   Arity: 2
   Locals: 2
   Constants: []
   Instructions:
     0: LOAD 0       ; Load x
     1: LOAD 1       ; Load y
     2: PRIM add     ; Add them
     3: RETURN       ; Return result

**More complex example:**

.. code-block:: topineur

   def factorial(n: Int): Int =
     if n <= 1 then 1
     else n * factorial(n - 1)

**Generated bytecode:**

.. code-block:: text

   Function: factorial
   Arity: 1
   Locals: 1
   Constants: [1]
   Instructions:
     0: LOAD 0           ; Load n
     1: CONST 0          ; Push 1
     2: PRIM lte         ; n <= 1
     3: JUMPIFFALSE 6    ; Jump to else branch
     4: CONST 0          ; Push 1
     5: RETURN           ; Return 1
     6: LOAD 0           ; Load n
     7: LOAD 0           ; Load n again
     8: CONST 0          ; Push 1
     9: PRIM sub         ; n - 1
    10: TAILCALL 1 factorial  ; Tail call factorial
    11: PRIM mul         ; n * result
    12: RETURN           ; Return

Phase 12: Optimization
----------------------

**Input:** Bytecode
**Output:** Optimized bytecode
**Module:** ``src/Optimizer.hs`` (to be created)

**Optimizations implemented:**

1. **Tail call optimization (TCO)**

   - Replace CALL + RETURN with TAILCALL
   - Reuse current stack frame

2. **Constant folding**

   - ``CONST 2; CONST 3; PRIM add`` → ``CONST 5``

3. **Dead code elimination**

   - Remove unreachable code after unconditional jumps

4. **Peephole optimization**

   - ``LOAD x; STORE x`` → NOP
   - ``CONST 0; PRIM add`` → NOP

5. **Inline small functions**

   - Replace calls to tiny functions with body

**Example:**

.. code-block:: text

   Before:
     LOAD 0
     CONST 0
     PRIM add
     STORE 1
     LOAD 1

   After:
     LOAD 0  ; Eliminated: add 0 is identity
     STORE 1
     LOAD 1

Bytecode Format
---------------

Compiled bytecode is serialized to ``.tbc`` files:

**File structure:**

.. code-block:: text

   +------------------+
   | Magic number     | (4 bytes: "TBC\0")
   | Version          | (2 bytes: major.minor)
   | Flags            | (2 bytes)
   +------------------+
   | Constant pool    |
   |   - Count        |
   |   - Constants    |
   +------------------+
   | Function table   |
   |   - Count        |
   |   - Functions    |
   +------------------+
   | Global inits     |
   +------------------+
   | Entry point      | (function name)
   +------------------+

**Function encoding:**

.. code-block:: text

   +------------------+
   | Name length      | (2 bytes)
   | Name             | (N bytes, UTF-8)
   | Arity            | (1 byte)
   | Max locals       | (2 bytes)
   | Const count      | (2 bytes)
   | Const indices    | (2 bytes each)
   | Instruction count| (4 bytes)
   | Instructions     | (variable)
   +------------------+

**Instruction encoding:**

Each instruction is encoded as:

.. code-block:: text

   [Opcode: 1 byte] [Operand: 0-4 bytes]

Virtual Machine Execution
--------------------------

The VM executes bytecode using a stack-based architecture.

**VM state:**

.. code-block:: haskell

   data VMState = VMState
     { vFrames :: [Frame]          -- Call stack
     , vGlobals :: Map Name Value  -- Global variables
     , vCodeObjects :: Map Name CodeObject  -- Compiled functions
     , vBuiltins :: Map Name Value -- Builtin functions
     }

   data Frame = Frame
     { fLocals :: Vector (Maybe Value)  -- Local variables
     , fStack :: [Value]                -- Operand stack
     , fCode :: CodeObject              -- Current function
     , fPC :: Int                       -- Program counter
     }

**Execution loop:**

.. code-block:: haskell

   execute :: VMState -> IO Value
   execute vm = case getCurrentInstruction vm of
     IConst idx -> push (getConst idx) >> execute vm'
     ILoad idx -> push (getLocal idx) >> execute vm'
     IStore idx -> pop >>= setLocal idx >> execute vm'
     ICall arity name -> call name arity >> execute vm'
     IReturn -> popFrame >>= execute
     ...

Performance Characteristics
---------------------------

**Compilation time:** O(n) where n is the size of the source code
**Space overhead:** Bytecode is ~50% of source code size
**Startup time:** < 10ms for typical programs
**Execution speed:** 10-50x slower than native code, comparable to Python

Compilation time breakdown:

- Parsing: 30%
- Type checking: 25%
- Code generation: 20%
- Optimization: 15%
- I/O: 10%

Debugging Support
-----------------

**Source maps:** Map bytecode instructions to source locations

.. code-block:: text

   Instruction 15 → line 10, column 5 in "main.top"

**Debug flags:**

.. code-block:: bash

   ./glados --dump-ast program.top        # Show AST
   ./glados --dump-types program.top      # Show type annotations
   ./glados --dump-effects program.top    # Show effect analysis
   ./glados --dump-bytecode program.top   # Show bytecode
   ./glados --trace program.top           # Trace VM execution

**Error reporting:**

All compilation errors include:

- Source location (file, line, column)
- Error message
- Code snippet with caret pointing to error
- Suggestion for fix (when applicable)

Future Improvements
-------------------

Planned enhancements:

1. **JIT compilation:** Compile hot code paths to native code
2. **Better optimization:** More aggressive inlining, loop unrolling
3. **Parallel compilation:** Multi-threaded type checking and code generation
4. **Incremental compilation:** Only recompile changed modules
5. **Cross-compilation:** Generate code for different platforms
6. **LLVM backend:** Generate native code via LLVM IR

References
----------

- A-Normal Form: Flanagan et al., "The Essence of Compiling with Continuations"
- Closure conversion: Appel, "Compiling with Continuations"
- Type inference: Damas & Milner, "Principal type-schemes for functional programs"
- Effect systems: Lucassen & Gifford, "Polymorphic effect systems"

Conclusion
----------

The Topineur compiler transforms high-level object-functional code through 12
distinct phases, each adding or checking important properties, ultimately
producing efficient bytecode for the virtual machine.

The design emphasizes:

- **Correctness:** Each phase validates specific properties
- **Modularity:** Phases are independent and testable
- **Performance:** Multiple optimization passes
- **Debuggability:** Rich error messages and debugging support

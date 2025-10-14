Transformation Plan: From Lisp to Topineur
==========================================

This document describes the complete transformation strategy to migrate the
GLaDOS project from a Lisp interpreter/compiler to a full Topineur language
implementation.

Overview
--------

**Current state:**
  A fully functional Lisp compiler with:

  - S-Expression parser (SExprParser.hs)
  - Lisp-specific AST (AST.hs)
  - Macro expander (MacroExpander.hs)
  - Desugaring pass (Desugar.hs)
  - ANF transformation (AlphaRename.hs)
  - Closure conversion (ClosureConversion.hs)
  - Bytecode generation (CodeGen.hs)
  - Stack-based VM with TCO (VM.hs)
  - Builtin functions (Builtins.hs)

**Target state:**
  A Topineur compiler supporting:

  - Object types with immutable fields and methods
  - Traits and typeclasses
  - Effect system (effect rows)
  - Linear types for safe mutation
  - Strict evaluation with opt-in laziness
  - Explicit ADT/Object conversions
  - Full bytecode compilation and VM execution

Transformation Strategy
-----------------------

This transformation will be done in phases to ensure the system remains
compilable and testable at each step:

Phase 1: Extend the AST
~~~~~~~~~~~~~~~~~~~~~~~

**Goal:** Add Topineur-specific constructs to the AST while keeping existing
Lisp constructs temporarily.

**New types to add:**

.. code-block:: haskell

   -- Object type definitions
   data ObjectDef = ObjectDef
     { objName :: Name
     , objFields :: [(Name, Type)]
     , objMethods :: [MethodDef]
     } deriving (Eq, Show)

   data MethodDef = MethodDef
     { methName :: Name
     , methParams :: [(Name, Type)]
     , methReturnType :: Type
     , methEffects :: EffectRow
     , methBody :: Expr
     } deriving (Eq, Show)

   -- Trait definitions
   data TraitDef = TraitDef
     { traitName :: Name
     , traitMethods :: [MethodSig]
     } deriving (Eq, Show)

   data TraitImpl = TraitImpl
     { implTrait :: Name
     , implForType :: Type
     , implMethods :: [MethodDef]
     } deriving (Eq, Show)

   -- Effect system
   data EffectRow = EffectRow [Effect]
     deriving (Eq, Show)

   data Effect
     = EffIO
     | EffState
     | EffNetwork
     | EffCustom Name
     deriving (Eq, Show)

   -- Type system
   data Type
     = TInt
     | TFloat
     | TBool
     | TString
     | TUnit
     | TObject Name
     | TLinear Type  -- !lin T
     | TFunc [Type] EffectRow Type
     | TVar Name
     deriving (Eq, Show)

   -- Extended expressions
   data Expr
     = ... -- Keep existing Lisp expressions
     | EObjectDef ObjectDef
     | ETraitDef TraitDef
     | ETraitImpl TraitImpl
     | EObjectLit Name [(Name, Expr)]  -- Point { x = 1.0, y = 2.0 }
     | EMethodCall Expr Name [Expr]     -- obj.method(args)
     | EFieldAccess Expr Name           -- obj.field
     | ETyped Expr Type                 -- expr : Type
     | ELinearBind Name Expr Expr       -- linear let
     deriving (Eq, Show)

**Files to modify:**
  - ``src/AST.hs`` — add new data types

**Action:** Extend AST with Topineur constructs


Phase 2: Implement the Topineur Parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Goal:** Create a new parser that understands Topineur syntax, completely
replacing the S-Expression parser.

**New parser structure:**

.. code-block:: text

   TopineurParser.hs
     - parseModule        -- top-level definitions
     - parseObjectDef     -- object type { ... }
     - parseTraitDef      -- trait Name { ... }
     - parseTraitImpl     -- impl Trait for Type { ... }
     - parseMethodDef     -- def name(params): RetType = body
     - parseExpr          -- expressions with infix operators
     - parseType          -- type annotations
     - parseEffectRow     -- !{IO, State, ...}
     - parseLiteral       -- values, object literals
     - parseBlock         -- { stmt; stmt; expr }

**Key parsing challenges:**

1. **Infix operators with precedence** (Part 2 requirement)

   - Implement Pratt parsing or operator precedence climbing
   - Support user-defined infix operators with custom precedence
   - Standard operators: +, -, *, /, <, >, ==, &&, ||, etc.

2. **Layout-sensitive syntax**

   - Handle indentation for blocks
   - Or use explicit braces/semicolons (simpler first step)

3. **Method call chaining**

   - Parse ``obj.method1().method2()``
   - Associate correctly with precedence

4. **Effect annotations**

   - Parse ``!{Effect1, Effect2}``
   - Allow empty effect rows (pure functions)

**Example Topineur syntax to parse:**

.. code-block:: topineur

   // Object definition
   object type Point {
     x: Float
     y: Float

     def translate(dx: Float, dy: Float): Point =
       Point { x = x + dx, y = y + dy }

     def distance(): Float =
       sqrt(x * x + y * y)
   }

   // Trait definition
   trait Drawable {
     def draw(ctx: Canvas): !{IO} Unit
   }

   // Trait implementation
   impl Drawable for Point {
     def draw(ctx: Canvas): !{IO} Unit =
       ctx.strokeCircle(x, y, 2.0)
   }

   // Function definition
   def main(): !{IO} Unit =
     let p = Point { x = 0.0, y = 0.0 }
     let p2 = p.translate(3.0, 4.0)
     println("distance = " ++ show(p2.distance()))

**Files to create:**
  - ``src/TopineurParser.hs`` — new parser
  - ``src/TopineurLexer.hs`` — lexer with tokens

**Files to modify:**
  - ``src/Compiler.hs`` — switch to new parser

**Action:** Replace SExprParser with TopineurParser


Phase 3: Type System and Type Checker
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Goal:** Implement type inference and checking, including effect tracking and
linearity checks.

**Required components:**

1. **Type inference engine**

   - Hindley-Milner style inference
   - Handle explicit type annotations
   - Infer method types from usage
   - Generate type constraints

2. **Effect checker**

   - Track effects through call chains
   - Ensure effect annotations are accurate
   - Prevent calling effectful code from pure contexts
   - Compute effect row unions

3. **Linearity checker**

   - Track linear values (``!lin T``)
   - Ensure single use (no aliasing)
   - Allow mutation through linear references
   - Detect use-after-move errors

4. **Trait resolution**

   - Check trait implementations
   - Resolve method calls through traits
   - Handle trait bounds on type variables

**Files to create:**
  - ``src/TypeChecker.hs`` — main type checking logic
  - ``src/TypeInference.hs`` — constraint generation and solving
  - ``src/EffectChecker.hs`` — effect system
  - ``src/LinearityChecker.hs`` — linearity analysis

**Files to modify:**
  - ``src/Compiler.hs`` — add type checking pass
  - ``src/AST.hs`` — add type annotations to Expr

**Action:** Implement type system with effects and linearity


Phase 4: Desugar Topineur Constructs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Goal:** Transform high-level Topineur constructs into a simpler intermediate
representation that the existing compilation pipeline can handle.

**Desugaring transformations:**

1. **Object types → Records + closures**

   .. code-block:: text

      object type Point { x: Float, y: Float, def translate(...) = ... }

      ↓ desugar to ↓

      data Point = Point { x: Float, y: Float, vtable: VTable }
      where VTable = { translate: Point → Float → Float → Point }

2. **Method calls → Function calls**

   .. code-block:: text

      p.translate(1.0, 2.0)

      ↓ desugar to ↓

      (p.vtable.translate)(p, 1.0, 2.0)

3. **Traits → Dictionary passing**

   .. code-block:: text

      def draw_all(items: [T]) where T: Drawable = ...

      ↓ desugar to ↓

      def draw_all(items: [T], dict: DrawableDict) = ...

4. **Linear types → Runtime tracking (or erase)**

   - Simple approach: erase linearity after checking
   - Advanced: add runtime linear reference counting

5. **Effect annotations → Erase (or embed in monad)**

   - Effects are checked statically, then erased
   - Or wrap effectful code in IO/Effect monad

**Files to modify:**
  - ``src/Desugar.hs`` — add Topineur desugaring rules

**Action:** Desugar Topineur to core functional IR


Phase 5: Adapt Code Generation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Goal:** Ensure the bytecode generator can handle the desugared Topineur code.

**Required changes:**

1. **Object creation**

   - Allocate object with fields + vtable
   - Generate instruction: ``MAKE_OBJECT <type> <field_count>``

2. **Method dispatch**

   - Load vtable from object
   - Call function pointer
   - Or use direct call if statically known

3. **Trait dictionaries**

   - Pass trait dictionaries as hidden parameters
   - Call methods through dictionary

4. **Linear value tracking**

   - Add VM instructions to track ownership transfer
   - Or use existing closure mechanism

**Files to modify:**
  - ``src/CodeGen.hs`` — handle new IR constructs
  - ``src/AST.hs`` — possibly add new Instr types

**Action:** Extend code generation for objects and traits


Phase 6: Extend the Virtual Machine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Goal:** Add VM support for objects, trait dictionaries, and effect handling.

**New VM features:**

1. **Object values**

   .. code-block:: haskell

      data Value
        = ... -- existing values
        | VObject Name (Map.Map Name Value) VTable
        | VTraitDict Name (Map.Map Name Value)

2. **New instructions**

   .. code-block:: haskell

      data Instr
        = ... -- existing instructions
        | IMakeObject Name Int  -- create object with N fields
        | IGetField Name        -- get field from object
        | ISetField Name        -- set field (for mutable objects)
        | IMethodCall Name Int  -- call method with N args
        | ILoadDict Name        -- load trait dictionary

3. **Effect runtime**

   - Track effect context in VM state
   - Optionally: effect handlers for advanced control

**Files to modify:**
  - ``src/VM.hs`` — add object/trait support
  - ``src/AST.hs`` — add new Value and Instr types

**Action:** Extend VM for Topineur runtime


Phase 7: Update Builtins
~~~~~~~~~~~~~~~~~~~~~~~~~

**Goal:** Provide Topineur-style standard library functions.

**New builtins:**

1. **Object utilities**

   - ``show`` for objects
   - Equality checks for objects

2. **Effect primitives**

   - ``println`` : ``String → !{IO} Unit``
   - ``readLine`` : ``!{IO} String``
   - ``fileRead`` : ``String → !{IO} String``

3. **Trait utilities**

   - Generic functions using traits (Show, Eq, Ord)

**Files to modify:**
  - ``src/Builtins.hs`` — update to Topineur signatures

**Action:** Update builtins for Topineur


Phase 8: Documentation
~~~~~~~~~~~~~~~~~~~~~~

**Goal:** Complete all Part 2 documentation requirements.

**Required documentation:**

1. **Grammar (BNF)** — MANDATORY

   - Formal grammar in BNF or EBNF
   - Cover all Topineur syntax
   - Show precedence and associativity

2. **User manual**

   - Getting started guide
   - Language features with examples
   - Standard library reference
   - Error message guide

3. **Compilation process**

   - Pipeline diagram
   - Each compilation pass explained
   - Bytecode format specification

4. **Security review**

   - Analysis of inspiration languages (Haskell, Rust, ML)
   - Security features implemented:

     * Effect system prevents hidden side effects
     * Linearity prevents use-after-free and data races
     * Type safety prevents undefined behavior
     * No null references (explicit Option type)

5. **Accessibility**

   - Documentation in accessible formats
   - Screen reader friendly
   - Clear, simple language

**Files to create:**
  - ``docs/topineur/grammar.rst`` — BNF grammar
  - ``docs/topineur/user_manual.rst`` — user guide
  - ``docs/topineur/compilation.rst`` — compiler internals
  - ``docs/topineur/security_review.rst`` — security analysis

**Files to update:**
  - ``README.md`` — update for Topineur
  - ``docs/README.md`` — update doc map

**Action:** Write complete documentation


Phase 9: Testing
~~~~~~~~~~~~~~~~

**Goal:** Comprehensive test coverage for Topineur.

**Test categories:**

1. **Unit tests** (Haskell)

   - Parser tests
   - Type checker tests
   - Desugaring tests
   - Code generation tests
   - VM tests

2. **Integration tests**

   - End-to-end compilation tests
   - Example programs
   - Error message tests

3. **Regression tests**

   - All examples must compile and run
   - Jenkins CI integration

**Files to create:**
  - ``test/TopineurParserSpec.hs``
  - ``test/TypeCheckerSpec.hs``
  - ``test/IntegrationSpec.hs``

**Files to update:**
  - ``test/`` — rewrite all tests for Topineur

**Action:** Create comprehensive test suite


Phase 10: Standard Library
~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Goal:** Provide a useful standard library written in Topineur.

**Modules to create:**

1. **Core**

   - Option type
   - Result type
   - List operations
   - String utilities

2. **IO**

   - File operations
   - Console IO
   - Formatting

3. **Collections**

   - Vector
   - HashMap
   - Set

4. **Traits**

   - Show, Eq, Ord
   - Functor, Monad (if applicable)

**Files to create:**
  - ``stdlib/core.top``
  - ``stdlib/io.top``
  - ``stdlib/collections.top``
  - ``stdlib/prelude.top`` — auto-imported

**Action:** Write standard library in Topineur


Migration Checklist
-------------------

Before considering the transformation complete, verify:

- [ ] All Topineur examples compile and run correctly
- [ ] Type checker catches common errors with helpful messages
- [ ] Effect system tracks side effects accurately
- [ ] Linearity checker prevents aliasing bugs
- [ ] VM executes object method calls correctly
- [ ] Trait dispatch works (both static and dynamic)
- [ ] Documentation is complete (especially BNF grammar)
- [ ] Security review is thorough
- [ ] Test coverage > 80%
- [ ] Standard library is usable
- [ ] CI/CD pipeline works
- [ ] All LISP code has been removed

File Removal Plan
-----------------

**Files to delete after transformation:**

- ``src/SExprParser.hs`` — replaced by TopineurParser
- ``src/SExprConstruct.hs`` — no longer needed
- ``src/MacroExpander.hs`` — Topineur uses different metaprogramming
- ``examples/*.lisp`` — replaced by ``examples/topineur/*.top``
- ``docs/LANGUAGE_GUIDE.md`` — if it's Lisp-specific

**Files to rename:**

- ``glados`` executable → keep name (it's the project name)
- ``README.md`` → update content, keep file
- ``B-FUN-500_GLaDOS.md`` → keep as project reference

Timeline Estimate
-----------------

**Realistic timeline for one developer:**

- Phase 1 (AST extension): 2-3 days
- Phase 2 (Parser): 5-7 days
- Phase 3 (Type system): 7-10 days
- Phase 4 (Desugaring): 3-5 days
- Phase 5 (Code generation): 3-4 days
- Phase 6 (VM extension): 4-6 days
- Phase 7 (Builtins): 2-3 days
- Phase 8 (Documentation): 5-7 days
- Phase 9 (Testing): 4-6 days
- Phase 10 (Stdlib): 3-5 days

**Total: 38-56 days** (approx 2-3 months)

**For team defense:** Prioritize phases 1-6 and 8 (grammar + security). The
rest can be minimal but functional.

Next Steps
----------

1. Review this plan with your team
2. Set up a new branch: ``git checkout -b topineur-transformation``
3. Start with Phase 1 (AST extension)
4. Keep commits small and test frequently
5. Update this document as you discover necessary changes

Remember: **The goal is not perfection, but a working, well-documented
language that demonstrates the core concepts of Topineur.**

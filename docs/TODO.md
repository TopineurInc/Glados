# AST Adaptation TODO — Lisp + Topineur Support

**Goal**: Extend the GLaDOS AST to support both Lisp (`.lisp`) and Topineur (`.top`) languages in a unified codebase while maintaining backward compatibility with existing Lisp tests.

**Reference**: `examples/all.top` demonstrates all Topineur language features.

**Scope**: This TODO covers ONLY the AST structure, expressions, types, values, and instructions. Parser, VM execution, builtins, and type checking are separate concerns handled in other branches.

**Branch**: `dev/top-ast` (AST-only modifications)

---

## Key Differences: Lisp vs Topineur

- **Types**: Topineur has static type annotations (parameters, returns, fields)
- **Control flow**: Topineur adds `while`, `for` loops, ranges (`start..end`)
- **Objects**: Topineur has nominal types (`object type`) with fields and methods
- **Return**: Topineur uses explicit `top` keyword (vs Lisp implicit return)
- **Data structures**: Native tuples, typed lists, annotations (`@cache`)
- **Mutability**: Topineur allows reassignment; Lisp is immutable

---

## AST Modifications Checklist

### Phase 1: Type System & Core Extensions

- [x] **1. Add `Type` data type in `src/AST.hs`**
  - Add: `TInt | TFloat | TBool | TString | TUnit | TList Type | TTuple [Type] | TObject Name | TFun [Type] Type`
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - Type system added, exported, all tests pass

- [x] **2. Extend `Expr` with type annotations**
  - Modify `ELambda` to include: `[(Name, Maybe Type)]` for params and `Maybe Type` for return
  - Modify `EDefine` similarly for function definitions
  - Files: `src/AST.hs`, `src/Desugar.hs`, `src/CodeGen.hs`, `src/AlphaRename.hs`, `src/ClosureConversion.hs`
  - **Status**: COMPLETED - ELambda now has `[(Name, Maybe Type)] (Maybe Type) Expr`, Lisp compatibility maintained via `LispLambda` pattern synonym in tests

- [x] **3. Add `EUnit` expression and `VUnit` value**
  - For Unit/void type support
  - Files: `src/AST.hs`, `src/AlphaRename.hs`, `src/ClosureConversion.hs`, `src/CodeGen.hs`, `src/Builtins.hs`, `app/Main.hs`
  - **Status**: COMPLETED - EUnit added to Expr, VVoid renamed to VUnit throughout codebase, all tests pass

### Phase 2: Control Flow

- [x] **4. Add loop constructs to `Expr`**
  - Add: `EWhile Expr Expr` (condition, body)
  - Add: `EFor Name Expr Expr Expr` (variable, start, end, body)
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - EWhile and EFor added to Expr, AlphaRename and ClosureConversion implemented

- [x] **5. Add `ERange` expression**
  - Add: `ERange Expr Expr` for `start..end` syntax
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - ERange added to Expr, AlphaRename and ClosureConversion implemented

- [x] **6. Add `EReturn` expression**
  - Add: `EReturn Expr` for explicit `top` keyword
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - EReturn added to Expr, AlphaRename and ClosureConversion implemented

### Phase 3: Operators

- [x] **7. Add binary operators to `Expr`**
  - Add: `EBinOp BinOp Expr Expr`
  - Define: `BinOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Lte | Gte | And | Or | Concat`
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - All binary operators implemented including Mod, AlphaRename and ClosureConversion implemented, CodeGen completed

- [x] **8. Add unary operators to `Expr`**
  - Add: `EUnOp UnOp Expr`
  - Define: `UnOp = Not | Neg`
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - EUnOp and UnOp added to AST, AlphaRename and ClosureConversion implemented, CodeGen completed, tests included

### Phase 4: Data Structures

- [x] **9. Add tuple support to `Expr`**
  - Add: `ETuple [Expr]` for tuple creation
  - Add: `ETupleDestruct [Name] Expr Expr` for destructuring
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - ETuple and ETupleDestruct added to Expr, AlphaRename and ClosureConversion implemented, CodeGen completed, 6 tests

- [x] **10. Add native list support**
  - Add: `EListLiteral [Expr] (Maybe Type)` distinct from `EList`
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - EListLiteral added to Expr, AlphaRename and ClosureConversion implemented, CodeGen completed, 5 tests

- [x] **11. Add indexing support**
  - Add: `EIndex Expr Expr` for `list[index]` syntax
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - EIndex added to Expr, AlphaRename and ClosureConversion implemented, CodeGen completed, tests included in tuples/lists

### Phase 5: Mutability

- [x] **12. Add assignment expression**
  - Add: `EAssign Name Expr` for mutable variable reassignment
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - EAssign added to Expr, AlphaRename and ClosureConversion implemented, CodeGen completed, 4 tests

### Phase 6: Objects

- [x] **13. Add object declaration to `Expr`**
  - Add: `EObjectDecl Name [Field] [Method]`
  - Define: `Field = (Name, Type, Maybe Expr)` (name, type, default value)
  - Define: `Method = (Name, [(Name, Type)], Type, Expr)` (name, params, return type, body)
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - EObjectDecl, Field, and Method added to AST, AlphaRename and ClosureConversion implemented, CodeGen completed, 5 tests

- [x] **14. Add object instantiation**
  - Add: `EObjectInst Name [(Name, Expr)]` for creating instances
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - EObjectInst added to Expr, AlphaRename and ClosureConversion implemented, CodeGen completed, tests included in objects

- [x] **15. Add member access**
  - Add: `EMemberAccess Expr Name` for field/method access
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - EMemberAccess added to Expr, AlphaRename and ClosureConversion implemented, CodeGen completed, tests included in objects

### Phase 7: Annotations & Modules

- [x] **16. Add annotation support**
  - Add: `Annotation = Cache | Custom String`
  - Add `[Annotation]` field to `EDefine` and `ELambda`
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - Annotation data type added, EDefine and ELambda now have [Annotation] parameter, AlphaRename and ClosureConversion implemented, CodeGen completed

- [x] **17. Add module/package support**
  - Add: `EPackage Name` for package declarations
  - Add: `EImport Name` for imports
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - EPackage and EImport added to Expr, AlphaRename and ClosureConversion implemented, CodeGen completed (no-ops), edge case tests

### Phase 8: Runtime Values

- [x] **18. Extend `Value` for new types**
  - Add: `VUnit` for Unit type
  - Add: `VList [Value]` for native lists
  - Add: `VTuple [Value]` for native tuples
  - Add: `VObject Name [(Name, Value)]` for object instances
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - VUnit, VList, VTuple, and VObject added to Value data type with Eq and Show instances

### Phase 9: Bytecode Instructions

- [x] **19. Add control flow instructions to `Instr`**
  - Add: `IWhile`, `IFor`, `IBreak`, `IContinue`
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - IWhile, IFor, IBreak, IContinue added to Instr data type

- [x] **20. Add data structure instructions**
  - Add: `ITupleCreate Int`, `ITupleGet Int`
  - Add: `IListCreate Int`, `IListGet`, `IListSet`
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - ITupleCreate, ITupleGet, IListCreate, IListGet, IListSet added to Instr

- [x] **21. Add object instructions**
  - Add: `IObjectCreate Name`, `IMemberGet Name`, `IMemberSet Name`
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - IObjectCreate, IMemberGet, IMemberSet added to Instr (note: IObjectCreate instead of ICreateObject)

- [x] **22. Add assignment instruction**
  - Add: `IAssign Int` for mutable variable update
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - IAssign added to Instr

- [x] **23. Add range instruction**
  - Add: `IRangeCreate` for range iteration
  - Files: `src/AST.hs`
  - **Status**: COMPLETED - IRangeCreate added to Instr

---

## CodeGen Adaptations (AST → Bytecode)

- [x] **24. Implement codegen for loops**
  - Handle `EWhile`, `EFor`, `ERange` in `compileExpr`
  - Emit appropriate loop instructions with jump patching
  - Files: `src/CodeGen.hs`
  - **Status**: COMPLETED - Loop constructs compile to bytecode with proper jump patching, 6 tests

- [x] **25. Implement codegen for all operators**
  - Handle `EBinOp`, `EUnOp` in `compileExpr`
  - Files: `src/CodeGen.hs`
  - **Status**: COMPLETED - All operators implemented (Add, Sub, Mul, Div, Mod, Lt, Lte, Gt, Gte, Eq, Neq, And, Or, Concat, Not, Neg), 18 tests (16 original + 2 for Mod)

- [x] **26. Implement codegen for tuples**
  - Handle `ETuple`, `ETupleDestruct` in `compileExpr`
  - Emit `ITupleCreate`, `ITupleGet`, `IStore` for destructuring
  - Files: `src/CodeGen.hs`
  - **Status**: COMPLETED - Tuple creation and destructuring implemented, 6 tests

- [x] **27. Implement codegen for native lists**
  - Handle `EListLiteral`, `EIndex` in `compileExpr`
  - Emit `IListCreate`, `IListGet`, `ITupleGet` for static indices
  - Files: `src/CodeGen.hs`
  - **Status**: COMPLETED - List literals and indexing implemented, 5 tests

- [x] **28. Implement codegen for objects**
  - Handle `EObjectDecl`, `EObjectInst`, `EMemberAccess`
  - Emit object-related instructions
  - Handle default field values
  - Files: `src/CodeGen.hs`
  - **Status**: COMPLETED - Object instantiation and member access implemented (declarations are no-ops), 5 tests

- [x] **29. Implement codegen for assignment**
  - Handle `EAssign` in `compileExpr`
  - Emit `IAssign` instruction, then reload value
  - Files: `src/CodeGen.hs`
  - **Status**: COMPLETED - Assignment to local variables implemented with value reload, 4 tests

- [x] **30. Implement codegen for return**
  - Handle `EReturn` in `compileExpr`
  - Emit `IReturn` instruction
  - Files: `src/CodeGen.hs`
  - **Status**: COMPLETED - Explicit return statement implemented, 3 tests

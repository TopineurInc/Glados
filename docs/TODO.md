# AST Adaptation TODO — Lisp + Topineur Support

**Goal**: Extend the GLaDOS AST to support both Lisp (`.lisp`) and Topineur (`.top`) languages in a unified codebase while maintaining backward compatibility with existing Lisp tests.

**Reference**: `examples/all.top` demonstrates all Topineur language features.

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

- [x] **5. Add `ERange` expression**
  - Add: `ERange Expr Expr` for `start..end` syntax
  - Files: `src/AST.hs`

- [x] **6. Add `EReturn` expression**
  - Add: `EReturn Expr` for explicit `top` keyword
  - Files: `src/AST.hs`

### Phase 3: Operators

- [x] **7. Add binary operators to `Expr`**
  - Add: `EBinOp BinOp Expr Expr`
  - Define: `BinOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Lte | Gte | And | Or | Concat`
  - Files: `src/AST.hs`

- [x] **8. Add unary operators to `Expr`**
  - Add: `EUnOp UnOp Expr`
  - Define: `UnOp = Not | Neg`
  - Files: `src/AST.hs`

### Phase 4: Data Structures

- [x] **9. Add tuple support to `Expr`**
  - Add: `ETuple [Expr]` for tuple creation
  - Add: `ETupleDestruct [Name] Expr Expr` for destructuring
  - Files: `src/AST.hs`

- [x] **10. Add native list support**
  - Add: `EListLiteral [Expr] (Maybe Type)` distinct from `EList`
  - Files: `src/AST.hs`

- [x] **11. Add indexing support**
  - Add: `EIndex Expr Expr` for `list[index]` syntax
  - Files: `src/AST.hs`

### Phase 5: Mutability

- [x] **12. Add assignment expression**
  - Add: `EAssign Name Expr` for mutable variable reassignment
  - Files: `src/AST.hs`

### Phase 6: Objects

- [x] **13. Add object declaration to `Expr`**
  - Add: `EObjectDecl Name [Field] [Method]`
  - Define: `Field = (Name, Type, Maybe Expr)` (name, type, default value)
  - Define: `Method = (Name, [(Name, Type)], Type, Expr)` (name, params, return type, body)
  - Files: `src/AST.hs`

- [x] **14. Add object instantiation**
  - Add: `EObjectInst Name [(Name, Expr)]` for creating instances
  - Files: `src/AST.hs`

- [x] **15. Add member access**
  - Add: `EMemberAccess Expr Name` for field/method access
  - Files: `src/AST.hs`

### Phase 7: Annotations & Modules

- [x] **16. Add annotation support**
  - Add: `Annotation = Cache | Custom String`
  - Add `[Annotation]` field to `EDefine` and `ELambda`
  - Files: `src/AST.hs`

- [x] **17. Add module/package support**
  - Add: `EPackage Name` for package declarations
  - Add: `EImport Name` for imports
  - Files: `src/AST.hs`

### Phase 8: Runtime Values

- [x] **18. Extend `Value` for new types**
  - Add: `VUnit` for Unit type
  - Add: `VList [Value]` for native lists
  - Add: `VTuple [Value]` for native tuples
  - Add: `VObject Name (Map Name Value)` for object instances
  - Files: `src/AST.hs`

### Phase 9: Bytecode Instructions

- [x] **19. Add control flow instructions to `Instr`**
  - Add: `IWhile`, `IFor`, `IBreak`, `IContinue`
  - Files: `src/AST.hs`

- [x] **20. Add data structure instructions**
  - Add: `ITupleCreate Int`, `ITupleGet Int`
  - Add: `IListCreate Int`, `IListGet`, `IListSet`
  - Files: `src/AST.hs`

- [x] **21. Add object instructions**
  - Add: `ICreateObject Name`, `IGetMember Name`, `ISetMember Name`
  - Files: `src/AST.hs`

- [x] **22. Add assignment instruction**
  - Add: `IAssign Int` for mutable variable update
  - Files: `src/AST.hs`

- [x] **23. Add range instruction**
  - Add: `IRangeCreate` for range iteration
  - Files: `src/AST.hs`

---

## CodeGen Adaptations

- [ ] **24. Implement codegen for loops**
  - Handle `EWhile`, `EFor`, `ERange` in `compileExpr`
  - Emit appropriate loop instructions
  - Files: `src/CodeGen.hs`

- [ ] **25. Implement codegen for operators**
  - Handle `EBinOp`, `EUnOp` in `compileExpr`
  - Files: `src/CodeGen.hs`

- [ ] **26. Implement codegen for tuples**
  - Handle `ETuple`, `ETupleDestruct` in `compileExpr`
  - Emit `ITupleCreate`, `ITupleGet`
  - Files: `src/CodeGen.hs`

- [ ] **27. Implement codegen for native lists**
  - Handle `EListLiteral`, `EIndex` in `compileExpr`
  - Emit `IListCreate`, `IListGet`
  - Files: `src/CodeGen.hs`

- [ ] **28. Implement codegen for objects**
  - Handle `EObjectDecl`, `EObjectInst`, `EMemberAccess`
  - Emit object-related instructions
  - Handle default field values
  - Files: `src/CodeGen.hs`

- [ ] **29. Implement codegen for assignment**
  - Handle `EAssign` in `compileExpr`
  - Emit `IAssign` instruction
  - Files: `src/CodeGen.hs`

- [ ] **30. Implement codegen for return**
  - Handle `EReturn` in `compileExpr`
  - Respect `LanguageMode` (implicit vs explicit return)
  - Files: `src/CodeGen.hs`

---

## VM Execution

- [ ] **31. Implement VM execution for loops**
  - Handle `IWhile`, `IFor` in `executeInstr`
  - Files: `src/VM.hs`

- [ ] **32. Implement VM execution for tuples**
  - Handle `ITupleCreate`, `ITupleGet`
  - Support `VTuple` values
  - Files: `src/VM.hs`

- [ ] **33. Implement VM execution for lists**
  - Handle `IListCreate`, `IListGet`, `IListSet`
  - Support `VList` values with bounds checking
  - Files: `src/VM.hs`

- [ ] **34. Implement VM execution for objects**
  - Handle `ICreateObject`, `IGetMember`, `ISetMember`
  - Support `VObject` values
  - Files: `src/VM.hs`

- [ ] **35. Implement VM execution for assignment**
  - Handle `IAssign` instruction
  - Update local slots
  - Files: `src/VM.hs`

- [ ] **36. Implement VM execution for ranges**
  - Handle `IRangeCreate` instruction
  - Files: `src/VM.hs`

---

## Closure Conversion

- [ ] **37. Adapt closure conversion for methods**
  - Auto-capture `self` in method bodies
  - Add `self` to free variables for methods
  - Files: `src/ClosureConversion.hs`

---

## Builtins

- [ ] **38. Add Topineur-specific builtins**
  - Add: `println`, `show` (type-to-string conversion)
  - Add: `length`, `head`, `tail` (if not already present)
  - Add: `map`, `filter`, `fold` for lists
  - Add: `range` function
  - Add: `float`, `int` (type conversions)
  - Add: `sqrt`, `abs` (math functions)
  - Files: `src/Builtins.hs`

---

## Type Checker

- [ ] **39. Create `src/TypeChecker.hs` module**
  - Define: `type TypeEnv = Map Name Type`
  - Implement: `typeCheck :: TypeEnv -> Expr -> Either CompileError Type`
  - Validate type annotations
  - Check function call compatibility
  - Files: `src/TypeChecker.hs` (new file)

- [ ] **40. Integrate type checker in compiler**
  - Call type checker before codegen for Topineur mode
  - Files: `src/Compiler.hs`

---

## Compiler Configuration

- [ ] **41. Add `LanguageMode` to `CompilerConfig`**
  - Add: `data LanguageMode = Lisp | Topineur`
  - Add field to `CompilerConfig`
  - Files: `src/Compiler.hs`

- [ ] **42. Adapt compilation based on mode**
  - Lisp: implicit return, immutable
  - Topineur: explicit `top`, mutable, type checking
  - Files: `src/Compiler.hs`, `src/Desugar.hs`

---

## Desugar & Compatibility

- [ ] **43. Maintain Lisp compatibility in `Desugar.hs`**
  - Ensure existing `sexprToExpr` still works
  - Files: `src/Desugar.hs`

- [ ] **44. Update `Show` instances**
  - Add `Show` instances for new AST constructors
  - Files: `src/AST.hs`

- [ ] **45. Update `Eq` instances**
  - Add `Eq` instances for new types (Type, BinOp, UnOp, etc.)
  - Files: `src/AST.hs`

---

## Testing

- [ ] **46. Ensure all existing Lisp tests pass**
  - Run: `stack test`
  - Verify no regressions
  - Files: `test/Test/*Spec.hs`

- [ ] **47. Create basic Topineur unit tests**
  - Create: `test/Test/TopineurSpec.hs`
  - Test: types, tuples, lists, loops, objects, operators
  - Files: `test/Test/TopineurSpec.hs` (new file)

---

## Files Modified Summary

**Core AST & Data Types:**
- `src/AST.hs` — all new types, expressions, instructions, values

**Compilation Pipeline:**
- `src/Compiler.hs` — language mode, type checker integration
- `src/Desugar.hs` — compatibility layer
- `src/CodeGen.hs` — all new expression codegen
- `src/ClosureConversion.hs` — method closure handling

**Runtime:**
- `src/VM.hs` — all new instruction execution
- `src/Builtins.hs` — Topineur builtins

**Type System:**
- `src/TypeChecker.hs` — new module for type checking

**Testing:**
- `test/Test/TopineurSpec.hs` — new test file

---

## Implementation Strategy

**Priority 1** (Foundation):
- Tasks 1-3, 18, 41-43, 46

**Priority 2** (Basic Features):
- Tasks 4-12, 19-23, 24-30

**Priority 3** (Advanced Features):
- Tasks 13-17, 31-37

**Priority 4** (Polish):
- Tasks 38-40, 44-45, 47

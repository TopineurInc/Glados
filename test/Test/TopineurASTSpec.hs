{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Test.TopineurASTSpec (tests) where

import Test.HUnit

import AST
import AlphaRename
import ClosureConversion

-- Tests for all new Topineur AST features (Phases 1-23)

tests :: Test
tests = TestList
  [ TestLabel "Phase 1: Type System" testTypeSystem
  , TestLabel "Phase 2: Control Flow" testControlFlow
  , TestLabel "Phase 3: Operators" testOperators
  , TestLabel "Phase 4: Data Structures" testDataStructures
  , TestLabel "Phase 5: Mutability" testMutability
  , TestLabel "Phase 6: Objects" testObjects
  , TestLabel "Phase 7: Annotations & Modules" testAnnotationsModules
  , TestLabel "Phase 8: Runtime Values" testRuntimeValues
  , TestLabel "Phase 9: Bytecode Instructions" testBytecodeInstructions
  ]

-- ======================
-- Phase 1: Type System
-- ======================

testTypeSystem :: Test
testTypeSystem = TestList
  [ "type equality TInt" ~: TInt ~?= TInt
  , "type equality TFloat" ~: TFloat ~?= TFloat
  , "type equality TBool" ~: TBool ~?= TBool
  , "type equality TString" ~: TString ~?= TString
  , "type equality TUnit" ~: TUnit ~?= TUnit
  , "type equality TList" ~: TList TInt ~?= TList TInt
  , "type inequality TList different inner" ~: TList TInt /= TList TFloat ~? "TList types should differ"
  , "type equality TTuple" ~: TTuple [TInt, TString] ~?= TTuple [TInt, TString]
  , "type equality TObject" ~: TObject "Person" ~?= TObject "Person"
  , "type equality TFun" ~: TFun [TInt, TInt] TInt ~?= TFun [TInt, TInt] TInt
  
  , "lambda with typed params" ~: case alphaRename (ELambda [("x", Just TInt), ("y", Just TInt)] (Just TInt) (EVar "x") []) of
      Right (ELambda [(x', Just TInt), (y', Just TInt)] (Just TInt) (EVar xVar) []) | x' == xVar -> True
      _ -> False
      ~? "Should preserve types during alpha rename"
  
  , "lambda without types (Lisp style)" ~: case alphaRename (ELambda [("x", Nothing)] Nothing (EVar "x") []) of
      Right (ELambda [(x', Nothing)] Nothing (EVar xVar) []) | x' == xVar -> True
      _ -> False
      ~? "Should handle untyped lambdas"
  
  , "EUnit value" ~: alphaRename EUnit ~?= Right EUnit
  , "define with annotations" ~: case alphaRename (EDefine "f" (EInt 42) [Cache]) of
      Right (EDefine _ (EInt 42) [Cache]) -> True
      _ -> False
      ~? "Should preserve annotations"
  ]

-- ======================
-- Phase 2: Control Flow
-- ======================

testControlFlow :: Test
testControlFlow = TestList
  [ "EWhile simple" ~: alphaRename (EWhile (EBool True) (EInt 1)) ~?= Right (EWhile (EBool True) (EInt 1))
  
  , "EWhile with variable" ~: case alphaRename (ELambda [("x", Nothing)] Nothing (EWhile (EVar "x") (EVar "x")) []) of
      Right (ELambda [(x', Nothing)] Nothing (EWhile (EVar xCond) (EVar xBody)) []) | x' == xCond && x' == xBody -> True
      _ -> False
      ~? "Should rename vars in while"
  
  , "EFor basic" ~: case alphaRename (EFor "i" (EInt 0) (EInt 10) (EVar "i")) of
      Right (EFor i' (EInt 0) (EInt 10) (EVar iBody)) | i' == iBody && i' /= "i" -> True
      _ -> False
      ~? "Should rename loop variable"
  
  , "EFor nested" ~: case alphaRename (EFor "i" (EInt 0) (EInt 10) (EFor "j" (EInt 0) (EInt 5) (EVar "j"))) of
      Right (EFor i' _ _ (EFor j' _ _ (EVar jBody))) | i' /= "i" && j' /= "j" && j' == jBody && i' /= j' -> True
      _ -> False
      ~? "Should rename nested loop variables"
  
  , "ERange simple" ~: alphaRename (ERange (EInt 0) (EInt 10)) ~?= Right (ERange (EInt 0) (EInt 10))
  
  , "ERange with vars" ~: case alphaRename (ELambda [("start", Nothing), ("end", Nothing)] Nothing (ERange (EVar "start") (EVar "end")) []) of
      Right (ELambda [(s', Nothing), (e', Nothing)] Nothing (ERange (EVar sVar) (EVar eVar)) []) | s' == sVar && e' == eVar -> True
      _ -> False
      ~? "Should rename range variables"
  
  , "EReturn simple" ~: alphaRename (EReturn (EInt 42)) ~?= Right (EReturn (EInt 42))
  
  , "EReturn with var" ~: case alphaRename (ELambda [("x", Nothing)] Nothing (EReturn (EVar "x")) []) of
      Right (ELambda [(x', Nothing)] Nothing (EReturn (EVar xVar)) []) | x' == xVar -> True
      _ -> False
      ~? "Should rename return value"
  
  , "closure conversion while" ~: closureConvert (EWhile (EBool True) (EInt 1)) ~?= Right (EWhile (EBool True) (EInt 1))
  
  , "closure conversion for" ~: closureConvert (EFor "i" (EInt 0) (EInt 10) (EVar "i")) ~?= Right (EFor "i" (EInt 0) (EInt 10) (EVar "i"))
  ]

-- ======================
-- Phase 3: Operators
-- ======================

testOperators :: Test
testOperators = TestList
  [ "BinOp Add" ~: alphaRename (EBinOp Add (EInt 1) (EInt 2)) ~?= Right (EBinOp Add (EInt 1) (EInt 2))
  , "BinOp Sub" ~: alphaRename (EBinOp Sub (EInt 5) (EInt 3)) ~?= Right (EBinOp Sub (EInt 5) (EInt 3))
  , "BinOp Mul" ~: alphaRename (EBinOp Mul (EInt 3) (EInt 4)) ~?= Right (EBinOp Mul (EInt 3) (EInt 4))
  , "BinOp Div" ~: alphaRename (EBinOp Div (EInt 10) (EInt 2)) ~?= Right (EBinOp Div (EInt 10) (EInt 2))
  , "BinOp Eq" ~: alphaRename (EBinOp Eq (EInt 1) (EInt 1)) ~?= Right (EBinOp Eq (EInt 1) (EInt 1))
  , "BinOp Neq" ~: alphaRename (EBinOp Neq (EInt 1) (EInt 2)) ~?= Right (EBinOp Neq (EInt 1) (EInt 2))
  , "BinOp Lt" ~: alphaRename (EBinOp Lt (EInt 1) (EInt 2)) ~?= Right (EBinOp Lt (EInt 1) (EInt 2))
  , "BinOp Lte" ~: alphaRename (EBinOp Lte (EInt 1) (EInt 2)) ~?= Right (EBinOp Lte (EInt 1) (EInt 2))
  , "BinOp Gt" ~: alphaRename (EBinOp Gt (EInt 2) (EInt 1)) ~?= Right (EBinOp Gt (EInt 2) (EInt 1))
  , "BinOp Gte" ~: alphaRename (EBinOp Gte (EInt 2) (EInt 1)) ~?= Right (EBinOp Gte (EInt 2) (EInt 1))
  , "BinOp And" ~: alphaRename (EBinOp And (EBool True) (EBool False)) ~?= Right (EBinOp And (EBool True) (EBool False))
  , "BinOp Or" ~: alphaRename (EBinOp Or (EBool True) (EBool False)) ~?= Right (EBinOp Or (EBool True) (EBool False))
  , "BinOp Concat" ~: alphaRename (EBinOp Concat (EString "Hello") (EString "World")) ~?= Right (EBinOp Concat (EString "Hello") (EString "World"))
  
  , "BinOp with variables" ~: case alphaRename (ELambda [("x", Nothing), ("y", Nothing)] Nothing (EBinOp Add (EVar "x") (EVar "y")) []) of
      Right (ELambda [(x', Nothing), (y', Nothing)] Nothing (EBinOp Add (EVar xVar) (EVar yVar)) []) | x' == xVar && y' == yVar -> True
      _ -> False
      ~? "Should rename binop operands"
  
  , "UnOp Neg" ~: alphaRename (EUnOp Neg (EInt 5)) ~?= Right (EUnOp Neg (EInt 5))
  , "UnOp Not" ~: alphaRename (EUnOp Not (EBool True)) ~?= Right (EUnOp Not (EBool True))
  
  , "UnOp with variable" ~: case alphaRename (ELambda [("x", Nothing)] Nothing (EUnOp Neg (EVar "x")) []) of
      Right (ELambda [(x', Nothing)] Nothing (EUnOp Neg (EVar xVar)) []) | x' == xVar -> True
      _ -> False
      ~? "Should rename unop operand"
  
  , "nested operators" ~: case alphaRename (EBinOp Add (EBinOp Mul (EInt 2) (EInt 3)) (EInt 4)) of
      Right (EBinOp Add (EBinOp Mul (EInt 2) (EInt 3)) (EInt 4)) -> True
      _ -> False
      ~? "Should handle nested operators"
  
  , "closure conversion binop" ~: closureConvert (EBinOp Add (EInt 1) (EInt 2)) ~?= Right (EBinOp Add (EInt 1) (EInt 2))
  , "closure conversion unop" ~: closureConvert (EUnOp Neg (EInt 5)) ~?= Right (EUnOp Neg (EInt 5))
  ]

-- ======================
-- Phase 4: Data Structures
-- ======================

testDataStructures :: Test
testDataStructures = TestList
  [ "ETuple empty" ~: alphaRename (ETuple []) ~?= Right (ETuple [])
  , "ETuple single" ~: alphaRename (ETuple [EInt 1]) ~?= Right (ETuple [EInt 1])
  , "ETuple multiple" ~: alphaRename (ETuple [EInt 1, EString "hello", EBool True]) ~?= Right (ETuple [EInt 1, EString "hello", EBool True])
  
  , "ETuple with variables" ~: case alphaRename (ELambda [("x", Nothing), ("y", Nothing)] Nothing (ETuple [EVar "x", EVar "y"]) []) of
      Right (ELambda [(x', Nothing), (y', Nothing)] Nothing (ETuple [EVar xVar, EVar yVar]) []) | x' == xVar && y' == yVar -> True
      _ -> False
      ~? "Should rename tuple elements"
  
  , "ETupleDestruct simple" ~: case alphaRename (ETupleDestruct ["a", "b"] (ETuple [EInt 1, EInt 2]) (EVar "a")) of
      Right (ETupleDestruct [a', b'] (ETuple [EInt 1, EInt 2]) (EVar aVar)) | a' == aVar && a' /= "a" && b' /= "b" -> True
      _ -> False
      ~? "Should rename destructured names"
  
  , "ETupleDestruct nested" ~: case alphaRename (ETupleDestruct ["x", "y"] (ETuple [EInt 1, EInt 2]) (ETupleDestruct ["a", "b"] (ETuple [EVar "x", EVar "y"]) (EVar "a"))) of
      Right _ -> True
      _ -> False
      ~? "Should handle nested tuple destructuring"
  
  , "EListLiteral empty" ~: alphaRename (EListLiteral [] Nothing) ~?= Right (EListLiteral [] Nothing)
  , "EListLiteral typed" ~: alphaRename (EListLiteral [EInt 1, EInt 2, EInt 3] (Just (TList TInt))) ~?= Right (EListLiteral [EInt 1, EInt 2, EInt 3] (Just (TList TInt)))
  , "EListLiteral untyped" ~: alphaRename (EListLiteral [EInt 1, EInt 2] Nothing) ~?= Right (EListLiteral [EInt 1, EInt 2] Nothing)
  
  , "EIndex simple" ~: alphaRename (EIndex (EListLiteral [EInt 1, EInt 2] Nothing) (EInt 0)) ~?= Right (EIndex (EListLiteral [EInt 1, EInt 2] Nothing) (EInt 0))
  
  , "EIndex with variables" ~: case alphaRename (ELambda [("list", Nothing), ("idx", Nothing)] Nothing (EIndex (EVar "list") (EVar "idx")) []) of
      Right (ELambda [(l', Nothing), (i', Nothing)] Nothing (EIndex (EVar lVar) (EVar iVar)) []) | l' == lVar && i' == iVar -> True
      _ -> False
      ~? "Should rename index operands"
  
  , "closure conversion tuple" ~: closureConvert (ETuple [EInt 1, EInt 2]) ~?= Right (ETuple [EInt 1, EInt 2])
  , "closure conversion list literal" ~: closureConvert (EListLiteral [EInt 1, EInt 2] Nothing) ~?= Right (EListLiteral [EInt 1, EInt 2] Nothing)
  ]

-- ======================
-- Phase 5: Mutability
-- ======================

testMutability :: Test
testMutability = TestList
  [ "EAssign simple" ~: case alphaRename (EAssign "x" (EInt 42)) of
      Right (EAssign "x" (EInt 42)) -> True  -- Free variable, not renamed
      _ -> False
      ~? "Should handle assignment to free variable"
  
  , "EAssign to bound variable" ~: case alphaRename (ELambda [("x", Nothing)] Nothing (EAssign "x" (EInt 42)) []) of
      Right (ELambda [(x', Nothing)] Nothing (EAssign xAssign (EInt 42)) []) | x' == xAssign && x' /= "x" -> True
      _ -> False
      ~? "Should rename assigned bound variable"
  
  , "EAssign with expression" ~: case alphaRename (ELambda [("x", Nothing), ("y", Nothing)] Nothing (EAssign "x" (EBinOp Add (EVar "y") (EInt 1))) []) of
      Right (ELambda [(x', Nothing), (y', Nothing)] Nothing (EAssign xAssign (EBinOp Add (EVar yVar) (EInt 1))) []) | x' == xAssign && y' == yVar -> True
      _ -> False
      ~? "Should rename assignment with complex expression"
  
  , "closure conversion assign" ~: closureConvert (EAssign "x" (EInt 42)) ~?= Right (EAssign "x" (EInt 42))
  ]

-- ======================
-- Phase 6: Objects
-- ======================

testObjects :: Test
testObjects = TestList
  [ "EObjectDecl simple" ~: case alphaRename (EObjectDecl "Person" [Field "name" TString Nothing, Field "age" TInt Nothing] []) of
      Right (EObjectDecl newName [Field fName TString Nothing, Field fAge TInt Nothing] []) | newName /= "Person" -> True
      _ -> False
      ~? "Should rename object type name"
  
  , "EObjectDecl with methods" ~: case alphaRename (EObjectDecl "Counter" [] [Method "increment" [("amount", Just TInt)] TInt (EVar "amount")]) of
      Right (EObjectDecl _ [] [Method _ [(amt', Just TInt)] TInt (EVar amtVar)]) | amt' == amtVar -> True
      _ -> False
      ~? "Should rename method parameters"
  
  , "EObjectDecl with default values" ~: case alphaRename (EObjectDecl "Config" [Field "timeout" TInt (Just (EInt 30))] []) of
      Right (EObjectDecl _ [Field _ TInt (Just (EInt 30))] []) -> True
      _ -> False
      ~? "Should preserve default field values"
  
  , "EObjectInst simple" ~: case alphaRename (EObjectInst "Person" [("name", EString "Alice"), ("age", EInt 30)]) of
      Right (EObjectInst "Person" [("name", EString "Alice"), ("age", EInt 30)]) -> True  -- Free variable
      _ -> False
      ~? "Should handle object instantiation"
  
  , "EMemberAccess simple" ~: case alphaRename (EMemberAccess (EVar "person") "name") of
      Right (EMemberAccess (EVar "person") "name") -> True
      _ -> False
      ~? "Should handle member access"
  
  , "EMemberAccess chained" ~: case alphaRename (EMemberAccess (EMemberAccess (EVar "obj") "field1") "field2") of
      Right (EMemberAccess (EMemberAccess (EVar "obj") "field1") "field2") -> True
      _ -> False
      ~? "Should handle chained member access"
  
  , "closure conversion object decl" ~: closureConvert (EObjectDecl "Point" [Field "x" TInt Nothing, Field "y" TInt Nothing] []) ~?= Right (EObjectDecl "Point" [Field "x" TInt Nothing, Field "y" TInt Nothing] [])
  , "closure conversion object inst" ~: closureConvert (EObjectInst "Point" [("x", EInt 1), ("y", EInt 2)]) ~?= Right (EObjectInst "Point" [("x", EInt 1), ("y", EInt 2)])
  ]

-- ======================
-- Phase 7: Annotations & Modules
-- ======================

testAnnotationsModules :: Test
testAnnotationsModules = TestList
  [ "Annotation Cache" ~: Cache ~?= Cache
  , "Annotation Custom" ~: Custom "memoize" ~?= Custom "memoize"
  , "Annotation inequality" ~: Cache /= Custom "test" ~? "Annotations should differ"
  
  , "lambda with Cache annotation" ~: case alphaRename (ELambda [("x", Nothing)] Nothing (EVar "x") [Cache]) of
      Right (ELambda _ Nothing _ [Cache]) -> True
      _ -> False
      ~? "Should preserve Cache annotation"
  
  , "lambda with multiple annotations" ~: case alphaRename (ELambda [("x", Nothing)] Nothing (EVar "x") [Cache, Custom "inline"]) of
      Right (ELambda _ Nothing _ [Cache, Custom "inline"]) -> True
      _ -> False
      ~? "Should preserve multiple annotations"
  
  , "define with annotations" ~: case alphaRename (EDefine "factorial" (EInt 1) [Cache, Custom "tail-recursive"]) of
      Right (EDefine _ (EInt 1) [Cache, Custom "tail-recursive"]) -> True
      _ -> False
      ~? "Should preserve define annotations"
  
  , "EPackage" ~: alphaRename (EPackage "math.utils") ~?= Right (EPackage "math.utils")
  , "EImport" ~: alphaRename (EImport "std.io") ~?= Right (EImport "std.io")
  
  , "closure conversion package" ~: closureConvert (EPackage "test") ~?= Right (EPackage "test")
  , "closure conversion import" ~: closureConvert (EImport "test") ~?= Right (EImport "test")
  ]

-- ======================
-- Phase 8: Runtime Values
-- ======================

testRuntimeValues :: Test
testRuntimeValues = TestList
  [ "VUnit equality" ~: VUnit ~?= VUnit
  , "VUnit show" ~: show VUnit ~?= "#<void>"
  
  -- VList, VTuple, VObject don't have Eq instances, so we test show instead
  , "VList empty show" ~: show (VList []) ~?= "[]"
  , "VList integers show" ~: show (VList [VInt 1, VInt 2, VInt 3]) ~?= "[1 2 3]"
  , "VList show" ~: show (VList [VInt 1, VInt 2]) ~?= "[1 2]"
  
  , "VTuple empty show" ~: show (VTuple []) ~?= "()"
  , "VTuple mixed show" ~: show (VTuple [VInt 1, VString "hello", VBool True]) ~?= "(1 \"hello\" True)"
  , "VTuple pair show" ~: show (VTuple [VInt 1, VString "hello"]) ~?= "(1 \"hello\")"
  
  , "VObject show" ~: show (VObject "Person" [("name", VString "Alice")]) ~?= "VObject Person [(\"name\",\"Alice\")]"
  , "VObject fields show" ~: show (VObject "Point" [("x", VInt 1), ("y", VInt 2)]) ~?= "VObject Point [(\"x\",1),(\"y\",2)]"
  ]

-- ======================
-- Phase 9: Bytecode Instructions
-- ======================

testBytecodeInstructions :: Test
testBytecodeInstructions = TestList
  [ "IWhile equality" ~: IWhile ~?= IWhile
  , "IFor equality" ~: IFor ~?= IFor
  , "IBreak equality" ~: IBreak ~?= IBreak
  , "IContinue equality" ~: IContinue ~?= IContinue
  
  , "ITupleCreate" ~: ITupleCreate 3 ~?= ITupleCreate 3
  , "ITupleGet" ~: ITupleGet 0 ~?= ITupleGet 0
  
  , "IListCreate" ~: IListCreate 5 ~?= IListCreate 5
  , "IListGet equality" ~: IListGet ~?= IListGet
  , "IListSet equality" ~: IListSet ~?= IListSet
  
  , "IObjectCreate" ~: IObjectCreate "Person" ~?= IObjectCreate "Person"
  , "IMemberGet" ~: IMemberGet "name" ~?= IMemberGet "name"
  , "IMemberSet" ~: IMemberSet "age" ~?= IMemberSet "age"
  
  , "IAssign" ~: IAssign 0 ~?= IAssign 0
  , "IRangeCreate equality" ~: IRangeCreate ~?= IRangeCreate
  
  , "instruction show" ~: show IWhile ~?= "IWhile"
  , "instruction equality different" ~: IWhile /= IFor ~? "Different instructions should not be equal"
  ]

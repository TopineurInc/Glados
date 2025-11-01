{-# LANGUAGE PatternSynonyms #-}

module Test.CodeGenTopineurSpec (tests) where

import Test.HUnit

import AST
import CodeGen
import qualified Data.Map as Map
import qualified Data.Vector as Vector

-- ======================
-- Task 24: Loops
-- ======================

testCodeGenLoops :: Test
testCodeGenLoops = TestList
  [ "gen while loop" ~: case generateCode "test" (EWhile (EBool True) (EInt 42)) of
      Right code -> any (\instr -> case instr of IJumpIfFalse _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IJumpIfFalse for while"
  
  , "gen while with jump back" ~: case generateCode "test" (EWhile (EBool True) (EInt 1)) of
      Right code -> any (\instr -> case instr of IJump _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IJump back to loop start"
  
  , "gen for loop" ~: case generateCode "test" (EFor "i" (EInt 0) (EInt 10) (EInt 1)) of
      Right code -> any (\instr -> case instr of IStore _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IStore for iterator"
  
  , "gen for with condition" ~: case generateCode "test" (EFor "i" (EInt 0) (EInt 10) (EVar "i")) of
      Right code -> any (\instr -> case instr of IPrim "<" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate < comparison for for loop"
  
  , "gen for with increment" ~: case generateCode "test" (EFor "i" (EInt 0) (EInt 10) (EInt 1)) of
      Right code -> any (\instr -> case instr of IPrim "+" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate + for increment"
  
  , "gen range" ~: case generateCode "test" (ERange (EInt 0) (EInt 10)) of
      Right code -> any (\instr -> case instr of IRangeCreate -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IRangeCreate"
  ]

-- ======================
-- Task 25: Operators
-- ======================

testCodeGenOperators :: Test
testCodeGenOperators = TestList
  [ "gen BinOp Add" ~: case generateCode "test" (EBinOp Add (EInt 1) (EInt 2)) of
      Right code -> any (\instr -> case instr of IPrim "+" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim + for Add"
  
  , "gen BinOp Sub" ~: case generateCode "test" (EBinOp Sub (EInt 5) (EInt 3)) of
      Right code -> any (\instr -> case instr of IPrim "-" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim - for Sub"
  
  , "gen BinOp Mul" ~: case generateCode "test" (EBinOp Mul (EInt 3) (EInt 4)) of
      Right code -> any (\instr -> case instr of IPrim "*" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim * for Mul"
  
  , "gen BinOp Div" ~: case generateCode "test" (EBinOp Div (EInt 10) (EInt 2)) of
      Right code -> any (\instr -> case instr of IPrim "div" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim div for Div"
  
  , "gen BinOp Mod" ~: case generateCode "test" (EBinOp Mod (EInt 10) (EInt 3)) of
      Right code -> any (\instr -> case instr of IPrim "mod" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim mod for Mod"
  
  , "gen BinOp Lt" ~: case generateCode "test" (EBinOp Lt (EInt 1) (EInt 2)) of
      Right code -> any (\instr -> case instr of IPrim "<" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim < for Lt"
  
  , "gen BinOp Lte" ~: case generateCode "test" (EBinOp Lte (EInt 1) (EInt 2)) of
      Right code -> any (\instr -> case instr of IPrim "<=" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim <= for Lte"
  
  , "gen BinOp Gt" ~: case generateCode "test" (EBinOp Gt (EInt 2) (EInt 1)) of
      Right code -> any (\instr -> case instr of IPrim ">" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim > for Gt"
  
  , "gen BinOp Gte" ~: case generateCode "test" (EBinOp Gte (EInt 2) (EInt 1)) of
      Right code -> any (\instr -> case instr of IPrim ">=" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim >= for Gte"
  
  , "gen BinOp Eq" ~: case generateCode "test" (EBinOp Eq (EInt 1) (EInt 1)) of
      Right code -> any (\instr -> case instr of IPrim "eq?" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim eq? for Eq"
  
  , "gen BinOp Neq" ~: case generateCode "test" (EBinOp Neq (EInt 1) (EInt 2)) of
      Right code -> any (\instr -> case instr of IPrim "not" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim not for Neq"
  
  , "gen BinOp And" ~: case generateCode "test" (EBinOp And (EBool True) (EBool False)) of
      Right code -> any (\instr -> case instr of IPrim "and" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim and for And"
  
  , "gen BinOp Or" ~: case generateCode "test" (EBinOp Or (EBool True) (EBool False)) of
      Right code -> any (\instr -> case instr of IPrim "or" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim or for Or"
  
  , "gen BinOp Concat" ~: case generateCode "test" (EBinOp Concat (EString "hello") (EString "world")) of
      Right code -> any (\instr -> case instr of IPrim "string-append" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim string-append for Concat"
  
  , "gen UnOp Not" ~: case generateCode "test" (EUnOp Not (EBool True)) of
      Right code -> any (\instr -> case instr of IPrim "not" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim not for Not"
  
  , "gen UnOp Neg" ~: case generateCode "test" (EUnOp Neg (EInt 5)) of
      Right code -> any (\instr -> case instr of IPrim "-" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim - for Neg"
  
  , "gen complex nested operators" ~: case generateCode "test" (EBinOp Add (EBinOp Mul (EInt 2) (EInt 3)) (EInt 4)) of
      Right code -> length (filter (\instr -> case instr of IPrim _ -> True; _ -> False) (Vector.toList $ coInstrs code)) >= 2
      Left _ -> False
      ~? "Should handle nested operators"
  
  , "gen modulo for isEven check" ~: case generateCode "test" (EBinOp Eq (EBinOp Mod (EVar "n") (EInt 2)) (EInt 0)) of
      Right code -> any (\instr -> case instr of IPrim "mod" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate mod for n % 2 == 0 pattern"
  ]

-- ======================
-- Task 26: Tuples
-- ======================

testCodeGenTuples :: Test
testCodeGenTuples = TestList
  [ "gen ETuple empty" ~: case generateCode "test" (ETuple []) of
      Right code -> any (\instr -> case instr of ITupleCreate 0 -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate ITupleCreate 0"
  
  , "gen ETuple with elements" ~: case generateCode "test" (ETuple [EInt 1, EInt 2, EInt 3]) of
      Right code -> any (\instr -> case instr of ITupleCreate 3 -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate ITupleCreate 3"
  
  , "gen ETupleDestruct" ~: case generateCode "test" (ETupleDestruct ["x", "y"] (ETuple [EInt 1, EInt 2]) (EVar "x")) of
      Right code -> any (\instr -> case instr of ITupleGet 0 -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate ITupleGet for destructuring"
  
  , "gen EIndex with int" ~: case generateCode "test" (EIndex (ETuple [EInt 1, EInt 2]) (EInt 0)) of
      Right code -> any (\instr -> case instr of ITupleGet 0 -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate ITupleGet with index"
  
  , "gen nested tuple" ~: case generateCode "test" (ETuple [ETuple [EInt 1, EInt 2], EInt 3]) of
      Right code -> length (filter (\instr -> case instr of ITupleCreate _ -> True; _ -> False) (Vector.toList $ coInstrs code)) >= 2
      Left _ -> False
      ~? "Should generate multiple ITupleCreate"
  
  , "gen ETupleDestruct with IStore" ~: case generateCode "test" (ETupleDestruct ["x", "y", "z"] (ETuple [EInt 1, EInt 2, EInt 3]) (EVar "x")) of
      Right code -> length (filter (\instr -> case instr of IStore _ -> True; _ -> False) (Vector.toList $ coInstrs code)) >= 3
      Left _ -> False
      ~? "Should generate IStore for each destructured variable"
  ]

-- ======================
-- Task 27: Native Lists
-- ======================

testCodeGenNativeLists :: Test
testCodeGenNativeLists = TestList
  [ "gen EListLiteral empty" ~: case generateCode "test" (EListLiteral [] Nothing) of
      Right code -> any (\instr -> case instr of IListCreate 0 -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IListCreate 0"
  
  , "gen EListLiteral with elements" ~: case generateCode "test" (EListLiteral [EInt 1, EInt 2, EInt 3] Nothing) of
      Right code -> any (\instr -> case instr of IListCreate 3 -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IListCreate 3"
  
  , "gen EListLiteral typed" ~: case generateCode "test" (EListLiteral [EInt 1, EInt 2] (Just TInt)) of
      Right code -> any (\instr -> case instr of IListCreate 2 -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IListCreate with type"
  
  , "gen EIndex dynamic" ~: case generateCode "test" (EIndex (EListLiteral [EInt 1, EInt 2] Nothing) (EVar "i")) of
      Right code -> any (\instr -> case instr of IListGet -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IListGet"
  
  , "gen nested list" ~: case generateCode "test" (EListLiteral [EListLiteral [EInt 1] Nothing, EListLiteral [EInt 2] Nothing] Nothing) of
      Right code -> length (filter (\instr -> case instr of IListCreate _ -> True; _ -> False) (Vector.toList $ coInstrs code)) >= 2
      Left _ -> False
      ~? "Should generate multiple IListCreate"
  ]

-- ======================
-- Task 28: Objects
-- ======================

testCodeGenObjects :: Test
testCodeGenObjects = TestList
  [ "gen EObjectDecl" ~: case generateCode "test" (EObjectDecl "Person" [Field "name" TString Nothing] []) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate code for object declaration"
  
  , "gen EObjectInst" ~: case generateCode "test" (EObjectInst "Person" [("name", EString "Alice")]) of
      Right code -> any (\instr -> case instr of IObjectCreate "Person" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IObjectCreate"
  
  , "gen EObjectInst with multiple fields" ~: case generateCode "test" (EObjectInst "Point" [("x", EInt 1), ("y", EInt 2)]) of
      Right code -> any (\instr -> case instr of IObjectCreate "Point" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IObjectCreate with fields"
  
  , "gen EMemberAccess" ~: case generateCode "test" (EMemberAccess (EVar "obj") "name") of
      Right code -> any (\instr -> case instr of IMemberGet "name" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IMemberGet"
  
  , "gen nested member access" ~: case generateCode "test" (EMemberAccess (EMemberAccess (EVar "obj") "field1") "field2") of
      Right code -> length (filter (\instr -> case instr of IMemberGet _ -> True; _ -> False) (Vector.toList $ coInstrs code)) >= 2
      Left _ -> False
      ~? "Should generate multiple IMemberGet"
  ]

-- ======================
-- Task 29: Assignment
-- ======================

testCodeGenAssignment :: Test
testCodeGenAssignment = TestList
  [ "gen EAssign to defined var" ~: case generateCode "test" (EList [EDefine "x" (EInt 1) [], EAssign "x" (EInt 2)]) of
      Right code -> any (\instr -> case instr of IAssign _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IAssign"
  
  , "gen EAssign with expression" ~: case generateCode "test" (EList [EDefine "x" (EInt 1) [], EAssign "x" (EBinOp Add (EInt 2) (EInt 3))]) of
      Right code -> any (\instr -> case instr of IAssign _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should compile expression before assign"
  
  , "gen multiple assignments" ~: case generateCode "test" (EList [EDefine "x" (EInt 1) [], EAssign "x" (EInt 2), EAssign "x" (EInt 3)]) of
      Right code -> length (filter (\instr -> case instr of IAssign _ -> True; _ -> False) (Vector.toList $ coInstrs code)) >= 2
      Left _ -> False
      ~? "Should generate multiple IAssign"
  
  , "gen assignment pushes value back" ~: case generateCode "test" (EList [EDefine "x" (EInt 1) [], EAssign "x" (EInt 2)]) of
      Right code -> any (\instr -> case instr of ILoad _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should load variable after assignment"
  ]

-- ======================
-- Task 30: Return
-- ======================

testCodeGenReturn :: Test
testCodeGenReturn = TestList
  [ "gen EReturn simple" ~: case generateCode "test" (EReturn (EInt 42)) of
      Right code -> any (\instr -> case instr of IReturn -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IReturn"
  
  , "gen EReturn with expression" ~: case generateCode "test" (EReturn (EBinOp Add (EInt 1) (EInt 2))) of
      Right code -> any (\instr -> case instr of IReturn -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should compile expression before return"
  
  , "gen EReturn in lambda" ~: case generateCodeWithDefs "test" (EDefine "f" (ELambda [("x", Nothing)] Nothing (EReturn (EVar "x")) []) []) of
      (Right _, codeObjs) -> case Map.lookup "f" codeObjs of
        Just code -> length (filter (\instr -> case instr of IReturn -> True; _ -> False) (Vector.toList $ coInstrs code)) >= 2
        Nothing -> False
      _ -> False
      ~? "Should have explicit and implicit return"
  ]

-- ======================
-- Additional Edge Cases & Coverage
-- ======================

testCodeGenEdgeCases :: Test
testCodeGenEdgeCases = TestList
  [ "gen EPackage" ~: case generateCode "test" (EPackage "MyPackage") of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate code for package (no-op)"
  
  , "gen EImport" ~: case generateCode "test" (EImport "SomeModule") of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate code for import (no-op)"
  
  , "gen while loop with unit result" ~: case generateCode "test" (EWhile (EBool False) (EInt 1)) of
      Right code -> any (\instr -> case instr of IConst _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "While should push unit value as result"
  
  , "gen for loop with unit result" ~: case generateCode "test" (EFor "i" (EInt 0) (EInt 5) (EInt 1)) of
      Right code -> length (filter (\instr -> case instr of IConst _ -> True; _ -> False) (Vector.toList $ coInstrs code)) >= 2
      Left _ -> False
      ~? "For should push unit value as result"
  
  , "gen EObjectInst field order" ~: case generateCode "test" (EObjectInst "Point" [("x", EInt 10), ("y", EInt 20)]) of
      Right code -> any (\instr -> case instr of IObjectCreate "Point" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should compile field values in order before IObjectCreate"
  
  , "gen EIndex tuple vs list disambiguation" ~: case generateCode "test" (EIndex (EVar "collection") (EInt 5)) of
      Right code -> any (\instr -> case instr of ITupleGet 5 -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should use ITupleGet for constant integer index"
  ]

tests :: Test
tests = TestList
  [ TestLabel "CodeGen Loops (Task 24)" testCodeGenLoops
  , TestLabel "CodeGen Operators (Task 25)" testCodeGenOperators
  , TestLabel "CodeGen Tuples (Task 26)" testCodeGenTuples
  , TestLabel "CodeGen Native Lists (Task 27)" testCodeGenNativeLists
  , TestLabel "CodeGen Objects (Task 28)" testCodeGenObjects
  , TestLabel "CodeGen Assignment (Task 29)" testCodeGenAssignment
  , TestLabel "CodeGen Return (Task 30)" testCodeGenReturn
  , TestLabel "CodeGen Edge Cases" testCodeGenEdgeCases
  ]

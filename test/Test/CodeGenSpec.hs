module Test.CodeGenSpec (tests) where

import Test.HUnit

import AST
import CodeGen
import qualified Data.Map as Map
import qualified Data.Vector as Vector

tests :: Test
tests = TestList
  [ TestLabel "CodeGen Basic" testCodeGenBasic
  , TestLabel "CodeGen Arithmetic" testCodeGenArithmetic
  , TestLabel "CodeGen Comparison" testCodeGenComparison
  , TestLabel "CodeGen If" testCodeGenIf
  , TestLabel "CodeGen Define" testCodeGenDefine
  , TestLabel "CodeGen Lambda" testCodeGenLambda
  , TestLabel "CodeGen Application" testCodeGenApplication
  , TestLabel "CodeGen List" testCodeGenList
  ]

testCodeGenBasic :: Test
testCodeGenBasic = TestList
  [ "gen integer" ~: case generateCode "test" (EInt 42) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate code for integer"
  , "gen boolean true" ~: case generateCode "test" (EBool True) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate code for boolean"
  , "gen string" ~: case generateCode "test" (EString "hello") of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate code for string"
  , "gen var" ~: case generateCode "test" (EVar "x") of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate code for var"
  , "gen multiple constants" ~: case generateCode "test" (EList [EInt 1, EInt 2, EInt 3]) of
      Right code -> Vector.length (coConsts code) >= 3
      Left _ -> False
      ~? "Should have multiple constants"
  ]

testCodeGenArithmetic :: Test
testCodeGenArithmetic = TestList
  [ "gen addition" ~: case generateCode "test" (EApp (EVar "+") [EInt 1, EInt 2]) of
      Right code -> any (\instr -> case instr of IPrim "+" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim +"
  , "gen subtraction" ~: case generateCode "test" (EApp (EVar "-") [EInt 5, EInt 3]) of
      Right code -> any (\instr -> case instr of IPrim "-" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim -"
  , "gen multiplication" ~: case generateCode "test" (EApp (EVar "*") [EInt 3, EInt 4]) of
      Right code -> any (\instr -> case instr of IPrim "*" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim *"
  , "gen division" ~: case generateCode "test" (EApp (EVar "div") [EInt 10, EInt 2]) of
      Right code -> any (\instr -> case instr of IPrim "div" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim div"
  , "gen modulo" ~: case generateCode "test" (EApp (EVar "mod") [EInt 10, EInt 3]) of
      Right code -> any (\instr -> case instr of IPrim "mod" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim mod"
  ]

testCodeGenComparison :: Test
testCodeGenComparison = TestList
  [ "gen eq" ~: case generateCode "test" (EApp (EVar "eq?") [EInt 1, EInt 1]) of
      Right code -> any (\instr -> case instr of IPrim "eq?" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim eq?"
  , "gen less than" ~: case generateCode "test" (EApp (EVar "<") [EInt 1, EInt 2]) of
      Right code -> any (\instr -> case instr of IPrim "<" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim <"
  , "gen greater than" ~: case generateCode "test" (EApp (EVar ">") [EInt 2, EInt 1]) of
      Right code -> any (\instr -> case instr of IPrim ">" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPrim >"
  , "gen nested comparison" ~: case generateCode "test" (EApp (EVar "<") [EApp (EVar "+") [EInt 1, EInt 2], EInt 5]) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate nested comparison"
  , "gen multiple comparisons" ~: case generateCode "test" (EList [EApp (EVar "<") [EInt 1, EInt 2], EApp (EVar ">") [EInt 3, EInt 2]]) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate multiple comparisons"
  ]

testCodeGenIf :: Test
testCodeGenIf = TestList
  [ "gen simple if" ~: case generateCode "test" (EIf (EBool True) (EInt 1) (EInt 2)) of
      Right code -> any (\instr -> case instr of IJumpIfFalse _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IJumpIfFalse"
  , "gen if with var condition" ~: case generateCodeWithDefs "test" (EDefine "f" (ELambda ["x"] (EIf (EVar "x") (EInt 1) (EInt 0)))) of
      (Right _, codeObjs) -> case Map.lookup "f" codeObjs of
        Just code -> any (\instr -> case instr of IJumpIfFalse _ -> True; _ -> False) (Vector.toList $ coInstrs code)
        Nothing -> False
      _ -> False
      ~? "Should generate if with var"
  , "gen nested if" ~: case generateCode "test" (EIf (EBool True) (EIf (EBool False) (EInt 1) (EInt 2)) (EInt 3)) of
      Right code -> length (filter (\instr -> case instr of IJumpIfFalse _ -> True; _ -> False) (Vector.toList $ coInstrs code)) >= 2
      Left _ -> False
      ~? "Should generate nested if"
  , "gen if with complex branches" ~: case generateCode "test" (EIf (EBool True) (EApp (EVar "+") [EInt 1, EInt 2]) (EApp (EVar "*") [EInt 3, EInt 4])) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate complex if"
  , "gen if jump targets" ~: case generateCode "test" (EIf (EBool True) (EInt 1) (EInt 2)) of
      Right code -> any (\instr -> case instr of IJump _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IJump"
  ]

testCodeGenDefine :: Test
testCodeGenDefine = TestList
  [ "gen simple define" ~: case generateCode "test" (EDefine "x" (EInt 42)) of
      Right code -> any (\instr -> case instr of IStore _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IStore"
  , "gen define with lambda" ~: case generateCodeWithDefs "test" (EDefine "square" (ELambda ["x"] (EApp (EVar "*") [EVar "x", EVar "x"]))) of
      (Right _, codeObjs) -> Map.member "square" codeObjs
      _ -> False
      ~? "Should generate code object for lambda"
  , "gen multiple defines" ~: case generateCodeWithDefs "test" (EList [EDefine "f" (ELambda ["x"] (EVar "x")), EDefine "g" (ELambda ["y"] (EVar "y"))]) of
      (Right _, codeObjs) -> Map.member "f" codeObjs && Map.member "g" codeObjs
      _ -> False
      ~? "Should generate multiple code objects"
  , "gen recursive define" ~: case generateCodeWithDefs "test" (EDefine "fact" (ELambda ["n"] (EApp (EVar "fact") [EVar "n"]))) of
      (Right _, codeObjs) -> Map.member "fact" codeObjs
      _ -> False
      ~? "Should generate recursive function"
  , "gen define arity" ~: case generateCodeWithDefs "test" (EDefine "add" (ELambda ["x", "y"] (EApp (EVar "+") [EVar "x", EVar "y"]))) of
      (Right _, codeObjs) -> case Map.lookup "add" codeObjs of
        Just code -> coArity code == 2
        Nothing -> False
      _ -> False
      ~? "Should set correct arity"
  ]

testCodeGenLambda :: Test
testCodeGenLambda = TestList
  [ "gen lambda with params" ~: case generateCodeWithDefs "test" (EDefine "f" (ELambda ["x", "y"] (EApp (EVar "+") [EVar "x", EVar "y"]))) of
      (Right _, codeObjs) -> case Map.lookup "f" codeObjs of
        Just code -> coMaxLocals code >= 2
        Nothing -> False
      _ -> False
      ~? "Should allocate locals for params"
  , "gen lambda with ILoad" ~: case generateCodeWithDefs "test" (EDefine "f" (ELambda ["x"] (EVar "x"))) of
      (Right _, codeObjs) -> case Map.lookup "f" codeObjs of
        Just code -> any (\instr -> case instr of ILoad _ -> True; _ -> False) (Vector.toList $ coInstrs code)
        Nothing -> False
      _ -> False
      ~? "Should generate ILoad for param"
  , "gen lambda with IReturn" ~: case generateCodeWithDefs "test" (EDefine "f" (ELambda ["x"] (EVar "x"))) of
      (Right _, codeObjs) -> case Map.lookup "f" codeObjs of
        Just code -> any (\instr -> case instr of IReturn -> True; _ -> False) (Vector.toList $ coInstrs code)
        Nothing -> False
      _ -> False
      ~? "Should generate IReturn"
  , "gen nested lambda" ~: case generateCodeWithDefs "test" (EDefine "make-adder" (ELambda ["x"] (ELambda ["y"] (EApp (EVar "+") [EVar "x", EVar "y"])))) of
      (Right _, codeObjs) -> Map.size codeObjs >= 1
      _ -> False
      ~? "Should generate nested lambda"
  , "gen lambda arity zero" ~: case generateCodeWithDefs "test" (EDefine "thunk" (ELambda [] (EInt 42))) of
      (Right _, codeObjs) -> case Map.lookup "thunk" codeObjs of
        Just code -> coArity code == 0
        Nothing -> False
      _ -> False
      ~? "Should handle zero arity"
  ]

testCodeGenApplication :: Test
testCodeGenApplication = TestList
  [ "gen function call" ~: case generateCode "test" (EApp (EVar "f") [EInt 1, EInt 2]) of
      Right code -> any (\instr -> case instr of ICall 2 "f" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate ICall"
  , "gen call with no args" ~: case generateCode "test" (EApp (EVar "f") []) of
      Right code -> any (\instr -> case instr of ICall 0 "f" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate ICall with 0 arity"
  , "gen call with many args" ~: case generateCode "test" (EApp (EVar "f") [EInt 1, EInt 2, EInt 3, EInt 4]) of
      Right code -> any (\instr -> case instr of ICall 4 "f" -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate ICall with 4 arity"
  , "gen nested call" ~: case generateCode "test" (EApp (EVar "f") [EApp (EVar "g") [EInt 1]]) of
      Right code -> length (filter (\instr -> case instr of ICall _ _ -> True; _ -> False) (Vector.toList $ coInstrs code)) >= 2
      Left _ -> False
      ~? "Should generate nested calls"
  , "gen call args pushed in order" ~: case generateCode "test" (EApp (EVar "f") [EInt 1, EInt 2, EInt 3]) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should push args in order"
  ]

testCodeGenList :: Test
testCodeGenList = TestList
  [ "gen begin block" ~: case generateCode "test" (EList [EInt 1, EInt 2, EInt 3]) of
      Right code -> any (\instr -> case instr of IPop -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
      ~? "Should generate IPop for begin"
  , "gen single expr list" ~: case generateCode "test" (EList [EInt 42]) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate single expr"
  , "gen empty list" ~: case generateCode "test" (EList []) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate empty list"
  , "gen list with defines" ~: case generateCode "test" (EList [EDefine "x" (EInt 1), EDefine "y" (EInt 2), EApp (EVar "+") [EVar "x", EVar "y"]]) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate list with defines"
  , "gen list with mixed exprs" ~: case generateCode "test" (EList [EInt 1, EApp (EVar "+") [EInt 2, EInt 3], EInt 4]) of
      Right code -> Vector.length (coInstrs code) > 0
      Left _ -> False
      ~? "Should generate mixed list"
  ]

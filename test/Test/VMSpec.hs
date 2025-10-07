module Test.VMSpec (tests) where

import Test.HUnit

import AST
import Builtins
import Compiler
import VM
import qualified Data.Map as Map
import qualified Data.Vector as Vector

tests :: Test
tests = TestList
  [ TestLabel "VM Basic" testVMBasic
  , TestLabel "VM Arithmetic" testVMArithmetic
  , TestLabel "VM Subtraction" testVMSubtraction
  , TestLabel "VM Multiplication" testVMMultiplication
  , TestLabel "VM Division" testVMDivision
  , TestLabel "VM Modulo" testVMModulo
  , TestLabel "VM Comparison" testVMComparison
  , TestLabel "VM Less Than" testVMLessThan
  , TestLabel "VM Greater Than" testVMGreaterThan
  , TestLabel "VM Local Variables" testVMLocalVariables
  , TestLabel "VM If True" testVMIfTrue
  , TestLabel "VM If False" testVMIfFalse
  , TestLabel "VM Pop" testVMPop
  , TestLabel "VM Nop" testVMNop
  , TestLabel "VM Function Call" testVMFunctionCall
  , TestLabel "VM Recursive Fibonacci" testVMRecursiveFibonacci
  , TestLabel "VM Recursive Factorial" testVMRecursiveFactorial
  , TestLabel "VM Mutual Recursion" testVMMutualRecursion
  , TestLabel "VM Nested Calls" testVMNestedCalls
  , TestLabel "VM Complex Expression" testVMComplexExpression
  ]

testVMBasic :: Test
testVMBasic = "VM basic operations" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 42]
        , coInstrs = Vector.fromList [IConst 0, IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 42) -> return ()
    _ -> assertFailure $ "Expected VInt 42, got: " ++ show result

testVMArithmetic :: Test
testVMArithmetic = "VM addition" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 2, CInt 3]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "+", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 5) -> return ()
    _ -> assertFailure $ "Expected VInt 5, got: " ++ show result

testVMSubtraction :: Test
testVMSubtraction = "VM subtraction" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 10, CInt 3]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "-", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 7) -> return ()
    _ -> assertFailure $ "Expected VInt 7, got: " ++ show result

testVMMultiplication :: Test
testVMMultiplication = "VM multiplication" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 4, CInt 5]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "*", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 20) -> return ()
    _ -> assertFailure $ "Expected VInt 20, got: " ++ show result

testVMDivision :: Test
testVMDivision = "VM division" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 20, CInt 4]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "div", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 5) -> return ()
    _ -> assertFailure $ "Expected VInt 5, got: " ++ show result

testVMModulo :: Test
testVMModulo = "VM modulo" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 10, CInt 3]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "mod", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 1) -> return ()
    _ -> assertFailure $ "Expected VInt 1, got: " ++ show result

testVMComparison :: Test
testVMComparison = "VM comparison" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 5, CInt 5]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "eq?", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VBool True) -> return ()
    _ -> assertFailure $ "Expected VBool True, got: " ++ show result

testVMLessThan :: Test
testVMLessThan = "VM less than" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 3, CInt 7]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "<", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VBool True) -> return ()
    _ -> assertFailure $ "Expected VBool True, got: " ++ show result

testVMGreaterThan :: Test
testVMGreaterThan = "VM greater than" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 10, CInt 4]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim ">", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VBool True) -> return ()
    _ -> assertFailure $ "Expected VBool True, got: " ++ show result

testVMLocalVariables :: Test
testVMLocalVariables = "VM local variables" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 1
        , coConsts = Vector.fromList [CInt 42]
        , coInstrs = Vector.fromList
            [ IConst 0
            , IStore 0
            , ILoad 0
            , IReturn
            ]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 42) -> return ()
    _ -> assertFailure $ "Expected VInt 42, got: " ++ show result

testVMIfTrue :: Test
testVMIfTrue = "VM if true" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CBool True, CInt 1, CInt 0]
        , coInstrs = Vector.fromList
            [ IConst 0
            , IJumpIfFalse 4
            , IConst 1
            , IJump 5
            , IConst 2
            , IReturn
            ]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 1) -> return ()
    _ -> assertFailure $ "Expected VInt 1, got: " ++ show result

testVMIfFalse :: Test
testVMIfFalse = "VM if false" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CBool False, CInt 1, CInt 0]
        , coInstrs = Vector.fromList
            [ IConst 0
            , IJumpIfFalse 4
            , IConst 1
            , IJump 5
            , IConst 2
            , IReturn
            ]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 0) -> return ()
    _ -> assertFailure $ "Expected VInt 0, got: " ++ show result

testVMPop :: Test
testVMPop = "VM pop instruction" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 1, CInt 2, CInt 3]
        , coInstrs = Vector.fromList
            [ IConst 0
            , IPop
            , IConst 1
            , IPop
            , IConst 2
            , IReturn
            ]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 3) -> return ()
    _ -> assertFailure $ "Expected VInt 3, got: " ++ show result

testVMNop :: Test
testVMNop = "VM nop instruction" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 42]
        , coInstrs = Vector.fromList [INop, INop, IConst 0, INop, IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 42) -> return ()
    _ -> assertFailure $ "Expected VInt 42, got: " ++ show result

testVMFunctionCall :: Test
testVMFunctionCall = "VM function call" ~: TestCase $ do
  let squareCode = CodeObject
        { coName = "square"
        , coArity = 1
        , coMaxLocals = 1
        , coConsts = Vector.fromList []
        , coInstrs = Vector.fromList
            [ ILoad 0
            , ILoad 0
            , IPrim "*"
            , IReturn
            ]
        , coLabelMap = Map.empty
        }
  let mainCode = CodeObject
        { coName = "main"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 5]
        , coInstrs = Vector.fromList
            [ IConst 0
            , ICall 1 "square"
            , IReturn
            ]
        , coLabelMap = Map.empty
        }
  let vmState = initVMState
        { vCodeObjects = Map.fromList [("square", squareCode)]
        , vBuiltins = builtins
        }
  result <- execVM vmState mainCode
  case result of
    Right (VInt 25) -> return ()
    _ -> assertFailure $ "Expected VInt 25, got: " ++ show result

testVMRecursiveFibonacci :: Test
testVMRecursiveFibonacci = "VM recursive fibonacci" ~: TestCase $ do
  let source = "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 6)"
  case compileWithDefs defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right (code, defs) -> do
      let vmState = initVMState { vCodeObjects = defs }
      result <- execVM vmState code
      case result of
        Right (VInt 8) -> return ()
        _ -> assertFailure $ "Expected VInt 8, got: " ++ show result

testVMRecursiveFactorial :: Test
testVMRecursiveFactorial = "VM recursive factorial" ~: TestCase $ do
  let source = "(define (fact n) (if (eq? n 0) 1 (* n (fact (- n 1))))) (fact 5)"
  case compileWithDefs defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right (code, defs) -> do
      let vmState = initVMState { vCodeObjects = defs }
      result <- execVM vmState code
      case result of
        Right (VInt 120) -> return ()
        _ -> assertFailure $ "Expected VInt 120, got: " ++ show result

testVMMutualRecursion :: Test
testVMMutualRecursion = "VM mutual recursion" ~: TestCase $ do
  let source = "(letrec ((even? (lambda (n) (if (eq? n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (eq? n 0) #f (even? (- n 1)))))) (even? 10))"
  case compileWithDefs defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right (code, defs) -> do
      let vmState = initVMState { vCodeObjects = defs }
      result <- execVM vmState code
      case result of
        Right (VBool True) -> return ()
        _ -> assertFailure $ "Expected VBool True, got: " ++ show result

testVMNestedCalls :: Test
testVMNestedCalls = "VM nested calls" ~: TestCase $ do
  let source = "(define (add x y) (+ x y)) (define (mul x y) (* x y)) (add (mul 3 4) (mul 2 5))"
  case compileWithDefs defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right (code, defs) -> do
      let vmState = initVMState { vCodeObjects = defs }
      result <- execVM vmState code
      case result of
        Right (VInt 22) -> return ()
        _ -> assertFailure $ "Expected VInt 22, got: " ++ show result

testVMComplexExpression :: Test
testVMComplexExpression = "VM complex expression" ~: TestCase $ do
  let source = "(* (+ 2 3) (- 10 4))"
  case compileWithDefs defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right (code, defs) -> do
      let vmState = initVMState { vCodeObjects = defs }
      result <- execVM vmState code
      case result of
        Right (VInt 30) -> return ()
        _ -> assertFailure $ "Expected VInt 30, got: " ++ show result

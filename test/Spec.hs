{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import AST
import SExprParser
import MacroExpander
import Desugar
import AlphaRename
import Compiler
import CodeGen
import VM
import qualified Data.Map as Map

-- Test: Parsing simple expressions
testParsing :: Test
testParsing = TestList
  [ "parse integer" ~: parseFromString "42" ~?= Right [SAtom (AInteger 42) (Just (SourcePos 1 1))]
  , "parse boolean true" ~: parseFromString "#t" ~?= Right [SAtom (ABool True) (Just (SourcePos 1 1))]
  , "parse symbol" ~: parseFromString "foo" ~?= Right [SAtom (ASymbol "foo") (Just (SourcePos 1 1))]
  , "parse list" ~: case parseFromString "(+ 1 2)" of
      Right [SList [SAtom (ASymbol "+") _, SAtom (AInteger 1) _, SAtom (AInteger 2) _] _] -> True
      _ -> False
      ~? "Should parse list correctly"
  ]

-- Test: Macro expansion
testMacros :: Test
testMacros = TestList
  [ "when macro" ~:
      let input = SList [SAtom (ASymbol "when") Nothing, SAtom (ABool True) Nothing, SAtom (AInteger 1) Nothing] Nothing
          expected = SList [SAtom (ASymbol "if") Nothing, SAtom (ABool True) Nothing, SAtom (AInteger 1) Nothing, SAtom (ABool False) Nothing] Nothing
      in expandMacros defaultMacroEnv input ~?= Right expected
  ]

-- Test: Desugaring
testDesugar :: Test
testDesugar = TestList
  [ "desugar define with lambda" ~:
      let input = SList
            [ SAtom (ASymbol "define") Nothing
            , SList [SAtom (ASymbol "square") Nothing, SAtom (ASymbol "x") Nothing] Nothing
            , SList [SAtom (ASymbol "*") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "x") Nothing] Nothing
            ] Nothing
      in case sexprToExpr input of
           Right (EDefine "square" (ELambda ["x"] _)) -> True
           _ -> False
           ~? "Should desugar define to lambda"
  ]

-- Test: Full compilation factorial
testFactorial :: Test
testFactorial = "compile factorial" ~:
  let source = "(define (fact n) (if (eq? n 1) 1 (* n (fact (- n 1)))))"
  in case compile defaultConfig source of
       Right _ -> True
       Left err -> error $ "Compilation failed: " ++ show err
       ~? "Should compile factorial"

-- Test: Closure example
testClosure :: Test
testClosure = "compile closure" ~:
  let source = "(define (make-adder n) (lambda (x) (+ x n)))"
  in case compile defaultConfig source of
       Right _ -> True
       Left err -> error $ "Compilation failed: " ++ show err
       ~? "Should compile closure"

-- Test: Simple arithmetic
testArithmetic :: Test
testArithmetic = "run simple arithmetic" ~: TestCase $ do
  let source = "(+ 2 3)"
  case compile defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right code -> do
      let vmState = initVMState { vCodeObjects = Map.singleton "main" code }
      result <- execVM vmState code
      case result of
        Left err -> assertFailure $ "Execution failed: " ++ show err
        Right (VInt 5) -> return ()
        Right val -> assertFailure $ "Expected VInt 5, got: " ++ show val

-- Test: If expression
testIfExpr :: Test
testIfExpr = "run if expression" ~: TestCase $ do
  let source = "(if #t 1 2)"
  case compile defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right code -> do
      let vmState = initVMState { vCodeObjects = Map.singleton "main" code }
      result <- execVM vmState code
      case result of
        Left err -> assertFailure $ "Execution failed: " ++ show err
        Right (VInt 1) -> return ()
        Right val -> assertFailure $ "Expected VInt 1, got: " ++ show val

-- All tests
allTests :: Test
allTests = TestList
  [ TestLabel "Parsing" testParsing
  , TestLabel "Macros" testMacros
  , TestLabel "Desugar" testDesugar
  , TestLabel "Factorial" testFactorial
  , TestLabel "Closure" testClosure
  , TestLabel "Arithmetic" testArithmetic
  , TestLabel "If Expression" testIfExpr
  ]

main :: IO ()
main = do
  putStrLn "Running GLaDOS test suite..."
  counts <- runTestTT allTests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
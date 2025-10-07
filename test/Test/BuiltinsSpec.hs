module Test.BuiltinsSpec (tests) where

import Test.HUnit

import AST
import Builtins
import qualified Data.Map as Map

tests :: Test
tests = TestList
  [ TestLabel "Builtins Addition" testBuiltinsAddition
  , TestLabel "Builtins Subtraction" testBuiltinsSubtraction
  , TestLabel "Builtins Multiplication" testBuiltinsMultiplication
  , TestLabel "Builtins Division" testBuiltinsDivision
  , TestLabel "Builtins Modulo" testBuiltinsModulo
  , TestLabel "Builtins Eq True" testBuiltinsEqTrue
  , TestLabel "Builtins Eq False" testBuiltinsEqFalse
  , TestLabel "Builtins Less Than True" testBuiltinsLessThanTrue
  , TestLabel "Builtins Less Than False" testBuiltinsLessThanFalse
  , TestLabel "Builtins Greater Than True" testBuiltinsGreaterThanTrue
  , TestLabel "Builtins Greater Than False" testBuiltinsGreaterThanFalse
  , TestLabel "Builtins Negative Numbers" testBuiltinsNegativeNumbers
  , TestLabel "Builtins Zero" testBuiltinsZero
  , TestLabel "Builtins Large Numbers" testBuiltinsLargeNumbers
  , TestLabel "Builtins Chained Ops" testBuiltinsChainedOps
  ]

testBuiltinsAddition :: Test
testBuiltinsAddition = "builtins addition" ~: TestCase $ do
  case Map.lookup "+" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 3, VInt 4]
      result @?= VInt 7
    _ -> assertFailure "Builtin + not found"

testBuiltinsSubtraction :: Test
testBuiltinsSubtraction = "builtins subtraction" ~: TestCase $ do
  case Map.lookup "-" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 10, VInt 3]
      result @?= VInt 7
    _ -> assertFailure "Builtin - not found"

testBuiltinsMultiplication :: Test
testBuiltinsMultiplication = "builtins multiplication" ~: TestCase $ do
  case Map.lookup "*" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 6, VInt 7]
      result @?= VInt 42
    _ -> assertFailure "Builtin * not found"

testBuiltinsDivision :: Test
testBuiltinsDivision = "builtins division" ~: TestCase $ do
  case Map.lookup "div" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 20, VInt 4]
      result @?= VInt 5
    _ -> assertFailure "Builtin div not found"

testBuiltinsModulo :: Test
testBuiltinsModulo = "builtins modulo" ~: TestCase $ do
  case Map.lookup "mod" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 17, VInt 5]
      result @?= VInt 2
    _ -> assertFailure "Builtin mod not found"

testBuiltinsEqTrue :: Test
testBuiltinsEqTrue = "builtins eq true" ~: TestCase $ do
  case Map.lookup "eq?" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 5, VInt 5]
      result @?= VBool True
    _ -> assertFailure "Builtin eq? not found"

testBuiltinsEqFalse :: Test
testBuiltinsEqFalse = "builtins eq false" ~: TestCase $ do
  case Map.lookup "eq?" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 5, VInt 6]
      result @?= VBool False
    _ -> assertFailure "Builtin eq? not found"

testBuiltinsLessThanTrue :: Test
testBuiltinsLessThanTrue = "builtins less than true" ~: TestCase $ do
  case Map.lookup "<" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 3, VInt 5]
      result @?= VBool True
    _ -> assertFailure "Builtin < not found"

testBuiltinsLessThanFalse :: Test
testBuiltinsLessThanFalse = "builtins less than false" ~: TestCase $ do
  case Map.lookup "<" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 5, VInt 3]
      result @?= VBool False
    _ -> assertFailure "Builtin < not found"

testBuiltinsGreaterThanTrue :: Test
testBuiltinsGreaterThanTrue = "builtins greater than true" ~: TestCase $ do
  case Map.lookup ">" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 10, VInt 5]
      result @?= VBool True
    _ -> assertFailure "Builtin > not found"

testBuiltinsGreaterThanFalse :: Test
testBuiltinsGreaterThanFalse = "builtins greater than false" ~: TestCase $ do
  case Map.lookup ">" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 3, VInt 5]
      result @?= VBool False
    _ -> assertFailure "Builtin > not found"

testBuiltinsNegativeNumbers :: Test
testBuiltinsNegativeNumbers = "builtins negative numbers" ~: TestCase $ do
  case Map.lookup "+" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt (-5), VInt 3]
      result @?= VInt (-2)
    _ -> assertFailure "Builtin + not found"

testBuiltinsZero :: Test
testBuiltinsZero = "builtins with zero" ~: TestCase $ do
  case Map.lookup "*" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 42, VInt 0]
      result @?= VInt 0
    _ -> assertFailure "Builtin * not found"

testBuiltinsLargeNumbers :: Test
testBuiltinsLargeNumbers = "builtins large numbers" ~: TestCase $ do
  case Map.lookup "*" builtins of
    Just (VBuiltin _ func) -> do
      result <- func [VInt 1000, VInt 1000]
      result @?= VInt 1000000
    _ -> assertFailure "Builtin * not found"

testBuiltinsChainedOps :: Test
testBuiltinsChainedOps = "builtins chained operations" ~: TestCase $ do
  case (Map.lookup "+" builtins, Map.lookup "*" builtins) of
    (Just (VBuiltin _ add), Just (VBuiltin _ mul)) -> do
      temp <- mul [VInt 2, VInt 3]
      result <- add [temp, VInt 4]
      result @?= VInt 10
    _ -> assertFailure "Builtins not found"

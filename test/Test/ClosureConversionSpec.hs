module Test.ClosureConversionSpec (tests) where

import Test.HUnit

import AST
import ClosureConversion

tests :: Test
tests = TestList
  [ TestLabel "Closure Conversion Basic" testClosureConversionBasic
  , TestLabel "Closure Conversion Lambda" testClosureConversionLambda
  , TestLabel "Closure Conversion Define" testClosureConversionDefine
  , TestLabel "Closure Conversion Other" testClosureConversionOther
  ]

testClosureConversionBasic :: Test
testClosureConversionBasic = TestList
  [ "convert integer" ~: closureConvert (EInt 42) ~?= Right (EInt 42)
  , "convert boolean" ~: closureConvert (EBool True) ~?= Right (EBool True)
  , "convert string" ~: closureConvert (EString "hello") ~?= Right (EString "hello")
  , "convert var" ~: closureConvert (EVar "x") ~?= Right (EVar "x")
  , "convert quote" ~: closureConvert (EQuote (SAtom (AInteger 1) Nothing)) ~?= Right (EQuote (SAtom (AInteger 1) Nothing))
  ]

testClosureConversionLambda :: Test
testClosureConversionLambda = TestList
  [ "convert simple lambda" ~: closureConvert (ELambda ["x"] (EVar "x")) ~?= Right (ELambda ["x"] (EVar "x"))
  , "convert lambda with free var" ~: closureConvert (ELambda ["x"] (EApp (EVar "+") [EVar "x", EVar "y"])) ~?= Right (ELambda ["x"] (EApp (EVar "+") [EVar "x", EVar "y"]))
  , "convert nested lambda" ~: closureConvert (ELambda ["x"] (ELambda ["y"] (EApp (EVar "+") [EVar "x", EVar "y"]))) ~?= Right (ELambda ["x"] (ELambda ["y"] (EApp (EVar "+") [EVar "x", EVar "y"])))
  , "convert lambda with no params" ~: closureConvert (ELambda [] (EInt 42)) ~?= Right (ELambda [] (EInt 42))
  , "convert lambda with complex body" ~: closureConvert (ELambda ["x"] (EIf (EVar "x") (EInt 1) (EInt 0))) ~?= Right (ELambda ["x"] (EIf (EVar "x") (EInt 1) (EInt 0)))
  ]

testClosureConversionDefine :: Test
testClosureConversionDefine = TestList
  [ "convert simple define" ~: closureConvert (EDefine "x" (EInt 42)) ~?= Right (EDefine "x" (EInt 42))
  , "convert define with lambda" ~: closureConvert (EDefine "f" (ELambda ["x"] (EVar "x"))) ~?= Right (EDefine "f" (ELambda ["x"] (EVar "x")))
  , "convert define with expression" ~: closureConvert (EDefine "result" (EApp (EVar "+") [EInt 1, EInt 2])) ~?= Right (EDefine "result" (EApp (EVar "+") [EInt 1, EInt 2]))
  , "convert recursive define" ~: closureConvert (EDefine "fact" (ELambda ["n"] (EApp (EVar "fact") [EVar "n"]))) ~?= Right (EDefine "fact" (ELambda ["n"] (EApp (EVar "fact") [EVar "n"])))
  , "convert multiple defines" ~: closureConvert (EList [EDefine "x" (EInt 1), EDefine "y" (EInt 2)]) ~?= Right (EList [EDefine "x" (EInt 1), EDefine "y" (EInt 2)])
  ]

testClosureConversionOther :: Test
testClosureConversionOther = TestList
  [ "convert if expression" ~: closureConvert (EIf (EBool True) (EInt 1) (EInt 2)) ~?= Right (EIf (EBool True) (EInt 1) (EInt 2))
  , "convert application" ~: closureConvert (EApp (EVar "f") [EInt 1]) ~?= Right (EApp (EVar "f") [EInt 1])
  , "convert list" ~: closureConvert (EList [EInt 1, EInt 2]) ~?= Right (EList [EInt 1, EInt 2])
  , "convert nested structures" ~: closureConvert (EList [EDefine "f" (ELambda ["x"] (EVar "x")), EApp (EVar "f") [EInt 42]]) ~?= Right (EList [EDefine "f" (ELambda ["x"] (EVar "x")), EApp (EVar "f") [EInt 42]])
  , "convert complex expression" ~: closureConvert (ELambda ["x"] (EList [EDefine "y" (EVar "x"), EVar "y"])) ~?= Right (ELambda ["x"] (EList [EDefine "y" (EVar "x"), EVar "y"]))
  ]

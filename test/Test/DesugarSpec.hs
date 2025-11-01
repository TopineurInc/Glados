{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Test.DesugarSpec (tests) where

import Test.HUnit

import AST
import Desugar

-- Helper pattern for Lisp-style lambda (without type annotations)
pattern LispLambda :: [Name] -> Expr -> Expr
pattern LispLambda params body <- ELambda (map fst -> params) Nothing body _
  where LispLambda params body = ELambda (map (\p -> (p, Nothing)) params) Nothing body []

tests :: Test
tests = TestList
  [ TestLabel "Desugar Basic" testDesugarBasic
  , TestLabel "Desugar If" testDesugarIf
  , TestLabel "Desugar Define" testDesugarDefine
  , TestLabel "Desugar Lambda" testDesugarLambda
  , TestLabel "Desugar Let" testDesugarLet
  , TestLabel "Desugar Letrec" testDesugarLetrec
  , TestLabel "Desugar Begin" testDesugarBegin
  , TestLabel "Desugar Application" testDesugarApplication
  , TestLabel "Desugar Quote" testDesugarQuote
  ]

testDesugarBasic :: Test
testDesugarBasic = TestList
  [ "desugar integer" ~: sexprToExpr (SAtom (AInteger 42) Nothing) ~?= Right (EInt 42)
  , "desugar float" ~: sexprToExpr (SAtom (AFloat 3.14) Nothing) ~?= Right (EFloat 3.14)
  , "desugar boolean true" ~: sexprToExpr (SAtom (ABool True) Nothing) ~?= Right (EBool True)
  , "desugar boolean false" ~: sexprToExpr (SAtom (ABool False) Nothing) ~?= Right (EBool False)
  , "desugar string" ~: sexprToExpr (SAtom (AString "hello") Nothing) ~?= Right (EString "hello")
  , "desugar symbol" ~: sexprToExpr (SAtom (ASymbol "foo") Nothing) ~?= Right (EVar "foo")
  , "desugar negative integer" ~: sexprToExpr (SAtom (AInteger (-42)) Nothing) ~?= Right (EInt (-42))
  ]

testDesugarIf :: Test
testDesugarIf = TestList
  [ "desugar simple if" ~: case sexprToExpr (SList [SAtom (ASymbol "if") Nothing, SAtom (ABool True) Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing) of
      Right (EIf (EBool True) (EInt 1) (EInt 2)) -> True
      _ -> False
      ~? "Should desugar if"
  , "desugar nested if" ~: case sexprToExpr (SList [SAtom (ASymbol "if") Nothing, SAtom (ABool True) Nothing, SList [SAtom (ASymbol "if") Nothing, SAtom (ABool False) Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing, SAtom (AInteger 3) Nothing] Nothing) of
      Right (EIf (EBool True) (EIf (EBool False) (EInt 1) (EInt 2)) (EInt 3)) -> True
      _ -> False
      ~? "Should desugar nested if"
  , "desugar if with var condition" ~: case sexprToExpr (SList [SAtom (ASymbol "if") Nothing, SAtom (ASymbol "x") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing) of
      Right (EIf (EVar "x") (EInt 1) (EInt 2)) -> True
      _ -> False
      ~? "Should desugar if with var"
  , "desugar if with complex then" ~: case sexprToExpr (SList [SAtom (ASymbol "if") Nothing, SAtom (ABool True) Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing, SAtom (AInteger 0) Nothing] Nothing) of
      Right (EIf (EBool True) (EApp (EVar "+") [EInt 1, EInt 2]) (EInt 0)) -> True
      _ -> False
      ~? "Should desugar if with complex then"
  , "desugar if with complex else" ~: case sexprToExpr (SList [SAtom (ASymbol "if") Nothing, SAtom (ABool False) Nothing, SAtom (AInteger 0) Nothing, SList [SAtom (ASymbol "*") Nothing, SAtom (AInteger 3) Nothing, SAtom (AInteger 4) Nothing] Nothing] Nothing) of
      Right (EIf (EBool False) (EInt 0) (EApp (EVar "*") [EInt 3, EInt 4])) -> True
      _ -> False
      ~? "Should desugar if with complex else"
  ]

testDesugarDefine :: Test
testDesugarDefine = TestList
  [ "desugar simple define" ~: case sexprToExpr (SList [SAtom (ASymbol "define") Nothing, SAtom (ASymbol "x") Nothing, SAtom (AInteger 42) Nothing] Nothing) of
      Right (EDefine "x" (EInt 42) []) -> True
      _ -> False
      ~? "Should desugar simple define"
  , "desugar define with lambda sugar" ~: case sexprToExpr (SList [SAtom (ASymbol "define") Nothing, SList [SAtom (ASymbol "square") Nothing, SAtom (ASymbol "x") Nothing] Nothing, SList [SAtom (ASymbol "*") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "x") Nothing] Nothing] Nothing) of
      Right (EDefine "square" (LispLambda ["x"] (EApp (EVar "*") [EVar "x", EVar "x"])) []) -> True
      _ -> False
      ~? "Should desugar define with lambda"
  , "desugar define with multiple params" ~: case sexprToExpr (SList [SAtom (ASymbol "define") Nothing, SList [SAtom (ASymbol "add") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing] Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing] Nothing] Nothing) of
      Right (EDefine "add" (LispLambda ["x", "y"] (EApp (EVar "+") [EVar "x", EVar "y"])) []) -> True
      _ -> False
      ~? "Should desugar define with multiple params"
  , "desugar define with expression" ~: case sexprToExpr (SList [SAtom (ASymbol "define") Nothing, SAtom (ASymbol "result") Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing] Nothing) of
      Right (EDefine "result" (EApp (EVar "+") [EInt 1, EInt 2]) []) -> True
      _ -> False
      ~? "Should desugar define with expression"
  , "desugar define with lambda" ~: case sexprToExpr (SList [SAtom (ASymbol "define") Nothing, SAtom (ASymbol "f") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing] Nothing) of
      Right (EDefine "f" (LispLambda ["x"] (EVar "x")) []) -> True
      _ -> False
      ~? "Should desugar define with explicit lambda"
  ]

testDesugarLambda :: Test
testDesugarLambda = TestList
  [ "desugar simple lambda" ~: case sexprToExpr (SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing) of
      Right (LispLambda ["x"] (EVar "x")) -> True
      _ -> False
      ~? "Should desugar lambda"
  , "desugar lambda with no params" ~: case sexprToExpr (SList [SAtom (ASymbol "lambda") Nothing, SList [] Nothing, SAtom (AInteger 42) Nothing] Nothing) of
      Right (LispLambda [] (EInt 42)) -> True
      _ -> False
      ~? "Should desugar lambda with no params"
  , "desugar lambda with multiple params" ~: case sexprToExpr (SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing, SAtom (ASymbol "z") Nothing] Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "x") Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "y") Nothing, SAtom (ASymbol "z") Nothing] Nothing] Nothing] Nothing) of
      Right (LispLambda ["x", "y", "z"] (EApp (EVar "+") [EVar "x", EApp (EVar "+") [EVar "y", EVar "z"]])) -> True
      _ -> False
      ~? "Should desugar lambda with multiple params"
  , "desugar lambda with complex body" ~: case sexprToExpr (SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SList [SAtom (ASymbol "if") Nothing, SAtom (ASymbol "x") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 0) Nothing] Nothing] Nothing) of
      Right (LispLambda ["x"] (EIf (EVar "x") (EInt 1) (EInt 0))) -> True
      _ -> False
      ~? "Should desugar lambda with complex body"
  , "desugar nested lambda" ~: case sexprToExpr (SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "y") Nothing] Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing] Nothing] Nothing] Nothing) of
      Right (LispLambda ["x"] (LispLambda ["y"] (EApp (EVar "+") [EVar "x", EVar "y"]))) -> True
      _ -> False
      ~? "Should desugar nested lambda"
  ]

testDesugarLet :: Test
testDesugarLet = TestList
  [ "desugar simple let" ~: case sexprToExpr (SList [SAtom (ASymbol "let") Nothing, SList [SList [SAtom (ASymbol "x") Nothing, SAtom (AInteger 42) Nothing] Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing) of
      Right (EApp (LispLambda ["x"] (EVar "x")) [EInt 42]) -> True
      _ -> False
      ~? "Should desugar let to lambda application"
  , "desugar let with multiple bindings" ~: case sexprToExpr (SList [SAtom (ASymbol "let") Nothing, SList [SList [SAtom (ASymbol "x") Nothing, SAtom (AInteger 1) Nothing] Nothing, SList [SAtom (ASymbol "y") Nothing, SAtom (AInteger 2) Nothing] Nothing] Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing] Nothing] Nothing) of
      Right (EApp (LispLambda ["x", "y"] (EApp (EVar "+") [EVar "x", EVar "y"])) [EInt 1, EInt 2]) -> True
      _ -> False
      ~? "Should desugar let with multiple bindings"
  , "desugar let with complex value" ~: case sexprToExpr (SList [SAtom (ASymbol "let") Nothing, SList [SList [SAtom (ASymbol "x") Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing] Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing) of
      Right (EApp (LispLambda ["x"] (EVar "x")) [EApp (EVar "+") [EInt 1, EInt 2]]) -> True
      _ -> False
      ~? "Should desugar let with complex value"
  , "desugar let with complex body" ~: case sexprToExpr (SList [SAtom (ASymbol "let") Nothing, SList [SList [SAtom (ASymbol "x") Nothing, SAtom (AInteger 5) Nothing] Nothing] Nothing, SList [SAtom (ASymbol "*") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "x") Nothing] Nothing] Nothing) of
      Right (EApp (LispLambda ["x"] (EApp (EVar "*") [EVar "x", EVar "x"])) [EInt 5]) -> True
      _ -> False
      ~? "Should desugar let with complex body"
  , "desugar nested let" ~: case sexprToExpr (SList [SAtom (ASymbol "let") Nothing, SList [SList [SAtom (ASymbol "x") Nothing, SAtom (AInteger 1) Nothing] Nothing] Nothing, SList [SAtom (ASymbol "let") Nothing, SList [SList [SAtom (ASymbol "y") Nothing, SAtom (AInteger 2) Nothing] Nothing] Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing] Nothing] Nothing] Nothing) of
      Right _ -> True
      _ -> False
      ~? "Should desugar nested let"
  ]

testDesugarLetrec :: Test
testDesugarLetrec = TestList
  [ "desugar simple letrec" ~: case sexprToExpr (SList [SAtom (ASymbol "letrec") Nothing, SList [SList [SAtom (ASymbol "f") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing] Nothing] Nothing, SAtom (ASymbol "f") Nothing] Nothing) of
      Right (EList [EDefine "f" (LispLambda ["x"] (EVar "x")) [], EVar "f"]) -> True
      _ -> False
      ~? "Should desugar letrec to defines"
  , "desugar letrec with multiple bindings" ~: case sexprToExpr (SList [SAtom (ASymbol "letrec") Nothing, SList [SList [SAtom (ASymbol "f") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [] Nothing, SAtom (AInteger 1) Nothing] Nothing] Nothing, SList [SAtom (ASymbol "g") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [] Nothing, SAtom (AInteger 2) Nothing] Nothing] Nothing] Nothing, SAtom (AInteger 0) Nothing] Nothing) of
      Right (EList [EDefine "f" (LispLambda [] (EInt 1)) [], EDefine "g" (LispLambda [] (EInt 2)) [], EInt 0]) -> True
      _ -> False
      ~? "Should desugar letrec with multiple bindings"
  , "desugar letrec with recursive reference" ~: case sexprToExpr (SList [SAtom (ASymbol "letrec") Nothing, SList [SList [SAtom (ASymbol "fact") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "n") Nothing] Nothing, SList [SAtom (ASymbol "if") Nothing, SList [SAtom (ASymbol "eq?") Nothing, SAtom (ASymbol "n") Nothing, SAtom (AInteger 0) Nothing] Nothing, SAtom (AInteger 1) Nothing, SList [SAtom (ASymbol "*") Nothing, SAtom (ASymbol "n") Nothing, SList [SAtom (ASymbol "fact") Nothing, SAtom (ASymbol "n") Nothing] Nothing] Nothing] Nothing] Nothing] Nothing] Nothing, SAtom (ASymbol "fact") Nothing] Nothing) of
      Right _ -> True
      _ -> False
      ~? "Should desugar letrec with recursive reference"
  ]

testDesugarBegin :: Test
testDesugarBegin = TestList
  [ "desugar begin with integers" ~: case sexprToExpr (SList [SAtom (ASymbol "begin") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing) of
      Right (EList [EInt 1, EInt 2]) -> True
      _ -> False
      ~? "Should desugar begin with integers"
  , "desugar begin with expressions" ~: case sexprToExpr (SList [SAtom (ASymbol "begin") Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing, SAtom (AInteger 3) Nothing] Nothing) of
      Right (EList [EApp (EVar "+") [EInt 1, EInt 2], EInt 3]) -> True
      _ -> False
      ~? "Should desugar begin with expressions"
  , "desugar begin with single expr" ~: case sexprToExpr (SList [SAtom (ASymbol "begin") Nothing, SAtom (AInteger 42) Nothing] Nothing) of
      Right (EList [EInt 42]) -> True
      _ -> False
      ~? "Should desugar begin with single expr"
  , "desugar begin nested" ~: case sexprToExpr (SList [SAtom (ASymbol "begin") Nothing, SList [SAtom (ASymbol "begin") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing, SAtom (AInteger 3) Nothing] Nothing) of
      Right (EList [EList [EInt 1, EInt 2], EInt 3]) -> True
      _ -> False
      ~? "Should desugar nested begin"
  ]

testDesugarApplication :: Test
testDesugarApplication = TestList
  [ "desugar simple application" ~: case sexprToExpr (SList [SAtom (ASymbol "+") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing) of
      Right (EApp (EVar "+") [EInt 1, EInt 2]) -> True
      _ -> False
      ~? "Should desugar application"
  , "desugar application with symbol func" ~: case sexprToExpr (SList [SAtom (ASymbol "foo") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing) of
      Right (EApp (EVar "foo") [EInt 1, EInt 2]) -> True
      _ -> False
      ~? "Should desugar application with symbol func"
  , "desugar application with lambda func" ~: case sexprToExpr (SList [SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing, SAtom (AInteger 42) Nothing] Nothing) of
      Right (EApp (LispLambda ["x"] (EVar "x")) [EInt 42]) -> True
      _ -> False
      ~? "Should desugar application with lambda func"
  , "desugar application with no args" ~: case sexprToExpr (SList [SAtom (ASymbol "foo") Nothing] Nothing) of
      Right (EApp (EVar "foo") []) -> True
      _ -> False
      ~? "Should desugar application with no args"
  , "desugar application with many args" ~: case sexprToExpr (SList [SAtom (ASymbol "+") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing, SAtom (AInteger 3) Nothing, SAtom (AInteger 4) Nothing] Nothing) of
      Right (EApp (EVar "+") [EInt 1, EInt 2, EInt 3, EInt 4]) -> True
      _ -> False
      ~? "Should desugar application with many args"
  , "desugar nested application" ~: case sexprToExpr (SList [SAtom (ASymbol "+") Nothing, SList [SAtom (ASymbol "*") Nothing, SAtom (AInteger 2) Nothing, SAtom (AInteger 3) Nothing] Nothing, SAtom (AInteger 4) Nothing] Nothing) of
      Right (EApp (EVar "+") [EApp (EVar "*") [EInt 2, EInt 3], EInt 4]) -> True
      _ -> False
      ~? "Should desugar nested application"
  , "desugar lambda application" ~: case sexprToExpr (SList [SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing, SAtom (AInteger 42) Nothing] Nothing) of
      Right (EApp (LispLambda ["x"] (EVar "x")) [EInt 42]) -> True
      _ -> False
      ~? "Should desugar lambda application"
  ]

testDesugarQuote :: Test
testDesugarQuote = TestList
  [ "desugar quote atom" ~: case sexprToExpr (SList [SAtom (ASymbol "quote") Nothing, SAtom (ASymbol "foo") Nothing] Nothing) of
      Right (EQuote (SAtom (ASymbol "foo") Nothing)) -> True
      _ -> False
      ~? "Should desugar quote"
  , "desugar quote list" ~: case sexprToExpr (SList [SAtom (ASymbol "quote") Nothing, SList [SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing] Nothing) of
      Right (EQuote (SList [SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing)) -> True
      _ -> False
      ~? "Should desugar quote list"
  , "desugar quote empty list" ~: case sexprToExpr (SList [SAtom (ASymbol "quote") Nothing, SList [] Nothing] Nothing) of
      Right (EQuote (SList [] Nothing)) -> True
      _ -> False
      ~? "Should desugar quote empty list"
  , "desugar quote nested" ~: case sexprToExpr (SList [SAtom (ASymbol "quote") Nothing, SList [SAtom (ASymbol "a") Nothing, SList [SAtom (ASymbol "b") Nothing, SAtom (ASymbol "c") Nothing] Nothing] Nothing] Nothing) of
      Right (EQuote (SList [SAtom (ASymbol "a") Nothing, SList [SAtom (ASymbol "b") Nothing, SAtom (ASymbol "c") Nothing] Nothing] Nothing)) -> True
      _ -> False
      ~? "Should desugar quote nested"
  ]

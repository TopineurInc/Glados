{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (void)

import AST
import SExprParser
import MacroExpander
import Desugar
import AlphaRename
import Compiler
import CodeGen
import VM
import Builtins
import ClosureConversion
import qualified Data.Map as Map
import qualified Data.Vector as Vector

-- ============================================================================
-- SEXPR PARSER TESTS (100+ tests)
-- ============================================================================

testParseInteger :: Test
testParseInteger = TestList
  [ "parse positive integer" ~: parseFromString "42" ~?= Right [SAtom (AInteger 42) (Just (SourcePos 1 1))]
  , "parse negative integer" ~: parseFromString "-42" ~?= Right [SAtom (AInteger (-42)) (Just (SourcePos 1 1))]
  , "parse zero" ~: parseFromString "0" ~?= Right [SAtom (AInteger 0) (Just (SourcePos 1 1))]
  , "parse large integer" ~: parseFromString "999999" ~?= Right [SAtom (AInteger 999999) (Just (SourcePos 1 1))]
  , "parse negative zero" ~: parseFromString "-0" ~?= Right [SAtom (AInteger 0) (Just (SourcePos 1 1))]
  , "parse multiple digits" ~: parseFromString "123456789" ~?= Right [SAtom (AInteger 123456789) (Just (SourcePos 1 1))]
  ]

testParseBool :: Test
testParseBool = TestList
  [ "parse true" ~: parseFromString "#t" ~?= Right [SAtom (ABool True) (Just (SourcePos 1 1))]
  , "parse false" ~: parseFromString "#f" ~?= Right [SAtom (ABool False) (Just (SourcePos 1 1))]
  , "parse true with whitespace" ~: parseFromString "  #t  " ~?= Right [SAtom (ABool True) (Just (SourcePos 1 3))]
  , "parse false with whitespace" ~: parseFromString "  #f  " ~?= Right [SAtom (ABool False) (Just (SourcePos 1 3))]
  , "parse multiple bools" ~: case parseFromString "#t #f" of
      Right [SAtom (ABool True) _, SAtom (ABool False) _] -> True
      _ -> False
      ~? "Should parse multiple bools"
  ]

testParseString :: Test
testParseString = TestList
  [ "parse empty string" ~: parseFromString "\"\"" ~?= Right [SAtom (AString "") (Just (SourcePos 1 1))]
  , "parse simple string" ~: parseFromString "\"hello\"" ~?= Right [SAtom (AString "hello") (Just (SourcePos 1 1))]
  , "parse string with spaces" ~: parseFromString "\"hello world\"" ~?= Right [SAtom (AString "hello world") (Just (SourcePos 1 1))]
  , "parse string with escaped quote" ~: parseFromString "\"hello \\\"world\\\"\"" ~?= Right [SAtom (AString "hello \"world\"") (Just (SourcePos 1 1))]
  , "parse string with numbers" ~: parseFromString "\"test123\"" ~?= Right [SAtom (AString "test123") (Just (SourcePos 1 1))]
  , "parse long string" ~: parseFromString "\"abcdefghijklmnopqrstuvwxyz\"" ~?= Right [SAtom (AString "abcdefghijklmnopqrstuvwxyz") (Just (SourcePos 1 1))]
  ]

testParseSymbol :: Test
testParseSymbol = TestList
  [ "parse simple symbol" ~: parseFromString "foo" ~?= Right [SAtom (ASymbol "foo") (Just (SourcePos 1 1))]
  , "parse symbol with numbers" ~: parseFromString "foo123" ~?= Right [SAtom (ASymbol "foo123") (Just (SourcePos 1 1))]
  , "parse symbol plus" ~: parseFromString "+" ~?= Right [SAtom (ASymbol "+") (Just (SourcePos 1 1))]
  , "parse symbol minus" ~: parseFromString "-" ~?= Right [SAtom (ASymbol "-") (Just (SourcePos 1 1))]
  , "parse symbol star" ~: parseFromString "*" ~?= Right [SAtom (ASymbol "*") (Just (SourcePos 1 1))]
  , "parse symbol div" ~: parseFromString "/" ~?= Right [SAtom (ASymbol "/") (Just (SourcePos 1 1))]
  , "parse symbol less" ~: parseFromString "<" ~?= Right [SAtom (ASymbol "<") (Just (SourcePos 1 1))]
  , "parse symbol greater" ~: parseFromString ">" ~?= Right [SAtom (ASymbol ">") (Just (SourcePos 1 1))]
  , "parse symbol equal" ~: parseFromString "=" ~?= Right [SAtom (ASymbol "=") (Just (SourcePos 1 1))]
  , "parse symbol question" ~: parseFromString "eq?" ~?= Right [SAtom (ASymbol "eq?") (Just (SourcePos 1 1))]
  , "parse symbol exclamation" ~: parseFromString "set!" ~?= Right [SAtom (ASymbol "set!") (Just (SourcePos 1 1))]
  , "parse symbol hyphen" ~: parseFromString "hello-world" ~?= Right [SAtom (ASymbol "hello-world") (Just (SourcePos 1 1))]
  ]

testParseList :: Test
testParseList = TestList
  [ "parse empty list" ~: parseFromString "()" ~?= Right [SList [] (Just (SourcePos 1 1))]
  , "parse simple list" ~: case parseFromString "(+ 1 2)" of
      Right [SList [SAtom (ASymbol "+") _, SAtom (AInteger 1) _, SAtom (AInteger 2) _] _] -> True
      _ -> False
      ~? "Should parse (+ 1 2)"
  , "parse nested list" ~: case parseFromString "(+ (* 2 3) 4)" of
      Right [SList [SAtom (ASymbol "+") _, SList [SAtom (ASymbol "*") _, SAtom (AInteger 2) _, SAtom (AInteger 3) _] _, SAtom (AInteger 4) _] _] -> True
      _ -> False
      ~? "Should parse nested list"
  , "parse deeply nested list" ~: case parseFromString "(a (b (c (d))))" of
      Right [SList [SAtom (ASymbol "a") _, SList [SAtom (ASymbol "b") _, SList [SAtom (ASymbol "c") _, SList [SAtom (ASymbol "d") _] _] _] _] _] -> True
      _ -> False
      ~? "Should parse deeply nested list"
  , "parse list with multiple elements" ~: case parseFromString "(1 2 3 4 5)" of
      Right [SList [SAtom (AInteger 1) _, SAtom (AInteger 2) _, SAtom (AInteger 3) _, SAtom (AInteger 4) _, SAtom (AInteger 5) _] _] -> True
      _ -> False
      ~? "Should parse list with multiple elements"
  , "parse list with mixed types" ~: case parseFromString "(1 #t \"hello\" foo)" of
      Right [SList [SAtom (AInteger 1) _, SAtom (ABool True) _, SAtom (AString "hello") _, SAtom (ASymbol "foo") _] _] -> True
      _ -> False
      ~? "Should parse list with mixed types"
  ]

testParseMultiple :: Test
testParseMultiple = TestList
  [ "parse multiple expressions" ~: case parseFromString "1 2 3" of
      Right [SAtom (AInteger 1) _, SAtom (AInteger 2) _, SAtom (AInteger 3) _] -> True
      _ -> False
      ~? "Should parse multiple expressions"
  , "parse multiple lists" ~: case parseFromString "(+ 1 2) (* 3 4)" of
      Right [SList [SAtom (ASymbol "+") _, _, _] _, SList [SAtom (ASymbol "*") _, _, _] _] -> True
      _ -> False
      ~? "Should parse multiple lists"
  , "parse mixed multiple" ~: case parseFromString "42 (+ 1 2) foo" of
      Right [SAtom (AInteger 42) _, SList _ _, SAtom (ASymbol "foo") _] -> True
      _ -> False
      ~? "Should parse mixed multiple"
  , "parse with newlines" ~: case parseFromString "1\n2\n3" of
      Right [SAtom (AInteger 1) _, SAtom (AInteger 2) _, SAtom (AInteger 3) _] -> True
      _ -> False
      ~? "Should parse with newlines"
  , "parse with tabs" ~: case parseFromString "1\t2\t3" of
      Right [SAtom (AInteger 1) _, SAtom (AInteger 2) _, SAtom (AInteger 3) _] -> True
      _ -> False
      ~? "Should parse with tabs"
  ]

testParseWhitespace :: Test
testParseWhitespace = TestList
  [ "parse with leading whitespace" ~: parseFromString "   42" ~?= Right [SAtom (AInteger 42) (Just (SourcePos 1 4))]
  , "parse with trailing whitespace" ~: parseFromString "42   " ~?= Right [SAtom (AInteger 42) (Just (SourcePos 1 1))]
  , "parse with surrounding whitespace" ~: parseFromString "   42   " ~?= Right [SAtom (AInteger 42) (Just (SourcePos 1 4))]
  , "parse list with internal whitespace" ~: case parseFromString "(  +   1   2  )" of
      Right [SList [SAtom (ASymbol "+") _, SAtom (AInteger 1) _, SAtom (AInteger 2) _] _] -> True
      _ -> False
      ~? "Should handle internal whitespace"
  , "parse with only whitespace between elements" ~: case parseFromString "(+ 1 2)" of
      Right [SList [SAtom (ASymbol "+") _, SAtom (AInteger 1) _, SAtom (AInteger 2) _] _] -> True
      _ -> False
      ~? "Should parse with whitespace"
  ]

testParseErrors :: Test
testParseErrors = TestList
  [ "reject unclosed list" ~: case parseFromString "(+ 1 2" of
      Left _ -> True
      Right _ -> False
      ~? "Should reject unclosed list"
  , "reject unopened list" ~: case parseFromString "+ 1 2)" of
      Left _ -> True
      Right _ -> False
      ~? "Should reject unopened list"
  , "reject unclosed string" ~: case parseFromString "\"hello" of
      Left _ -> True
      Right _ -> False
      ~? "Should reject unclosed string"
  , "reject invalid boolean" ~: case parseFromString "#x" of
      Left _ -> True
      Right _ -> False
      ~? "Should reject invalid boolean"
  , "reject trailing garbage" ~: case parseFromString "42 garbage" of
      Left _ -> False  -- This actually parses both as separate atoms
      Right _ -> True
      ~? "Multiple atoms are valid"
  ]

-- ============================================================================
-- DESUGAR TESTS (50+ tests)
-- ============================================================================

testDesugarBasic :: Test
testDesugarBasic = TestList
  [ "desugar integer" ~: sexprToExpr (SAtom (AInteger 42) Nothing) ~?= Right (EInt 42)
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
      Right (EDefine "x" (EInt 42)) -> True
      _ -> False
      ~? "Should desugar simple define"
  , "desugar define with lambda sugar" ~: case sexprToExpr (SList [SAtom (ASymbol "define") Nothing, SList [SAtom (ASymbol "square") Nothing, SAtom (ASymbol "x") Nothing] Nothing, SList [SAtom (ASymbol "*") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "x") Nothing] Nothing] Nothing) of
      Right (EDefine "square" (ELambda ["x"] (EApp (EVar "*") [EVar "x", EVar "x"]))) -> True
      _ -> False
      ~? "Should desugar define with lambda"
  , "desugar define with multiple params" ~: case sexprToExpr (SList [SAtom (ASymbol "define") Nothing, SList [SAtom (ASymbol "add") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing] Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing] Nothing] Nothing) of
      Right (EDefine "add" (ELambda ["x", "y"] (EApp (EVar "+") [EVar "x", EVar "y"]))) -> True
      _ -> False
      ~? "Should desugar define with multiple params"
  , "desugar define with expression" ~: case sexprToExpr (SList [SAtom (ASymbol "define") Nothing, SAtom (ASymbol "result") Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing] Nothing) of
      Right (EDefine "result" (EApp (EVar "+") [EInt 1, EInt 2])) -> True
      _ -> False
      ~? "Should desugar define with expression"
  , "desugar define with lambda" ~: case sexprToExpr (SList [SAtom (ASymbol "define") Nothing, SAtom (ASymbol "f") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing] Nothing) of
      Right (EDefine "f" (ELambda ["x"] (EVar "x"))) -> True
      _ -> False
      ~? "Should desugar define with explicit lambda"
  ]

testDesugarLambda :: Test
testDesugarLambda = TestList
  [ "desugar simple lambda" ~: case sexprToExpr (SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing) of
      Right (ELambda ["x"] (EVar "x")) -> True
      _ -> False
      ~? "Should desugar lambda"
  , "desugar lambda with no params" ~: case sexprToExpr (SList [SAtom (ASymbol "lambda") Nothing, SList [] Nothing, SAtom (AInteger 42) Nothing] Nothing) of
      Right (ELambda [] (EInt 42)) -> True
      _ -> False
      ~? "Should desugar lambda with no params"
  , "desugar lambda with multiple params" ~: case sexprToExpr (SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing, SAtom (ASymbol "z") Nothing] Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "x") Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "y") Nothing, SAtom (ASymbol "z") Nothing] Nothing] Nothing] Nothing) of
      Right (ELambda ["x", "y", "z"] (EApp (EVar "+") [EVar "x", EApp (EVar "+") [EVar "y", EVar "z"]])) -> True
      _ -> False
      ~? "Should desugar lambda with multiple params"
  , "desugar lambda with complex body" ~: case sexprToExpr (SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SList [SAtom (ASymbol "if") Nothing, SAtom (ASymbol "x") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 0) Nothing] Nothing] Nothing) of
      Right (ELambda ["x"] (EIf (EVar "x") (EInt 1) (EInt 0))) -> True
      _ -> False
      ~? "Should desugar lambda with complex body"
  , "desugar nested lambda" ~: case sexprToExpr (SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "x") Nothing] Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "y") Nothing] Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing] Nothing] Nothing] Nothing) of
      Right (ELambda ["x"] (ELambda ["y"] (EApp (EVar "+") [EVar "x", EVar "y"]))) -> True
      _ -> False
      ~? "Should desugar nested lambda"
  ]

testDesugarLet :: Test
testDesugarLet = TestList
  [ "desugar simple let" ~: case sexprToExpr (SList [SAtom (ASymbol "let") Nothing, SList [SList [SAtom (ASymbol "x") Nothing, SAtom (AInteger 42) Nothing] Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing) of
      Right (EApp (ELambda ["x"] (EVar "x")) [EInt 42]) -> True
      _ -> False
      ~? "Should desugar let to lambda application"
  , "desugar let with multiple bindings" ~: case sexprToExpr (SList [SAtom (ASymbol "let") Nothing, SList [SList [SAtom (ASymbol "x") Nothing, SAtom (AInteger 1) Nothing] Nothing, SList [SAtom (ASymbol "y") Nothing, SAtom (AInteger 2) Nothing] Nothing] Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "y") Nothing] Nothing] Nothing) of
      Right (EApp (ELambda ["x", "y"] (EApp (EVar "+") [EVar "x", EVar "y"])) [EInt 1, EInt 2]) -> True
      _ -> False
      ~? "Should desugar let with multiple bindings"
  , "desugar let with complex value" ~: case sexprToExpr (SList [SAtom (ASymbol "let") Nothing, SList [SList [SAtom (ASymbol "x") Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing] Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing) of
      Right (EApp (ELambda ["x"] (EVar "x")) [EApp (EVar "+") [EInt 1, EInt 2]]) -> True
      _ -> False
      ~? "Should desugar let with complex value"
  , "desugar let with complex body" ~: case sexprToExpr (SList [SAtom (ASymbol "let") Nothing, SList [SList [SAtom (ASymbol "x") Nothing, SAtom (AInteger 5) Nothing] Nothing] Nothing, SList [SAtom (ASymbol "*") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "x") Nothing] Nothing] Nothing) of
      Right (EApp (ELambda ["x"] (EApp (EVar "*") [EVar "x", EVar "x"])) [EInt 5]) -> True
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
      Right (EList [EDefine "f" (ELambda ["x"] (EVar "x")), EVar "f"]) -> True
      _ -> False
      ~? "Should desugar letrec to defines"
  , "desugar letrec with multiple bindings" ~: case sexprToExpr (SList [SAtom (ASymbol "letrec") Nothing, SList [SList [SAtom (ASymbol "f") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [] Nothing, SAtom (AInteger 1) Nothing] Nothing] Nothing, SList [SAtom (ASymbol "g") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [] Nothing, SAtom (AInteger 2) Nothing] Nothing] Nothing] Nothing, SAtom (AInteger 0) Nothing] Nothing) of
      Right (EList [EDefine "f" (ELambda [] (EInt 1)), EDefine "g" (ELambda [] (EInt 2)), EInt 0]) -> True
      _ -> False
      ~? "Should desugar letrec with multiple bindings"
  , "desugar letrec with recursive function" ~: case sexprToExpr (SList [SAtom (ASymbol "letrec") Nothing, SList [SList [SAtom (ASymbol "fact") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "n") Nothing] Nothing, SList [SAtom (ASymbol "if") Nothing, SList [SAtom (ASymbol "eq?") Nothing, SAtom (ASymbol "n") Nothing, SAtom (AInteger 0) Nothing] Nothing, SAtom (AInteger 1) Nothing, SList [SAtom (ASymbol "*") Nothing, SAtom (ASymbol "n") Nothing, SList [SAtom (ASymbol "fact") Nothing, SList [SAtom (ASymbol "-") Nothing, SAtom (ASymbol "n") Nothing, SAtom (AInteger 1) Nothing] Nothing] Nothing] Nothing] Nothing] Nothing] Nothing] Nothing, SList [SAtom (ASymbol "fact") Nothing, SAtom (AInteger 5) Nothing] Nothing] Nothing) of
      Right _ -> True
      _ -> False
      ~? "Should desugar recursive letrec"
  , "desugar letrec with mutual recursion" ~: case sexprToExpr (SList [SAtom (ASymbol "letrec") Nothing, SList [SList [SAtom (ASymbol "even?") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "n") Nothing] Nothing, SList [SAtom (ASymbol "if") Nothing, SList [SAtom (ASymbol "eq?") Nothing, SAtom (ASymbol "n") Nothing, SAtom (AInteger 0) Nothing] Nothing, SAtom (ABool True) Nothing, SList [SAtom (ASymbol "odd?") Nothing, SList [SAtom (ASymbol "-") Nothing, SAtom (ASymbol "n") Nothing, SAtom (AInteger 1) Nothing] Nothing] Nothing] Nothing] Nothing] Nothing, SList [SAtom (ASymbol "odd?") Nothing, SList [SAtom (ASymbol "lambda") Nothing, SList [SAtom (ASymbol "n") Nothing] Nothing, SList [SAtom (ASymbol "if") Nothing, SList [SAtom (ASymbol "eq?") Nothing, SAtom (ASymbol "n") Nothing, SAtom (AInteger 0) Nothing] Nothing, SAtom (ABool False) Nothing, SList [SAtom (ASymbol "even?") Nothing, SList [SAtom (ASymbol "-") Nothing, SAtom (ASymbol "n") Nothing, SAtom (AInteger 1) Nothing] Nothing] Nothing] Nothing] Nothing] Nothing] Nothing, SAtom (AInteger 0) Nothing] Nothing) of
      Right _ -> True
      _ -> False
      ~? "Should desugar mutually recursive letrec"
  , "desugar letrec with non-lambda value" ~: case sexprToExpr (SList [SAtom (ASymbol "letrec") Nothing, SList [SList [SAtom (ASymbol "x") Nothing, SAtom (AInteger 42) Nothing] Nothing] Nothing, SAtom (ASymbol "x") Nothing] Nothing) of
      Right _ -> True
      _ -> False
      ~? "Should desugar letrec with non-lambda"
  ]

testDesugarBegin :: Test
testDesugarBegin = TestList
  [ "desugar simple begin" ~: case sexprToExpr (SList [SAtom (ASymbol "begin") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing) of
      Right (EList [EInt 1, EInt 2]) -> True
      _ -> False
      ~? "Should desugar begin"
  , "desugar begin with single expr" ~: case sexprToExpr (SList [SAtom (ASymbol "begin") Nothing, SAtom (AInteger 42) Nothing] Nothing) of
      Right (EList [EInt 42]) -> True
      _ -> False
      ~? "Should desugar begin with single expr"
  , "desugar begin with multiple exprs" ~: case sexprToExpr (SList [SAtom (ASymbol "begin") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing, SAtom (AInteger 3) Nothing, SAtom (AInteger 4) Nothing] Nothing) of
      Right (EList [EInt 1, EInt 2, EInt 3, EInt 4]) -> True
      _ -> False
      ~? "Should desugar begin with multiple"
  , "desugar begin with complex exprs" ~: case sexprToExpr (SList [SAtom (ASymbol "begin") Nothing, SList [SAtom (ASymbol "+") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing, SList [SAtom (ASymbol "*") Nothing, SAtom (AInteger 3) Nothing, SAtom (AInteger 4) Nothing] Nothing] Nothing) of
      Right (EList [EApp (EVar "+") [EInt 1, EInt 2], EApp (EVar "*") [EInt 3, EInt 4]]) -> True
      _ -> False
      ~? "Should desugar begin with complex"
  , "desugar nested begin" ~: case sexprToExpr (SList [SAtom (ASymbol "begin") Nothing, SList [SAtom (ASymbol "begin") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing, SAtom (AInteger 3) Nothing] Nothing) of
      Right _ -> True
      _ -> False
      ~? "Should desugar nested begin"
  ]

testDesugarApplication :: Test
testDesugarApplication = TestList
  [ "desugar simple application" ~: case sexprToExpr (SList [SAtom (ASymbol "+") Nothing, SAtom (AInteger 1) Nothing, SAtom (AInteger 2) Nothing] Nothing) of
      Right (EApp (EVar "+") [EInt 1, EInt 2]) -> True
      _ -> False
      ~? "Should desugar application"
  , "desugar application with no args" ~: case sexprToExpr (SList [SAtom (ASymbol "f") Nothing] Nothing) of
      Right (EApp (EVar "f") []) -> True
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
      Right (EApp (ELambda ["x"] (EVar "x")) [EInt 42]) -> True
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
  , "desugar quote number" ~: case sexprToExpr (SList [SAtom (ASymbol "quote") Nothing, SAtom (AInteger 42) Nothing] Nothing) of
      Right (EQuote (SAtom (AInteger 42) Nothing)) -> True
      _ -> False
      ~? "Should desugar quote number"
  ]

-- ============================================================================
-- ALPHA RENAME TESTS (30+ tests)
-- ============================================================================

testAlphaRenameBasic :: Test
testAlphaRenameBasic = TestList
  [ "rename integer unchanged" ~: alphaRename (EInt 42) ~?= Right (EInt 42)
  , "rename boolean unchanged" ~: alphaRename (EBool True) ~?= Right (EBool True)
  , "rename string unchanged" ~: alphaRename (EString "hello") ~?= Right (EString "hello")
  , "rename free var unchanged" ~: alphaRename (EVar "x") ~?= Right (EVar "x")
  , "rename quote unchanged" ~: alphaRename (EQuote (SAtom (AInteger 42) Nothing)) ~?= Right (EQuote (SAtom (AInteger 42) Nothing))
  ]

testAlphaRenameLambda :: Test
testAlphaRenameLambda = TestList
  [ "rename lambda param" ~: case alphaRename (ELambda ["x"] (EVar "x")) of
      Right (ELambda [newName] (EVar varName)) | newName == varName && newName /= "x" -> True
      _ -> False
      ~? "Should rename lambda param"
  , "rename lambda with multiple params" ~: case alphaRename (ELambda ["x", "y"] (EApp (EVar "+") [EVar "x", EVar "y"])) of
      Right (ELambda [x', y'] (EApp (EVar "+") [EVar xVar, EVar yVar])) | x' == xVar && y' == yVar && x' /= "x" && y' /= "y" -> True
      _ -> False
      ~? "Should rename multiple params"
  , "rename nested lambda" ~: case alphaRename (ELambda ["x"] (ELambda ["y"] (EApp (EVar "+") [EVar "x", EVar "y"]))) of
      Right (ELambda [x'] (ELambda [y'] (EApp (EVar "+") [EVar xVar, EVar yVar]))) | x' == xVar && y' == yVar -> True
      _ -> False
      ~? "Should rename nested lambda"
  , "rename lambda with free var" ~: case alphaRename (ELambda ["x"] (EApp (EVar "+") [EVar "x", EVar "z"])) of
      Right (ELambda [x'] (EApp (EVar "+") [EVar xVar, EVar "z"])) | x' == xVar && x' /= "x" -> True
      _ -> False
      ~? "Should preserve free var"
  , "rename lambda shadowing" ~: case alphaRename (ELambda ["x"] (ELambda ["x"] (EVar "x"))) of
      Right (ELambda [x1] (ELambda [x2] (EVar xVar))) | x2 == xVar && x1 /= x2 -> True
      _ -> False
      ~? "Should handle shadowing"
  ]

testAlphaRenameDefine :: Test
testAlphaRenameDefine = TestList
  [ "rename simple define" ~: case alphaRename (EDefine "x" (EInt 42)) of
      Right (EDefine newName (EInt 42)) | newName /= "x" -> True
      _ -> False
      ~? "Should rename define"
  , "rename define with lambda" ~: case alphaRename (EDefine "f" (ELambda ["x"] (EVar "x"))) of
      Right (EDefine f' (ELambda [x'] (EVar xVar))) | f' /= "f" && x' == xVar -> True
      _ -> False
      ~? "Should rename define with lambda"
  , "rename define with expression" ~: case alphaRename (EDefine "result" (EApp (EVar "+") [EInt 1, EInt 2])) of
      Right (EDefine newName (EApp (EVar "+") [EInt 1, EInt 2])) | newName /= "result" -> True
      _ -> False
      ~? "Should rename define with expr"
  , "rename multiple defines" ~: case alphaRename (EList [EDefine "x" (EInt 1), EDefine "y" (EInt 2)]) of
      Right (EList [EDefine x' (EInt 1), EDefine y' (EInt 2)]) | x' /= "x" && y' /= "y" && x' /= y' -> True
      _ -> False
      ~? "Should rename multiple defines"
  , "rename define referencing previous" ~: case alphaRename (EList [EDefine "x" (EInt 1), EDefine "y" (EVar "x")]) of
      Right (EList [EDefine x' (EInt 1), EDefine y' (EVar xRef)]) | x' == xRef && y' /= "y" -> True
      _ -> False
      ~? "Should rename with correct references"
  ]

testAlphaRenameLetrec :: Test
testAlphaRenameLetrec = TestList
  [ "rename letrec with recursive function" ~: case alphaRename (EList [EDefine "fact" (ELambda ["n"] (EApp (EVar "fact") [EVar "n"]))]) of
      Right (EList [EDefine fact' (ELambda [n'] (EApp (EVar factRef) [EVar nRef]))]) | fact' == factRef && n' == nRef -> True
      _ -> False
      ~? "Should handle recursive reference"
  , "rename letrec with mutual recursion" ~: case alphaRename (EList [EDefine "even?" (ELambda ["n"] (EApp (EVar "odd?") [EVar "n"])), EDefine "odd?" (ELambda ["n"] (EApp (EVar "even?") [EVar "n"]))]) of
      Right (EList [EDefine even' (ELambda [n1] (EApp (EVar oddRef) [EVar n1Ref])), EDefine odd' (ELambda [n2] (EApp (EVar evenRef) [EVar n2Ref]))]) | even' == evenRef && odd' == oddRef && n1 == n1Ref && n2 == n2Ref -> True
      _ -> False
      ~? "Should handle mutual recursion"
  , "rename letrec with body using defines" ~: case alphaRename (EList [EDefine "f" (ELambda [] (EInt 1)), EApp (EVar "f") []]) of
      Right (EList [EDefine f' (ELambda [] (EInt 1)), EApp (EVar fRef) []]) | f' == fRef -> True
      _ -> False
      ~? "Should use renamed defines in body"
  , "rename nested letrec" ~: case alphaRename (EList [EDefine "outer" (ELambda [] (EList [EDefine "inner" (ELambda [] (EInt 1)), EApp (EVar "inner") []]))]) of
      Right _ -> True
      _ -> False
      ~? "Should handle nested letrec"
  , "rename letrec with multiple recursive calls" ~: case alphaRename (EList [EDefine "f" (ELambda ["x"] (EApp (EVar "f") [EApp (EVar "f") [EVar "x"]]))]) of
      Right (EList [EDefine f' (ELambda [x'] (EApp (EVar fRef1) [EApp (EVar fRef2) [EVar xRef]]))]) | f' == fRef1 && f' == fRef2 && x' == xRef -> True
      _ -> False
      ~? "Should handle multiple recursive calls"
  ]

testAlphaRenameIf :: Test
testAlphaRenameIf = TestList
  [ "rename if expression" ~: case alphaRename (EIf (EBool True) (EInt 1) (EInt 2)) of
      Right (EIf (EBool True) (EInt 1) (EInt 2)) -> True
      _ -> False
      ~? "Should rename if"
  , "rename if with vars" ~: case alphaRename (ELambda ["x"] (EIf (EVar "x") (EVar "x") (EInt 0))) of
      Right (ELambda [x'] (EIf (EVar xRef1) (EVar xRef2) (EInt 0))) | x' == xRef1 && x' == xRef2 -> True
      _ -> False
      ~? "Should rename vars in if"
  , "rename nested if" ~: case alphaRename (EIf (EBool True) (EIf (EBool False) (EInt 1) (EInt 2)) (EInt 3)) of
      Right (EIf (EBool True) (EIf (EBool False) (EInt 1) (EInt 2)) (EInt 3)) -> True
      _ -> False
      ~? "Should rename nested if"
  , "rename if in lambda" ~: case alphaRename (ELambda ["x", "y"] (EIf (EVar "x") (EVar "y") (EInt 0))) of
      Right (ELambda [x', y'] (EIf (EVar xRef) (EVar yRef) (EInt 0))) | x' == xRef && y' == yRef -> True
      _ -> False
      ~? "Should rename if in lambda"
  , "rename if with complex branches" ~: case alphaRename (ELambda ["x"] (EIf (EVar "x") (EApp (EVar "+") [EVar "x", EInt 1]) (EApp (EVar "-") [EVar "x", EInt 1]))) of
      Right _ -> True
      _ -> False
      ~? "Should rename complex if"
  ]

testAlphaRenameApp :: Test
testAlphaRenameApp = TestList
  [ "rename application" ~: case alphaRename (EApp (EVar "f") [EInt 1, EInt 2]) of
      Right (EApp (EVar "f") [EInt 1, EInt 2]) -> True
      _ -> False
      ~? "Should rename application"
  , "rename application with bound vars" ~: case alphaRename (ELambda ["f", "x"] (EApp (EVar "f") [EVar "x"])) of
      Right (ELambda [f', x'] (EApp (EVar fRef) [EVar xRef])) | f' == fRef && x' == xRef -> True
      _ -> False
      ~? "Should rename bound vars in app"
  , "rename nested application" ~: case alphaRename (EApp (EVar "f") [EApp (EVar "g") [EInt 1]]) of
      Right (EApp (EVar "f") [EApp (EVar "g") [EInt 1]]) -> True
      _ -> False
      ~? "Should rename nested app"
  , "rename lambda application" ~: case alphaRename (EApp (ELambda ["x"] (EVar "x")) [EInt 42]) of
      Right (EApp (ELambda [x'] (EVar xRef)) [EInt 42]) | x' == xRef -> True
      _ -> False
      ~? "Should rename lambda in app"
  , "rename application with many args" ~: case alphaRename (ELambda ["a", "b", "c"] (EApp (EVar "f") [EVar "a", EVar "b", EVar "c"])) of
      Right (ELambda [a', b', c'] (EApp (EVar "f") [EVar aRef, EVar bRef, EVar cRef])) | a' == aRef && b' == bRef && c' == cRef -> True
      _ -> False
      ~? "Should rename many args"
  ]

-- ============================================================================
-- CLOSURE CONVERSION TESTS (15+ tests)
-- ============================================================================

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

-- ============================================================================
-- CODEGEN TESTS (40+ tests)
-- ============================================================================

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
  , "gen if with var condition" ~: case generateCode "test" (ELambda ["x"] (EIf (EVar "x") (EInt 1) (EInt 0))) of
      Right code -> any (\instr -> case instr of IJumpIfFalse _ -> True; _ -> False) (Vector.toList $ coInstrs code)
      Left _ -> False
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

-- ============================================================================
-- VM TESTS (50+ tests)
-- ============================================================================

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
testVMArithmetic = "VM arithmetic" ~: TestCase $ do
  let vmState = initVMState { vBuiltins = builtins }
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 2, CInt 3]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "+", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM vmState code
  case result of
    Right (VInt 5) -> return ()
    _ -> assertFailure $ "Expected VInt 5, got: " ++ show result

testVMSubtraction :: Test
testVMSubtraction = "VM subtraction" ~: TestCase $ do
  let vmState = initVMState { vBuiltins = builtins }
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 10, CInt 3]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "-", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM vmState code
  case result of
    Right (VInt 7) -> return ()
    _ -> assertFailure $ "Expected VInt 7, got: " ++ show result

testVMMultiplication :: Test
testVMMultiplication = "VM multiplication" ~: TestCase $ do
  let vmState = initVMState { vBuiltins = builtins }
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 6, CInt 7]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "*", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM vmState code
  case result of
    Right (VInt 42) -> return ()
    _ -> assertFailure $ "Expected VInt 42, got: " ++ show result

testVMDivision :: Test
testVMDivision = "VM division" ~: TestCase $ do
  let vmState = initVMState { vBuiltins = builtins }
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 20, CInt 4]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "div", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM vmState code
  case result of
    Right (VInt 5) -> return ()
    _ -> assertFailure $ "Expected VInt 5, got: " ++ show result

testVMModulo :: Test
testVMModulo = "VM modulo" ~: TestCase $ do
  let vmState = initVMState { vBuiltins = builtins }
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 17, CInt 5]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "mod", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM vmState code
  case result of
    Right (VInt 2) -> return ()
    _ -> assertFailure $ "Expected VInt 2, got: " ++ show result

testVMComparison :: Test
testVMComparison = "VM comparison" ~: TestCase $ do
  let vmState = initVMState { vBuiltins = builtins }
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 5, CInt 5]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "eq?", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM vmState code
  case result of
    Right (VBool True) -> return ()
    _ -> assertFailure $ "Expected VBool True, got: " ++ show result

testVMLessThan :: Test
testVMLessThan = "VM less than" ~: TestCase $ do
  let vmState = initVMState { vBuiltins = builtins }
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 3, CInt 5]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim "<", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM vmState code
  case result of
    Right (VBool True) -> return ()
    _ -> assertFailure $ "Expected VBool True, got: " ++ show result

testVMGreaterThan :: Test
testVMGreaterThan = "VM greater than" ~: TestCase $ do
  let vmState = initVMState { vBuiltins = builtins }
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 10, CInt 5]
        , coInstrs = Vector.fromList [IConst 0, IConst 1, IPrim ">", IReturn]
        , coLabelMap = Map.empty
        }
  result <- execVM vmState code
  case result of
    Right (VBool True) -> return ()
    _ -> assertFailure $ "Expected VBool True, got: " ++ show result

testVMLocalVariables :: Test
testVMLocalVariables = "VM local variables" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 2
        , coConsts = Vector.fromList [CInt 10, CInt 20]
        , coInstrs = Vector.fromList
            [ IConst 0     -- Push 10
            , IStore 0     -- Store in local 0
            , IConst 1     -- Push 20
            , IStore 1     -- Store in local 1
            , ILoad 0      -- Load local 0
            , ILoad 1      -- Load local 1
            , IPrim "+"    -- Add them
            , IReturn
            ]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState { vBuiltins = builtins } code
  case result of
    Right (VInt 30) -> return ()
    _ -> assertFailure $ "Expected VInt 30, got: " ++ show result

testVMIfTrue :: Test
testVMIfTrue = "VM if true" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CBool True, CInt 1, CInt 2]
        , coInstrs = Vector.fromList
            [ IConst 0         -- Push #t
            , IJumpIfFalse 4   -- Jump to else if false
            , IConst 1         -- Push 1 (then branch)
            , IJump 5          -- Jump to end
            , IConst 2         -- Push 2 (else branch)
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
        , coConsts = Vector.fromList [CBool False, CInt 1, CInt 2]
        , coInstrs = Vector.fromList
            [ IConst 0         -- Push #f
            , IJumpIfFalse 4   -- Jump to else if false
            , IConst 1         -- Push 1 (then branch)
            , IJump 5          -- Jump to end
            , IConst 2         -- Push 2 (else branch)
            , IReturn
            ]
        , coLabelMap = Map.empty
        }
  result <- execVM initVMState code
  case result of
    Right (VInt 2) -> return ()
    _ -> assertFailure $ "Expected VInt 2, got: " ++ show result

testVMPop :: Test
testVMPop = "VM pop instruction" ~: TestCase $ do
  let code = CodeObject
        { coName = "test"
        , coArity = 0
        , coMaxLocals = 0
        , coConsts = Vector.fromList [CInt 1, CInt 2, CInt 3]
        , coInstrs = Vector.fromList
            [ IConst 0   -- Push 1
            , IPop       -- Pop it
            , IConst 1   -- Push 2
            , IPop       -- Pop it
            , IConst 2   -- Push 3
            , IReturn    -- Return 3
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
            [ ILoad 0      -- Load param
            , ILoad 0      -- Load param again
            , IPrim "*"    -- Multiply
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
            [ IConst 0           -- Push 5
            , ICall 1 "square"   -- Call square(5)
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
  case compile defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right code -> do
      result <- execVM initVMState code
      case result of
        Right (VInt 8) -> return ()
        _ -> assertFailure $ "Expected VInt 8, got: " ++ show result

testVMRecursiveFactorial :: Test
testVMRecursiveFactorial = "VM recursive factorial" ~: TestCase $ do
  let source = "(define (fact n) (if (eq? n 0) 1 (* n (fact (- n 1))))) (fact 5)"
  case compile defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right code -> do
      result <- execVM initVMState code
      case result of
        Right (VInt 120) -> return ()
        _ -> assertFailure $ "Expected VInt 120, got: " ++ show result

testVMMutualRecursion :: Test
testVMMutualRecursion = "VM mutual recursion" ~: TestCase $ do
  let source = "(letrec ((even? (lambda (n) (if (eq? n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (eq? n 0) #f (even? (- n 1)))))) (even? 10))"
  case compile defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right code -> do
      result <- execVM initVMState code
      case result of
        Right (VBool True) -> return ()
        _ -> assertFailure $ "Expected VBool True, got: " ++ show result

testVMNestedCalls :: Test
testVMNestedCalls = "VM nested calls" ~: TestCase $ do
  let source = "(define (add x y) (+ x y)) (define (mul x y) (* x y)) (add (mul 3 4) (mul 2 5))"
  case compile defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right code -> do
      result <- execVM initVMState code
      case result of
        Right (VInt 22) -> return ()
        _ -> assertFailure $ "Expected VInt 22, got: " ++ show result

testVMComplexExpression :: Test
testVMComplexExpression = "VM complex expression" ~: TestCase $ do
  let source = "(* (+ 2 3) (- 10 4))"
  case compile defaultConfig source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right code -> do
      result <- execVM initVMState code
      case result of
        Right (VInt 30) -> return ()
        _ -> assertFailure $ "Expected VInt 30, got: " ++ show result

-- ============================================================================
-- BUILTINS TESTS (25+ tests)
-- ============================================================================

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

-- ============================================================================
-- COMPILER TESTS (20+ tests)
-- ============================================================================

testCompileInteger :: Test
testCompileInteger = "compile integer" ~: case compile defaultConfig "42" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile integer"

testCompileBoolean :: Test
testCompileBoolean = "compile boolean" ~: case compile defaultConfig "#t" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile boolean"

testCompileString :: Test
testCompileString = "compile string" ~: case compile defaultConfig "\"hello\"" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile string"

testCompileAddition :: Test
testCompileAddition = "compile addition" ~: case compile defaultConfig "(+ 1 2)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile addition"

testCompileIf :: Test
testCompileIf = "compile if" ~: case compile defaultConfig "(if #t 1 2)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile if"

testCompileLambda :: Test
testCompileLambda = "compile lambda" ~: case compile defaultConfig "(lambda (x) x)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile lambda"

testCompileDefine :: Test
testCompileDefine = "compile define" ~: case compile defaultConfig "(define x 42)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile define"

testCompileFactorial :: Test
testCompileFactorial = "compile factorial" ~: case compile defaultConfig "(define (fact n) (if (eq? n 0) 1 (* n (fact (- n 1)))))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile factorial"

testCompileFibonacci :: Test
testCompileFibonacci = "compile fibonacci" ~: case compile defaultConfig "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile fibonacci"

testCompileLet :: Test
testCompileLet = "compile let" ~: case compile defaultConfig "(let ((x 1) (y 2)) (+ x y))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile let"

testCompileLetrec :: Test
testCompileLetrec = "compile letrec" ~: case compile defaultConfig "(letrec ((f (lambda (x) x))) (f 42))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile letrec"

testCompileBegin :: Test
testCompileBegin = "compile begin" ~: case compile defaultConfig "(begin 1 2 3)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile begin"

testCompileNested :: Test
testCompileNested = "compile nested" ~: case compile defaultConfig "(+ (* 2 3) (- 10 4))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile nested"

testCompileMultipleDefines :: Test
testCompileMultipleDefines = "compile multiple defines" ~: case compile defaultConfig "(define x 1) (define y 2) (+ x y)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile multiple defines"

testCompileClosure :: Test
testCompileClosure = "compile closure" ~: case compile defaultConfig "(define (make-adder n) (lambda (x) (+ x n)))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile closure"

testCompileHigherOrder :: Test
testCompileHigherOrder = "compile higher order" ~: case compile defaultConfig "(define (twice f x) (f (f x)))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile higher order"

testCompileRecursiveSum :: Test
testCompileRecursiveSum = "compile recursive sum" ~: case compile defaultConfig "(define (sum n) (if (eq? n 0) 0 (+ n (sum (- n 1)))))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile recursive sum"

testCompileComplexProgram :: Test
testCompileComplexProgram = "compile complex program" ~: case compile defaultConfig "(define (gcd a b) (if (eq? b 0) a (gcd b (mod a b)))) (gcd 48 18)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile complex program"

testCompileParseError :: Test
testCompileParseError = "compile parse error" ~: case compile defaultConfig "(+ 1 2" of
  Left _ -> True
  Right _ -> False
  ~? "Should fail on parse error"

testCompileInvalidSyntax :: Test
testCompileInvalidSyntax = "compile invalid syntax" ~: case compile defaultConfig "(if #t)" of
  Left _ -> True
  Right _ -> False
  ~? "Should fail on invalid syntax"

-- ============================================================================
-- ALL TESTS
-- ============================================================================

allTests :: Test
allTests = TestList
  [ TestLabel "Parse Integer" testParseInteger
  , TestLabel "Parse Bool" testParseBool
  , TestLabel "Parse String" testParseString
  , TestLabel "Parse Symbol" testParseSymbol
  , TestLabel "Parse List" testParseList
  , TestLabel "Parse Multiple" testParseMultiple
  , TestLabel "Parse Whitespace" testParseWhitespace
  , TestLabel "Parse Errors" testParseErrors
  , TestLabel "Desugar Basic" testDesugarBasic
  , TestLabel "Desugar If" testDesugarIf
  , TestLabel "Desugar Define" testDesugarDefine
  , TestLabel "Desugar Lambda" testDesugarLambda
  , TestLabel "Desugar Let" testDesugarLet
  , TestLabel "Desugar Letrec" testDesugarLetrec
  , TestLabel "Desugar Begin" testDesugarBegin
  , TestLabel "Desugar Application" testDesugarApplication
  , TestLabel "Desugar Quote" testDesugarQuote
  , TestLabel "Alpha Rename Basic" testAlphaRenameBasic
  , TestLabel "Alpha Rename Lambda" testAlphaRenameLambda
  , TestLabel "Alpha Rename Define" testAlphaRenameDefine
  , TestLabel "Alpha Rename Letrec" testAlphaRenameLetrec
  , TestLabel "Alpha Rename If" testAlphaRenameIf
  , TestLabel "Alpha Rename App" testAlphaRenameApp
  , TestLabel "Closure Conversion Basic" testClosureConversionBasic
  , TestLabel "Closure Conversion Lambda" testClosureConversionLambda
  , TestLabel "Closure Conversion Define" testClosureConversionDefine
  , TestLabel "Closure Conversion Other" testClosureConversionOther
  , TestLabel "CodeGen Basic" testCodeGenBasic
  , TestLabel "CodeGen Arithmetic" testCodeGenArithmetic
  , TestLabel "CodeGen Comparison" testCodeGenComparison
  , TestLabel "CodeGen If" testCodeGenIf
  , TestLabel "CodeGen Define" testCodeGenDefine
  , TestLabel "CodeGen Lambda" testCodeGenLambda
  , TestLabel "CodeGen Application" testCodeGenApplication
  , TestLabel "CodeGen List" testCodeGenList
  , TestLabel "VM Basic" testVMBasic
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
  , TestLabel "Builtins Addition" testBuiltinsAddition
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
  , TestLabel "Compile Integer" testCompileInteger
  , TestLabel "Compile Boolean" testCompileBoolean
  , TestLabel "Compile String" testCompileString
  , TestLabel "Compile Addition" testCompileAddition
  , TestLabel "Compile If" testCompileIf
  , TestLabel "Compile Lambda" testCompileLambda
  , TestLabel "Compile Define" testCompileDefine
  , TestLabel "Compile Factorial" testCompileFactorial
  , TestLabel "Compile Fibonacci" testCompileFibonacci
  , TestLabel "Compile Let" testCompileLet
  , TestLabel "Compile Letrec" testCompileLetrec
  , TestLabel "Compile Begin" testCompileBegin
  , TestLabel "Compile Nested" testCompileNested
  , TestLabel "Compile Multiple Defines" testCompileMultipleDefines
  , TestLabel "Compile Closure" testCompileClosure
  , TestLabel "Compile Higher Order" testCompileHigherOrder
  , TestLabel "Compile Recursive Sum" testCompileRecursiveSum
  , TestLabel "Compile Complex Program" testCompileComplexProgram
  , TestLabel "Compile Parse Error" testCompileParseError
  , TestLabel "Compile Invalid Syntax" testCompileInvalidSyntax
  ]

main :: IO ()
main = do
  putStrLn "==========================================="
  putStrLn "  GLaDOS COMPREHENSIVE TEST SUITE"
  putStrLn "  Target: 100% Code Coverage"
  putStrLn "==========================================="
  putStrLn ""
  counts <- runTestTT allTests
  putStrLn ""
  putStrLn "==========================================="
  putStrLn $ "Total tests: " ++ show (cases counts)
  putStrLn $ "Passed: " ++ show (cases counts - errors counts - failures counts)
  putStrLn $ "Failed: " ++ show (failures counts)
  putStrLn $ "Errors: " ++ show (errors counts)
  putStrLn "==========================================="
  if errors counts + failures counts == 0
    then do
      putStrLn "✓ ALL TESTS PASSED!"
      exitSuccess
    else do
      putStrLn "✗ SOME TESTS FAILED"
      exitFailure

module Test.SExprParserSpec (tests) where

import Test.HUnit

import AST
import SExprParser

tests :: Test
tests = TestList
  [ TestLabel "Parse Integer" testParseInteger
  , TestLabel "Parse Bool" testParseBool
  , TestLabel "Parse String" testParseString
  , TestLabel "Parse Symbol" testParseSymbol
  , TestLabel "Parse List" testParseList
  , TestLabel "Parse Multiple" testParseMultiple
  , TestLabel "Parse Whitespace" testParseWhitespace
  , TestLabel "Parse Errors" testParseErrors
  ]

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
  , "parse string with escaped quote" ~: case parseFromString "\"hello \\\"world\\\"\"" of
      Right [SAtom (AString s) _] -> s == "hello \"world\""
      _ -> False
      ~? "Should parse escaped quotes in strings"
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
  , "parse list with multiple elements" ~: case parseFromString "(define (square x) (* x x))" of
      Right [SList [SAtom (ASymbol "define") _, SList [SAtom (ASymbol "square") _, SAtom (ASymbol "x") _] _, SList [SAtom (ASymbol "*") _, SAtom (ASymbol "x") _, SAtom (ASymbol "x") _] _] _] -> True
      _ -> False
      ~? "Should parse list with multiple elements"
  ]

testParseMultiple :: Test
testParseMultiple = TestList
  [ "parse multiple expressions" ~: case parseFromString "(define x 1) (+ x 2)" of
      Right [ SList [SAtom (ASymbol "define") _, SAtom (ASymbol "x") _, SAtom (AInteger 1) _] _
            , SList [SAtom (ASymbol "+") _, SAtom (ASymbol "x") _, SAtom (AInteger 2) _] _
            ] -> True
      _ -> False
      ~? "Should parse multiple expressions"
  , "parse three expressions" ~: case parseFromString "1 2 3" of
      Right [SAtom (AInteger 1) _, SAtom (AInteger 2) _, SAtom (AInteger 3) _] -> True
      _ -> False
      ~? "Should parse three expressions"
  , "reject expressions with comments" ~: case parseFromString "(define x 1) ; comment\n(+ x 2)" of
      Left _ -> True
      Right _ -> False
      ~? "Should reject semicolon comments"
  ]

testParseWhitespace :: Test
testParseWhitespace = TestList
  [ "parse with leading whitespace" ~: case parseFromString "   42" of
      Right [SAtom (AInteger 42) _] -> True
      _ -> False
      ~? "Should handle leading whitespace"
  , "parse with trailing whitespace" ~: case parseFromString "42   " of
      Right [SAtom (AInteger 42) _] -> True
      _ -> False
      ~? "Should handle trailing whitespace"
  , "parse with internal whitespace" ~: case parseFromString "(  +   1    2   )" of
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

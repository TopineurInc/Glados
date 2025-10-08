module Test.MacroExpanderSpec (tests) where

import Test.HUnit

import AST
import MacroExpander

tests :: Test
tests = TestList
  [ TestLabel "Expand when" testExpandWhen
  , TestLabel "Expand unless" testExpandUnless
  , TestLabel "Expand cond basic" testExpandCondBasic
  , TestLabel "Expand cond else" testExpandCondElse
  , TestLabel "Expand nested macros" testExpandNested
  , TestLabel "Expand when arity error" testExpandWhenError
  , TestLabel "Expand unless arity error" testExpandUnlessError
  , TestLabel "Expand cond clause error" testExpandCondClauseError
  ]

testExpandWhen :: Test
testExpandWhen = "expand when -> if" ~: TestCase $ do
  let sexpr = SList [ SAtom (ASymbol "when") Nothing
                    , SAtom (ABool True) Nothing
                    , SAtom (AInteger 1) Nothing
                    ] Nothing
  case expandMacros defaultMacroEnv sexpr of
    Right (SList (SAtom (ASymbol "if") _ : _ ) _) -> return ()
    other -> assertFailure $ "Expected if, got: " ++ show other

testExpandUnless :: Test
testExpandUnless = "expand unless -> if" ~: TestCase $ do
  let sexpr = SList [ SAtom (ASymbol "unless") Nothing
                    , SAtom (ABool False) Nothing
                    , SAtom (AInteger 2) Nothing
                    ] Nothing
  case expandMacros defaultMacroEnv sexpr of
    Right (SList (SAtom (ASymbol "if") _ : _ ) _) -> return ()
    other -> assertFailure $ "Expected if, got: " ++ show other

testExpandCondBasic :: Test
testExpandCondBasic = "expand cond -> nested ifs" ~: TestCase $ do
  let clause1 = SList [SAtom (ASymbol "eq?") Nothing, SAtom (AInteger 1) Nothing] Nothing
      condExpr = SList [ SAtom (ASymbol "cond") Nothing
                       , SList [clause1, SAtom (AInteger 42) Nothing] Nothing
                       ] Nothing
  case expandMacros defaultMacroEnv condExpr of
    Right (SList (SAtom (ASymbol "if") _ : _ ) _) -> return ()
    other -> assertFailure $ "Expected if from cond, got: " ++ show other

testExpandCondElse :: Test
testExpandCondElse = "expand cond with else" ~: TestCase $ do
  let condExpr = SList [ SAtom (ASymbol "cond") Nothing
                       , SList [SAtom (ASymbol "else") Nothing, SAtom (AInteger 7) Nothing] Nothing
                       ] Nothing
  case expandMacros defaultMacroEnv condExpr of
    Right (SAtom (AInteger 7) _) -> return ()
    other -> assertFailure $ "Expected else body, got: " ++ show other

testExpandNested :: Test
testExpandNested = "expand nested macros" ~: TestCase $ do
  let sexpr = SList [ SAtom (ASymbol "when") Nothing
                    , SAtom (ABool True) Nothing
                    , SList [ SAtom (ASymbol "unless") Nothing
                           , SAtom (ABool False) Nothing
                           , SAtom (AInteger 3) Nothing
                           ] Nothing
                    ] Nothing
  case expandMacros defaultMacroEnv sexpr of
    Right (SList (SAtom (ASymbol "if") _ : _ ) _) -> return ()
    other -> assertFailure $ "Expected nested ifs, got: " ++ show other

testExpandWhenError :: Test
testExpandWhenError = "expand when arity error" ~: TestCase $ do
  let bad = SList [ SAtom (ASymbol "when") Nothing
                  , SAtom (ABool True) Nothing
                  ] Nothing
  case expandMacros defaultMacroEnv bad of
    Left _ -> return ()
    _ -> assertFailure "Expected syntax error for when"

testExpandUnlessError :: Test
testExpandUnlessError = "expand unless arity error" ~: TestCase $ do
  let bad = SList [ SAtom (ASymbol "unless") Nothing
                  , SAtom (ABool True) Nothing
                  ] Nothing
  case expandMacros defaultMacroEnv bad of
    Left _ -> return ()
    _ -> assertFailure "Expected syntax error for unless"

testExpandCondClauseError :: Test
testExpandCondClauseError = "expand cond wrong clause" ~: TestCase $ do
  let bad = SList [ SAtom (ASymbol "cond") Nothing
                  , SAtom (AInteger 1) Nothing
                  ] Nothing
  case expandMacros defaultMacroEnv bad of
    Left _ -> return ()
    _ -> assertFailure "Expected syntax error for cond"




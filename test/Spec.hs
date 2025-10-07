module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import qualified Test.AlphaRenameSpec as AlphaRenameSpec
import qualified Test.BuiltinsSpec as BuiltinsSpec
import qualified Test.ClosureConversionSpec as ClosureConversionSpec
import qualified Test.CodeGenSpec as CodeGenSpec
import qualified Test.CompilerSpec as CompilerSpec
import qualified Test.DesugarSpec as DesugarSpec
import qualified Test.SExprParserSpec as SExprParserSpec
import qualified Test.VMSpec as VMSpec

allTests :: Test
allTests = TestList
  [ TestLabel "SExpr Parser" SExprParserSpec.tests
  , TestLabel "Desugar" DesugarSpec.tests
  , TestLabel "Alpha Rename" AlphaRenameSpec.tests
  , TestLabel "Closure Conversion" ClosureConversionSpec.tests
  , TestLabel "CodeGen" CodeGenSpec.tests
  , TestLabel "VM" VMSpec.tests
  , TestLabel "Builtins" BuiltinsSpec.tests
  , TestLabel "Compiler" CompilerSpec.tests
  ]

main :: IO ()
main = do
  putStrLn "==========================================="
  putStrLn "  GLaDOS COMPREHENSIVE TEST SUITE"
  putStrLn "  Target: 100% Code Coverage"
  putStrLn "==========================================="
  putStrLn ""
  summary <- runTestTT allTests
  putStrLn ""
  putStrLn "==========================================="
  putStrLn $ "Total tests: " ++ show (cases summary)
  putStrLn $ "Passed: " ++ show (cases summary - errors summary - failures summary)
  putStrLn $ "Failed: " ++ show (failures summary)
  putStrLn $ "Errors: " ++ show (errors summary)
  putStrLn "==========================================="
  if errors summary + failures summary == 0
    then do
      putStrLn "✓ ALL TESTS PASSED!"
      exitSuccess
    else do
      putStrLn "✗ SOME TESTS FAILED"
      exitFailure

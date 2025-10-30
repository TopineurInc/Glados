{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Tests for Topineur type parser
-}

module Test.TopineurTypeSpec (tests) where

import TopineurParser
import Test.HUnit

(-->) :: (Eq a, Show a) => Either e a -> a -> Test
(-->) r expected = case r of
  Right v -> TestCase (v @?= expected)
  Left _  -> TestCase (assertFailure "Unexpected parse error")

boolTrue :: Bool -> Test
boolTrue b = TestCase (assertBool "predicate failed" b)

tests :: Test
tests = TestLabel "TopineurType" $ TestList
  [ "primitive Int" ~:
      parseTypeFromString "Int" --> TInt
  , "primitive Float" ~:
      parseTypeFromString "Float" --> TFloat
  , "primitive Bool" ~:
      parseTypeFromString "Bool" --> TBool
  , "primitive String" ~:
      parseTypeFromString "String" --> TString
  , "unit" ~:
      parseTypeFromString "Unit" --> TUnit
  , "List[Int]" ~:
      parseTypeFromString "List[Int]" --> TList TInt
  , "nested List[List[String]]" ~:
      parseTypeFromString "List[List[String]]" --> TList (TList TString)
  , "Tuple[Int, String]" ~:
      parseTypeFromString "Tuple[Int, String]" --> TTuple [TInt, TString]
  , "Tuple[Int,String,Bool]" ~:
      parseTypeFromString "Tuple[Int,String,Bool]" --> TTuple [TInt, TString, TBool]
  , "Generic Some[T]" ~:
      parseTypeFromString "Some[T]" --> TCustom "Some" [TVar "T"]
  , "Custom Polygon" ~:
      parseTypeFromString "Polygon" --> TCustom "Polygon" []
  , "Custom Point with param" ~:
      parseTypeFromString "Option[Point]" --> TCustom "Option" [TCustom "Point" []]
  , "Whitespace tolerance" ~:
      parseTypeFromString "  Tuple [ Int , String ]  " --> TTuple [TInt, TString]
  , "Invalid missing bracket" ~:
      TestCase $ case parseTypeFromString "List[Int" of
        Left _  -> return ()
        Right _ -> assertFailure "Should fail on missing bracket"
  ]

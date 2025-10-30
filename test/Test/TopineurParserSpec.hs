{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Tests for TopineurParser (minimal)
-}

module Test.TopineurParserSpec (tests) where

import TopineurParser
import Test.HUnit

tests :: Test
tests = TestLabel "TopineurParser" $ TestList
      [ "parse package header" ~:
                  let input = "package showcase\n" in
                  case parseTopineurFromString input of
                        Right m -> topPackage m == Just "showcase"
                        _       -> False
                  ~? "Should parse package header"
      , "tolerate banner comments" ~:
                  let input = unlines [
                                          "package showcase"
                                    , ""
                                    , "|- banner"
                                    , "|- another line"
                                    ] in
                  case parseTopineurFromString input of
                        Right m -> topPackage m == Just "showcase"
                        _       -> False
                  ~? "Should parse with banner comments"
      , "no package header yields Nothing" ~:
                  let input = "|- just comments without package\n" in
                  case parseTopineurFromString input of
                        Right m -> topPackage m == Nothing
                        _       -> False
                  ~? "Should return Nothing when no package is present"
      ]

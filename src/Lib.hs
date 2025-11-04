{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Lib - Utility module for development/testing
-}

module Lib (someFunc) where

import SExprConstruct (sourceFromSExprs)
import SExprParser (parseFromString)

-- | Test function for parsing and reconstructing S-expressions
someFunc :: IO ()
someFunc = do
  contents <- readFile "ok.lsp"
  either print (putStr . sourceFromSExprs) $ parseFromString contents

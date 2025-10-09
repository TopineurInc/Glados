{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Lib
-}

module Lib
    ( someFunc
    ) where

import SExprParser (parseFromString)
import SExprConstruct (sourceFromSExprs)

someFunc :: IO ()
someFunc = do
  contents <- readFile "ok.lsp"
  case parseFromString contents of
    Left err -> print err
    Right exprs -> putStr $ sourceFromSExprs exprs

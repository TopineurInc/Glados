{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/Lib.hs
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

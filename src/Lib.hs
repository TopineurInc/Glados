{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Lib
-}

module Lib
    ( someFunc
    ) where

import AST

someFunc :: IO ()
someFunc = do
  let pos = SourcePos 1 1
      sexpr = SList [SAtom (ASymbol "factorial") (Just pos), SAtom (AInteger 42) (Just pos)] (Just pos)
      ast = EDefine "fact" (ELambda ["x"] (EIf (EApp (EVar "eq?") [EVar "x", EInt 1]) (EInt 1) (EApp (EVar "*") [EVar "x", EApp (EVar "fact") [EApp (EVar "-") [EVar "x", EInt 1]]])))
  putStrLn "SExpr:"
  print sexpr
  putStrLn "\nAST:"
  print ast

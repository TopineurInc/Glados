{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- SExprConstruct
-}

module SExprConstruct
    ( sourceFromSExprs
    ) where

import AST

sourceFromAtom :: Atom -> String
sourceFromAtom (AInteger n) = show n
sourceFromAtom (AFloat n) = show n
sourceFromAtom (ABool True) = "#t"
sourceFromAtom (ABool False) = "#f"
sourceFromAtom (AString s) = show s
sourceFromAtom (ASymbol sym) = sym

sourceFromSExpr :: SExpr -> String
sourceFromSExpr (SAtom atom _) = sourceFromAtom atom
sourceFromSExpr (SList exprs _) =
    "(" ++ unwords (map sourceFromSExpr exprs) ++ ")"

sourceFromSExprs :: [SExpr] -> String
sourceFromSExprs exprs = unlines $ map sourceFromSExpr exprs

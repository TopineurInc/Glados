{-# LANGUAGE LambdaCase #-}

module MacroExpander
  ( expandMacros
  , MacroEnv
  , defaultMacroEnv
  ) where

import AST
import qualified Data.Map as Map

type MacroEnv = Map.Map String (SExpr -> Either CompileError SExpr)

-- Default host macros
defaultMacroEnv :: MacroEnv
defaultMacroEnv = Map.fromList
  [ ("when", expandWhen)
  , ("unless", expandUnless)
  , ("cond", expandCond)
  ]

-- Expand all macros recursively in an S-expression
expandMacros :: MacroEnv -> SExpr -> Either CompileError SExpr
expandMacros env sexpr = expandOnce env sexpr >>= \expanded ->
  if expanded == sexpr
    then expandChildren env sexpr
    else expandMacros env expanded

-- Expand one level (doesn't recurse into children)
expandOnce :: MacroEnv -> SExpr -> Either CompileError SExpr
expandOnce env (SList (SAtom (ASymbol name) _ : args) loc) =
  case Map.lookup name env of
    Just expander -> expander (SList (SAtom (ASymbol name) Nothing : args) loc)
    Nothing -> Right (SList (SAtom (ASymbol name) Nothing : args) loc)
expandOnce _ sexpr = Right sexpr

-- Recursively expand children
expandChildren :: MacroEnv -> SExpr -> Either CompileError SExpr
expandChildren _ atom@(SAtom _ _) = Right atom
expandChildren env (SList exprs loc) = do
  expanded <- mapM (expandMacros env) exprs
  return $ SList expanded loc

-- Macro: (when cond expr) => (if cond expr #f)
expandWhen :: SExpr -> Either CompileError SExpr
expandWhen (SList [_, cond, body] loc) =
  Right $ SList
    [ SAtom (ASymbol "if") Nothing
    , cond
    , body
    , SAtom (ABool False) Nothing
    ] loc
expandWhen (SList _ loc) = Left $ SyntaxError "when expects 2 arguments: (when cond body)" loc
expandWhen atom = Left $ SyntaxError "when expects a list" (sexprLoc atom)

-- Macro: (unless cond expr) => (if cond #f expr)
expandUnless :: SExpr -> Either CompileError SExpr
expandUnless (SList [_, cond, body] loc) =
  Right $ SList
    [ SAtom (ASymbol "if") Nothing
    , cond
    , SAtom (ABool False) Nothing
    , body
    ] loc
expandUnless (SList _ loc) = Left $ SyntaxError "unless expects 2 arguments: (unless cond body)" loc
expandUnless atom = Left $ SyntaxError "unless expects a list" (sexprLoc atom)

-- Macro: (cond (test1 expr1) (test2 expr2) ...) => nested ifs
expandCond :: SExpr -> Either CompileError SExpr
expandCond (SList (_ : clauses) loc) = expandCondClauses clauses loc
expandCond (SList _ loc) = Left $ SyntaxError "cond expects at least one clause" loc
expandCond atom = Left $ SyntaxError "cond expects a list" (sexprLoc atom)

expandCondClauses :: [SExpr] -> Loc -> Either CompileError SExpr
expandCondClauses [] loc = Right $ SAtom (ABool False) loc
expandCondClauses [SList [SAtom (ASymbol "else") _, body] _] _ = Right body
expandCondClauses (SList [test, body] _ : rest) loc = do
  elseClause <- expandCondClauses rest loc
  Right $ SList
    [ SAtom (ASymbol "if") Nothing
    , test
    , body
    , elseClause
    ] loc
expandCondClauses (clause : _) _ = Left $ SyntaxError "cond clause must be (test body)" (sexprLoc clause)
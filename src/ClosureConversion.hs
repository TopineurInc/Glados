{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ClosureConversion
-}

module ClosureConversion
  ( closureConvert
  , ClosureInfo(..)
  ) where

import AST
import qualified Data.Set as Set

data ClosureInfo = ClosureInfo
  { ciFreeVars :: [Name]
  , ciCode :: Expr
  } deriving (Eq, Show)

type Env = Set.Set Name

closureConvert :: Expr -> Either CompileError Expr
closureConvert expr = Right $ convertExpr Set.empty expr

convertExpr :: Env -> Expr -> Expr
convertExpr _ e@(EInt _) = e
convertExpr _ e@(EFloat _) = e
convertExpr _ e@(EBool _) = e
convertExpr _ e@(EString _) = e
convertExpr _ e@(EVar _) = e
convertExpr _ e@(EQuote _) = e

convertExpr env (EList exprs) =
  EList (map (convertExpr env) exprs)

convertExpr env (ELambda params body) =
  let env' = env `Set.union` Set.fromList params
      body' = convertExpr env' body
      -- Note: free variables analysis done but not yet used in current implementation
      _free = getFreeVars (Set.fromList params) body'
  in ELambda params body'

convertExpr env (EDefine name expr) =
  let env' = Set.insert name env
  in EDefine name (convertExpr env' expr)

convertExpr env (EIf cond thenE elseE) =
  EIf (convertExpr env cond) (convertExpr env thenE) (convertExpr env elseE)

convertExpr env (EApp func args) =
  EApp (convertExpr env func) (map (convertExpr env) args)

getFreeVars :: Set.Set Name -> Expr -> Set.Set Name
getFreeVars bound (EVar name)
  | Set.member name bound = Set.empty
  | otherwise = Set.singleton name
getFreeVars _ (EInt _) = Set.empty
getFreeVars _ (EFloat _) = Set.empty
getFreeVars _ (EBool _) = Set.empty
getFreeVars _ (EString _) = Set.empty
getFreeVars _ (EQuote _) = Set.empty

getFreeVars bound (EList exprs) =
  Set.unions (map (getFreeVars bound) exprs)

getFreeVars bound (ELambda params body) =
  let bound' = bound `Set.union` Set.fromList params
  in getFreeVars bound' body

getFreeVars bound (EDefine name expr) =
  let bound' = Set.insert name bound
  in getFreeVars bound' expr

getFreeVars bound (EIf cond thenE elseE) =
  getFreeVars bound cond `Set.union`
  getFreeVars bound thenE `Set.union`
  getFreeVars bound elseE

getFreeVars bound (EApp func args) =
  getFreeVars bound func `Set.union`
  Set.unions (map (getFreeVars bound) args)

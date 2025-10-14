{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- LinearityChecker - Ownership and linearity checking for Topineur
-}

module LinearityChecker
  ( checkLinearity
  , LinearityEnv
  , LinearityError(..)
  , emptyLinearityEnv
  ) where

import qualified AST
import Control.Monad (foldM, when)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Linearity environment tracking linear variables
data LinearityEnv = LinearityEnv
  { linVars :: Set.Set AST.Name       -- Variables with linear types
  , linUsed :: Set.Set AST.Name       -- Variables that have been consumed
  } deriving (Eq, Show)

emptyLinearityEnv :: LinearityEnv
emptyLinearityEnv = LinearityEnv Set.empty Set.empty

-- Linearity errors
data LinearityError
  = UseAfterMove AST.Name
  | UnusedLinear AST.Name
  | AliasingViolation AST.Name
  | LinearEscape AST.Name
  deriving (Eq, Show)

-- Mark variable as used
markUsed :: AST.Name -> LinearityEnv -> LinearityEnv
markUsed name env = env { linUsed = Set.insert name (linUsed env) }

-- Check if variable is linear
isLinear :: AST.Name -> LinearityEnv -> Bool
isLinear name env = Set.member name (linVars env)

-- Check if variable has been consumed
isConsumed :: AST.Name -> LinearityEnv -> Bool
isConsumed name env = Set.member name (linUsed env)

-- Add linear variable to environment
addLinear :: AST.Name -> LinearityEnv -> LinearityEnv
addLinear name env = env { linVars = Set.insert name (linVars env) }

-- Check expression for linearity violations
checkExpr :: LinearityEnv -> AST.Expr -> Either LinearityError LinearityEnv
checkExpr env expr = case expr of
  -- Pure expressions (no linearity concerns)
  AST.EInt _ -> Right env
  AST.EFloat _ -> Right env
  AST.EBool _ -> Right env
  AST.EString _ -> Right env

  -- Variable usage
  AST.EVar name ->
    if isLinear name env
    then if isConsumed name env
         then Left $ UseAfterMove name
         else Right $ markUsed name env
    else Right env

  -- Lambda
  AST.ELambda params body -> do
    -- Parameters might be linear
    let env' = foldr addLinear env params
    env'' <- checkExpr env' body
    -- Check all linear params were used
    mapM_ (\p -> when (isLinear p env' && not (isConsumed p env'')) $
                  Left $ UnusedLinear p) params
    Right env

  -- Function application
  AST.EApp func args -> do
    env' <- checkExpr env func
    foldM checkExpr env' args

  -- If expression
  AST.EIf cond thenE elseE -> do
    env' <- checkExpr env cond
    -- Both branches must consume same linear variables
    envThen <- checkExpr env' thenE
    envElse <- checkExpr env' elseE
    -- Verify consistent consumption
    let thenUsed = linUsed envThen
    let elseUsed = linUsed envElse
    if thenUsed == elseUsed
      then Right envThen
      else Left $ LinearEscape "inconsistent linear usage in branches"

  -- Let binding
  AST.ELet name maybeType value body -> do
    env' <- checkExpr env value
    let isLinearVar = case maybeType of
          Just (AST.TLinear _) -> True
          _ -> False
    let env'' = if isLinearVar then addLinear name env' else env'
    env''' <- checkExpr env'' body
    -- Check linear variable was consumed
    when (isLinearVar && not (isConsumed name env''')) $
      Left $ UnusedLinear name
    Right env'''

  -- List
  AST.EList exprs -> foldM checkExpr env exprs

  -- Object literal
  AST.EObjectLit _ fields ->
    foldM checkExpr env (map snd fields)

  -- Method call
  AST.EMethodCall obj _ args -> do
    env' <- checkExpr env obj
    foldM checkExpr env' args

  -- Field access
  AST.EFieldAccess obj _ ->
    checkExpr env obj

  -- Type annotation
  AST.ETyped expr _ ->
    checkExpr env expr

  -- Linear binding (explicit linear type)
  AST.ELinearBind name value body -> do
    env' <- checkExpr env value
    let env'' = addLinear name env'
    env''' <- checkExpr env'' body
    -- Must consume linear variable
    if isConsumed name env'''
      then Right env'''
      else Left $ UnusedLinear name

  -- Match expression
  AST.EMatch scrutinee cases -> do
    env' <- checkExpr env scrutinee
    -- All cases must have consistent linear usage
    envs <- mapM (\(_, e) -> checkExpr env' e) cases
    -- Check consistency (simplified)
    case envs of
      [] -> Right env'
      (first:rest) ->
        if all (\e -> linUsed e == linUsed first) rest
        then Right first
        else Left $ LinearEscape "inconsistent linear usage in match"

  -- Block
  AST.EBlock exprs ->
    foldM checkExpr env exprs

  -- Definitions
  AST.EObjectDef _ -> Right env
  AST.ETraitDef _ -> Right env
  AST.ETraitImpl _ -> Right env
  AST.EDefine _ expr -> checkExpr env expr

  -- Quote
  AST.EQuote _ -> Right env

-- Public interface
checkLinearity :: AST.Expr -> Either LinearityError ()
checkLinearity expr = do
  finalEnv <- checkExpr emptyLinearityEnv expr
  -- Check no unused linear variables remain
  let unused = Set.difference (linVars finalEnv) (linUsed finalEnv)
  if Set.null unused
    then Right ()
    else Left $ UnusedLinear (Set.findMin unused)

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AlphaRename
  ( alphaRename
  , RenameM
  , runRename
  ) where

import AST
import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

type Env = Map.Map Name Name
type FreeVars = Set.Set Name

data RenameState = RenameState
  { rsCounter :: Int
  , rsFreeVars :: FreeVars
  } deriving (Show)

newtype RenameM a = RenameM { unRenameM :: State RenameState a }
  deriving (Functor, Applicative, Monad, MonadState RenameState)

runRename :: RenameM a -> (a, FreeVars)
runRename m =
  let (result, st) = runState (unRenameM m) (RenameState 0 Set.empty)
  in (result, rsFreeVars st)

-- Generate a unique name
gensym :: Name -> RenameM Name
gensym base = do
  st <- get
  let counter = rsCounter st
  put st { rsCounter = counter + 1 }
  return $ base ++ "#" ++ show counter

-- Add a free variable to the set
addFreeVar :: Name -> RenameM ()
addFreeVar name = modify $ \st -> st { rsFreeVars = Set.insert name (rsFreeVars st) }

-- Alpha-rename an expression
alphaRename :: Expr -> Either CompileError Expr
alphaRename expr = Right $ fst $ runRename (renameExpr Map.empty expr)

renameExpr :: Env -> Expr -> RenameM Expr
renameExpr _ (EInt n) = return $ EInt n
renameExpr _ (EBool b) = return $ EBool b
renameExpr _ (EString s) = return $ EString s

renameExpr env (EVar name) =
  case Map.lookup name env of
    Just newName -> return $ EVar newName
    Nothing -> do
      addFreeVar name
      return $ EVar name

renameExpr env (EList exprs) = do
  -- Check if this is a letrec pattern (all defines followed by expressions)
  let (defines, rest) = span isDefine exprs
  if not (null defines)
    then do
      -- This looks like a letrec: rename all defines simultaneously
      -- Generate new names for all defined functions
      let defNames = map getDefineName defines
      newNames <- mapM gensym defNames
      let env' = Map.fromList (zip defNames newNames) `Map.union` env
      -- Rename all define bodies with the extended environment
      defines' <- mapM (renameDefineWith env env') defines
      -- Rename the rest with the extended environment
      (rest', _) <- foldM renameInSequence ([], env') rest
      return $ EList (defines' ++ reverse rest')
    else do
      -- Not a letrec pattern, just thread environment normally
      (exprs', _) <- foldM renameInSequence ([], env) exprs
      return $ EList (reverse exprs')
  where
    isDefine (EDefine _ _) = True
    isDefine _ = False

    getDefineName (EDefine name _) = name
    getDefineName _ = error "Not a define"

    renameDefineWith _oldEnv newEnv (EDefine name expr) = do
      case Map.lookup name newEnv of
        Just newName -> do
          expr' <- renameExpr newEnv expr  -- Rename the value with new env (for letrec semantics)
          return $ EDefine newName expr'
        Nothing -> error "Name not in new env"
    renameDefineWith _ _ _ = error "Not a define"

    renameInSequence (acc, currentEnv) expr = do
      (expr', newEnv) <- renameExprWithEnv currentEnv expr
      return (expr' : acc, newEnv)

renameExpr env (ELambda params body) = do
  -- Generate new names for parameters
  newParams <- mapM gensym params
  let env' = Map.fromList (zip params newParams) `Map.union` env
  body' <- renameExpr env' body
  return $ ELambda newParams body'

renameExpr env (EDefine name expr) = do
  newName <- gensym name
  expr' <- renameExpr env expr  -- Don't use env' here, the binding is for subsequent exprs
  return $ EDefine newName expr'

renameExpr env (EIf cond thenE elseE) = do
  cond' <- renameExpr env cond
  thenE' <- renameExpr env thenE
  elseE' <- renameExpr env elseE
  return $ EIf cond' thenE' elseE'

renameExpr env (EApp func args) = do
  func' <- renameExpr env func
  args' <- mapM (renameExpr env) args
  return $ EApp func' args'

renameExpr _ (EQuote sexpr) = return $ EQuote sexpr

-- Helper that returns both renamed expr and updated environment (for EList threading)
renameExprWithEnv :: Env -> Expr -> RenameM (Expr, Env)
renameExprWithEnv env (EDefine name expr) = do
  newName <- gensym name
  let env' = Map.insert name newName env
  expr' <- renameExpr env expr
  return (EDefine newName expr', env')
renameExprWithEnv env expr = do
  expr' <- renameExpr env expr
  return (expr', env)

-- Note: freeVars and collectFreeVars are not used in current implementation
-- but kept for potential future use in closure conversion optimization
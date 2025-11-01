{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- AlphaRename
-}

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

gensym :: Name -> RenameM Name
gensym base = do
  st <- get
  let counter = rsCounter st
  put st { rsCounter = counter + 1 }
  return $ base ++ "#" ++ show counter

addFreeVar :: Name -> RenameM ()
addFreeVar name = modify $ \st -> st { rsFreeVars = Set.insert name (rsFreeVars st) }

alphaRename :: Expr -> Either CompileError Expr
alphaRename expr = Right $ fst $ runRename (renameExpr Map.empty expr)

renameExpr :: Env -> Expr -> RenameM Expr
renameExpr _ (EInt n) = return $ EInt n
renameExpr _ (EFloat n) = return $ EFloat n
renameExpr _ (EBool b) = return $ EBool b
renameExpr _ (EString s) = return $ EString s
renameExpr _ EUnit = return EUnit

renameExpr env (EVar name) =
  case Map.lookup name env of
    Just newName -> return $ EVar newName
    Nothing -> addFreeVar name >> return (EVar name)

renameExpr env (EList exprs)
  | null defines = do
      (exprs', _) <- foldM renameInSequence ([], env) exprs
      return $ EList (reverse exprs')
  | otherwise = do
      let defNames = map getDefineName defines
      newNames <- mapM gensym defNames
      let env' = Map.fromList (zip defNames newNames) `Map.union` env
      defines' <- mapM (renameDefineWith env env') defines
      (rest', _) <- foldM renameInSequence ([], env') rest
      return $ EList (defines' ++ reverse rest')
  where
    (defines, rest) = span isDefine exprs

    isDefine (EDefine {}) = True
    isDefine _ = False

    getDefineName (EDefine name _ _) = name
    getDefineName _ = error "Not a define"

    renameDefineWith _oldEnv newEnv (EDefine name expr anns) =
      case Map.lookup name newEnv of
        Just newName -> do
          expr' <- renameExpr newEnv expr
          return $ EDefine newName expr' anns
        Nothing -> error "Name not in new env"
    renameDefineWith _ _ _ = error "Not a define"

    renameInSequence (acc, currentEnv) expr = do
      (expr', newEnv) <- renameExprWithEnv currentEnv expr
      return (expr' : acc, newEnv)

renameExpr env (ELambda params retType body anns) = do
  let paramNames = map fst params
  let paramTypes = map snd params
  newParamNames <- mapM gensym paramNames
  let env' = Map.fromList (zip paramNames newParamNames) `Map.union` env
  body' <- renameExpr env' body
  let newParams = zip newParamNames paramTypes
  return $ ELambda newParams retType body' anns

renameExpr env (EDefine name expr anns) = do
  newName <- gensym name
  expr' <- renameExpr env expr
  return $ EDefine newName expr' anns

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

renameExpr env (EWhile cond body) = do
  cond' <- renameExpr env cond
  body' <- renameExpr env body
  return $ EWhile cond' body'

renameExpr env (EFor var start end body) = do
  newVar <- gensym var
  let env' = Map.insert var newVar env
  start' <- renameExpr env start
  end' <- renameExpr env end
  body' <- renameExpr env' body
  return $ EFor newVar start' end' body'

renameExpr env (ERange start end) = do
  start' <- renameExpr env start
  end' <- renameExpr env end
  return $ ERange start' end'

renameExpr env (EReturn expr) = do
  expr' <- renameExpr env expr
  return $ EReturn expr'

renameExpr env (EBinOp op left right) = do
  left' <- renameExpr env left
  right' <- renameExpr env right
  return $ EBinOp op left' right'

renameExpr env (EUnOp op expr) = do
  expr' <- renameExpr env expr
  return $ EUnOp op expr'

renameExpr env (ETuple exprs) = do
  exprs' <- mapM (renameExpr env) exprs
  return $ ETuple exprs'

renameExpr env (ETupleDestruct names expr body) = do
  newNames <- mapM gensym names
  let env' = Map.fromList (zip names newNames) `Map.union` env
  expr' <- renameExpr env expr
  body' <- renameExpr env' body
  return $ ETupleDestruct newNames expr' body'

renameExpr env (EListLiteral exprs ty) = do
  exprs' <- mapM (renameExpr env) exprs
  return $ EListLiteral exprs' ty

renameExpr env (EIndex container index) = do
  container' <- renameExpr env container
  index' <- renameExpr env index
  return $ EIndex container' index'

renameExpr env (EAssign name expr) = do
  expr' <- renameExpr env expr
  case Map.lookup name env of
    Just newName -> return $ EAssign newName expr'
    Nothing -> addFreeVar name >> return (EAssign name expr')

renameExpr env (EObjectDecl name fields methods) = do
  newName <- gensym name
  fields' <- mapM (renameField env) fields
  methods' <- mapM (renameMethod env) methods
  return $ EObjectDecl newName fields' methods'
  where
    renameField env' (Field fname ftype mdefault) = do
      newFname <- gensym fname
      mdefault' <- case mdefault of
        Just defExpr -> Just <$> renameExpr env' defExpr
        Nothing -> return Nothing
      return $ Field newFname ftype mdefault'
    renameMethod env' (Method mname params retType body) = do
      newMname <- gensym mname
      let paramNames = map fst params
      let paramTypes = map snd params
      newParamNames <- mapM gensym paramNames
      let env'' = Map.fromList (zip paramNames newParamNames) `Map.union` env'
      body' <- renameExpr env'' body
      let newParams = zip newParamNames paramTypes
      return $ Method newMname newParams retType body'

renameExpr env (EObjectInst name fieldInits) = do
  case Map.lookup name env of
    Just newName -> do
      fieldInits' <- mapM renameFieldInit fieldInits
      return $ EObjectInst newName fieldInits'
    Nothing -> do
      addFreeVar name
      fieldInits' <- mapM renameFieldInit fieldInits
      return $ EObjectInst name fieldInits'
  where
    renameFieldInit (fname, expr) = do
      expr' <- renameExpr env expr
      return (fname, expr')

renameExpr env (EMemberAccess obj member) = do
  obj' <- renameExpr env obj
  return $ EMemberAccess obj' member

renameExpr _ (EPackage name) = return $ EPackage name

renameExpr _ (EImport name) = return $ EImport name

renameExprWithEnv :: Env -> Expr -> RenameM (Expr, Env)
renameExprWithEnv env (EDefine name expr ann) = do
  newName <- gensym name
  let env' = Map.insert name newName env
  expr' <- renameExpr env expr
  return (EDefine newName expr' ann, env')
renameExprWithEnv env expr = do
  expr' <- renameExpr env expr
  return (expr', env)

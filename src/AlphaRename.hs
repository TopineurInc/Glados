{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- AlphaRename
-}

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

type RenameM = State RenameState

runRename :: RenameM a -> (a, FreeVars)
runRename m =
  let (result, st) = runState m (RenameState 0 Set.empty)
  in (result, rsFreeVars st)

gensym :: Name -> RenameM Name
gensym base =
  get >>= \st ->
    let counter = rsCounter st
        newName = base ++ "#" ++ show counter
    in put st { rsCounter = counter + 1 } >> pure newName

addFreeVar :: Name -> RenameM ()
addFreeVar name =
  modify $ \st -> st { rsFreeVars = Set.insert name (rsFreeVars st) }

alphaRename :: Expr -> Either CompileError Expr
alphaRename expr = Right $ fst $ runRename (renameExpr Map.empty expr)

renameExpr :: Env -> Expr -> RenameM Expr
renameExpr _ (EInt n) = pure $ EInt n
renameExpr _ (EBool b) = pure $ EBool b
renameExpr _ (EString s) = pure $ EString s
renameExpr env (EVar name) = renameVar env name
renameExpr env (EList exprs) = renameList env exprs
renameExpr env (ELambda params body) = renameLambda env params body
renameExpr env (EDefine name expr) = renameDefine env name expr
renameExpr env (EIf cond thenE elseE) = renameIf env cond thenE elseE
renameExpr env (EApp func args) = renameApp env func args
renameExpr _ (EQuote sexpr) = pure $ EQuote sexpr

renameVar :: Env -> Name -> RenameM Expr
renameVar env name =
  maybe
    (addFreeVar name >> pure (EVar name))
    (pure . EVar)
    (Map.lookup name env)

renameList :: Env -> [Expr] -> RenameM Expr
renameList env exprs =
  let (defines, rest) = span isDefine exprs
  in if null defines
      then renameWithoutDefines env exprs
      else renameWithDefines env defines rest

isDefine :: Expr -> Bool
isDefine (EDefine _ _) = True
isDefine _ = False

renameWithoutDefines :: Env -> [Expr] -> RenameM Expr
renameWithoutDefines env exprs =
  foldM renameSequence ([], env) exprs >>= \(renamed, _) ->
    pure $ EList (reverse renamed)

renameWithDefines :: Env -> [Expr] -> [Expr] -> RenameM Expr
renameWithDefines env defExprs restExprs =
  let defNames = map getDefineName defExprs
  in mapM gensym defNames >>= \newNames ->
       let env' = Map.fromList (zip defNames newNames) `Map.union` env
       in mapM (renameDefineWith env env') defExprs >>= \renamedDefines ->
            foldM renameSequence ([], env') restExprs >>= \(renamedRest, _) ->
              pure $ EList (renamedDefines ++ reverse renamedRest)

getDefineName :: Expr -> Name
getDefineName (EDefine name _) = name
getDefineName _ = error "Not a define"

renameSequence :: ([Expr], Env) -> Expr -> RenameM ([Expr], Env)
renameSequence (acc, currentEnv) expression =
  renameExprWithEnv currentEnv expression >>= \(renamed, newEnv) ->
    pure (renamed : acc, newEnv)

renameLambda :: Env -> [Name] -> Expr -> RenameM Expr
renameLambda env params body =
  mapM gensym params >>= \newParams ->
    let env' = Map.fromList (zip params newParams) `Map.union` env
    in renameExpr env' body >>= \body' -> pure $ ELambda newParams body'

renameDefine :: Env -> Name -> Expr -> RenameM Expr
renameDefine env name expr =
  gensym name >>= \newName ->
    renameExpr env expr >>= \expr' -> pure $ EDefine newName expr'

renameIf :: Env -> Expr -> Expr -> Expr -> RenameM Expr
renameIf env cond thenE elseE =
  renameExpr env cond >>= \cond' ->
    renameExpr env thenE >>= \thenE' ->
      renameExpr env elseE >>= \elseE' -> pure $ EIf cond' thenE' elseE'

renameApp :: Env -> Expr -> [Expr] -> RenameM Expr
renameApp env func args =
  renameExpr env func >>= \func' ->
    mapM (renameExpr env) args >>= \args' -> pure $ EApp func' args'

renameDefineWith :: Env -> Env -> Expr -> RenameM Expr
renameDefineWith _ newEnv (EDefine name expr) =
  maybe
    (error "Name not in new env")
    renameWithNewName
    (Map.lookup name newEnv)
  where
    renameWithNewName newName =
      renameExpr newEnv expr >>= \expr' -> pure $ EDefine newName expr'
renameDefineWith _ _ _ = error "Not a define"

renameExprWithEnv :: Env -> Expr -> RenameM (Expr, Env)
renameExprWithEnv env (EDefine name expr) =
  gensym name >>= \newName ->
    let env' = Map.insert name newName env
    in renameExpr env expr >>= \expr' -> pure (EDefine newName expr', env')
renameExprWithEnv env expr =
  renameExpr env expr >>= \expr' ->
    pure (expr', env)

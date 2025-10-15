{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TypeChecker - Type inference and checking for Topineur
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker
  ( typeCheck
  , inferType
  , TypeEnv
  , TypeError(..)
  , emptyTypeEnv
  , defaultTypeEnv
  ) where

import qualified AST
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (zipWithM, replicateM, void, forM_, when)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Type environment mapping variables to types
type TypeEnv = Map.Map AST.Name AST.Type

-- Type errors
data TypeError
  = UnboundVariable AST.Name
  | TypeMismatch AST.Type AST.Type
  | OccursCheckFailed AST.Name AST.Type
  | CannotUnify AST.Type AST.Type
  | InvalidEffectUsage AST.Effect
  | LinearityViolation AST.Name
  | TraitNotImplemented AST.Name AST.Type
  | ArityMismatch Int Int
  | CustomError String
  deriving (Eq, Show)

-- Type inference state
data TypeState = TypeState
  { tsCounter :: Int
  , tsSubstitution :: Substitution
  } deriving (Show)

type Substitution = Map.Map AST.Name AST.Type

emptyState :: TypeState
emptyState = TypeState 0 Map.empty

-- Type inference monad
newtype TypeM a = TypeM { unTypeM :: ExceptT TypeError (State TypeState) a }
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState TypeState)

runTypeM :: TypeM a -> Either TypeError a
runTypeM m = evalState (runExceptT (unTypeM m)) emptyState

-- Empty type environment
emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.empty

-- Generate fresh type variable
freshTyVar :: TypeM AST.Type
freshTyVar = do
  s <- get
  let n = tsCounter s
  put s { tsCounter = n + 1 }
  return $ AST.TVar ("t" ++ show n)

-- Apply substitution to type
applySubst :: Substitution -> AST.Type -> AST.Type
applySubst subst ty = case ty of
  AST.TVar name -> Map.findWithDefault ty name subst
  AST.TFunc params effects ret ->
    AST.TFunc (map (applySubst subst) params) effects (applySubst subst ret)
  AST.TLinear t -> AST.TLinear (applySubst subst t)
  AST.TList t -> AST.TList (applySubst subst t)
  AST.TTuple ts -> AST.TTuple (map (applySubst subst) ts)
  AST.TLazy t -> AST.TLazy (applySubst subst t)
  _ -> ty

-- Apply substitution to environment
applySubstEnv :: Substitution -> TypeEnv -> TypeEnv
applySubstEnv subst = Map.map (applySubst subst)

-- Get free type variables in a type
freeVars :: AST.Type -> Set.Set AST.Name
freeVars ty = case ty of
  AST.TVar name -> Set.singleton name
  AST.TFunc params _ ret ->
    Set.unions (freeVars ret : map freeVars params)
  AST.TLinear t -> freeVars t
  AST.TList t -> freeVars t
  AST.TTuple ts -> Set.unions (map freeVars ts)
  AST.TLazy t -> freeVars t
  _ -> Set.empty

-- Occurs check for infinite types
occursCheck :: AST.Name -> AST.Type -> Bool
occursCheck name ty = Set.member name (freeVars ty)

-- Unification algorithm (Robinson's algorithm)
unify :: AST.Type -> AST.Type -> TypeM Substitution
unify t1 t2 = do
  subst <- gets tsSubstitution
  let t1' = applySubst subst t1
  let t2' = applySubst subst t2
  case (t1', t2') of
    -- Same types
    _ | t1' == t2' -> return Map.empty

    -- Variable unification
    (AST.TVar name, t) -> bindVar name t
    (t, AST.TVar name) -> bindVar name t

    -- Function unification
    (AST.TFunc p1 e1 r1, AST.TFunc p2 e2 r2) -> do
      when (length p1 /= length p2) $
        throwError $ CannotUnify t1' t2'
      paramSubsts <- zipWithM unify p1 p2
      retSubst <- unify r1 r2
      return $ Map.unions (retSubst : paramSubsts)

    -- List unification
    (AST.TList t1, AST.TList t2) -> unify t1 t2

    -- Tuple unification
    (AST.TTuple ts1, AST.TTuple ts2) -> do
      when (length ts1 /= length ts2) $
        throwError $ CannotUnify t1' t2'
      substs <- zipWithM unify ts1 ts2
      return $ Map.unions substs

    -- Linear type unification
    (AST.TLinear t1, AST.TLinear t2) -> unify t1 t2

    -- Lazy type unification
    (AST.TLazy t1, AST.TLazy t2) -> unify t1 t2

    -- Failed unification
    _ -> throwError $ CannotUnify t1' t2'

-- Bind type variable to type
bindVar :: AST.Name -> AST.Type -> TypeM Substitution
bindVar name ty
  | occursCheck name ty = throwError $ OccursCheckFailed name ty
  | otherwise = do
      s <- get
      let newSubst = Map.insert name ty (tsSubstitution s)
      put s { tsSubstitution = newSubst }
      return $ Map.singleton name ty

-- Instantiate type scheme (for polymorphism)
instantiate :: AST.Type -> TypeM AST.Type
instantiate ty = return ty  -- Simplified for now

-- Generalize type to type scheme
generalize :: TypeEnv -> AST.Type -> AST.Type
generalize env ty = ty  -- Simplified for now

-- Main type inference function
inferType :: TypeEnv -> AST.Expr -> TypeM AST.Type
inferType env expr = case expr of
  -- Literals
  AST.EInt _ -> return AST.TInt
  AST.EFloat _ -> return AST.TFloat
  AST.EBool _ -> return AST.TBool
  AST.EString _ -> return AST.TString

  -- Variables
  AST.EVar name -> case Map.lookup name env of
    Just ty -> instantiate ty
    Nothing -> throwError $ UnboundVariable name

  -- Lambda abstraction
  AST.ELambda params body -> do
    -- Create fresh type variables for parameters
    paramTys <- replicateM (length params) freshTyVar
    let env' = foldr (uncurry Map.insert) env (zip params paramTys)
    bodyTy <- inferType env' body
    return $ AST.TFunc paramTys (AST.EffectRow []) bodyTy

  -- Function application
  AST.EApp func args -> do
    funcTy <- inferType env func
    argTys <- mapM (inferType env) args
    resultTy <- freshTyVar
    let expectedFuncTy = AST.TFunc argTys (AST.EffectRow []) resultTy
    subst <- unify funcTy expectedFuncTy
    return $ applySubst subst resultTy

  -- If expression
  AST.EIf cond thenE elseE -> do
    condTy <- inferType env cond
    _ <- unify condTy AST.TBool
    thenTy <- inferType env thenE
    elseTy <- inferType env elseE
    _ <- unify thenTy elseTy
    return thenTy

  -- Let binding
  AST.ELet name maybeType value body -> do
    valueTy <- inferType env value
    -- If type annotation provided, check it matches
    case maybeType of
      Just annotatedTy -> void $ unify valueTy annotatedTy
      Nothing -> return ()
    let env' = Map.insert name (generalize env valueTy) env
    inferType env' body

  -- List
  AST.EList [] -> do
    elemTy <- freshTyVar
    return $ AST.TList elemTy
  AST.EList (e:es) -> do
    elemTy <- inferType env e
    mapM_ (\ex -> inferType env ex >>= unify elemTy) es
    return $ AST.TList elemTy

  -- Object literal
  AST.EObjectLit name fields -> do
    -- For now, return object type
    -- TODO: Check field types match object definition
    return $ AST.TObject name

  -- Method call
  AST.EMethodCall obj methodName args -> do
    objTy <- inferType env obj
    -- TODO: Look up method in object type and check args
    -- For now, return fresh type variable
    freshTyVar

  -- Field access
  AST.EFieldAccess obj fieldName -> do
    objTy <- inferType env obj
    -- TODO: Look up field type in object
    freshTyVar

  -- Type annotation
  AST.ETyped expr ty -> do
    exprTy <- inferType env expr
    _ <- unify exprTy ty
    return ty

  -- Linear binding
  AST.ELinearBind name value body -> do
    valueTy <- inferType env value
    let linearTy = AST.TLinear valueTy
    let env' = Map.insert name linearTy env
    inferType env' body

  -- Match expression
  AST.EMatch scrutinee cases -> do
    scrutineeTy <- inferType env scrutinee
    case cases of
      [] -> freshTyVar
      ((pat, expr):rest) -> do
        (patEnv, patTy) <- inferPattern env pat
        _ <- unify scrutineeTy patTy
        firstTy <- inferType (Map.union patEnv env) expr
        -- Check all cases return same type
        forM_ rest $ \(p, e) -> do
          (pEnv, pTy) <- inferPattern env p
          _ <- unify scrutineeTy pTy
          eTy <- inferType (Map.union pEnv env) e
          _ <- unify firstTy eTy
          return ()
        return firstTy

  -- Block expression
  AST.EBlock [] -> return $ AST.TInt  -- Unit type as 0
  AST.EBlock exprs -> inferType env (last exprs)

  -- Object definition
  AST.EObjectDef objDef -> do
    -- Object definitions don't have a type themselves
    -- They add a type to the environment
    return AST.TUnit

  -- Trait definition
  AST.ETraitDef traitDef -> return AST.TUnit

  -- Trait implementation
  AST.ETraitImpl traitImpl -> return AST.TUnit

  -- Define
  AST.EDefine name expr -> do
    ty <- inferType env expr
    return ty

  -- Quote (legacy placeholder)
  AST.EQuote _ -> return AST.TUnit

-- Infer pattern type and extract bindings
inferPattern :: TypeEnv -> AST.Pattern -> TypeM (TypeEnv, AST.Type)
inferPattern env pat = case pat of
  AST.PLit constant -> do
    ty <- case constant of
      AST.CInt _ -> return AST.TInt
      AST.CFloat _ -> return AST.TFloat
      AST.CBool _ -> return AST.TBool
      AST.CString _ -> return AST.TString
      AST.CFuncRef _ -> freshTyVar
    return (Map.empty, ty)

  AST.PVar name -> do
    ty <- freshTyVar
    return (Map.singleton name ty, ty)

  AST.PConstructor name patterns -> do
    -- TODO: Look up constructor type
    ty <- freshTyVar
    return (Map.empty, ty)

  AST.PWildcard -> do
    ty <- freshTyVar
    return (Map.empty, ty)

  AST.PTuple patterns -> do
    results <- mapM (inferPattern env) patterns
    let envs = map fst results
    let tys = map snd results
    return (Map.unions envs, AST.TTuple tys)

-- Public type checking interface
typeCheck :: TypeEnv -> AST.Expr -> Either TypeError AST.Type
typeCheck env expr = runTypeM (inferType env expr)

-- Helper: default type environment used by the compiler pipeline.
defaultTypeEnv :: TypeEnv
defaultTypeEnv = Map.fromList
  [ ("+", AST.TFunc [AST.TInt, AST.TInt] (AST.EffectRow []) AST.TInt)
  , ("-", AST.TFunc [AST.TInt, AST.TInt] (AST.EffectRow []) AST.TInt)
  , ("*", AST.TFunc [AST.TInt, AST.TInt] (AST.EffectRow []) AST.TInt)
  , ("div", AST.TFunc [AST.TInt, AST.TInt] (AST.EffectRow []) AST.TInt)
  , ("mod", AST.TFunc [AST.TInt, AST.TInt] (AST.EffectRow []) AST.TInt)
  , ("<", AST.TFunc [AST.TInt, AST.TInt] (AST.EffectRow []) AST.TBool)
  , (">", AST.TFunc [AST.TInt, AST.TInt] (AST.EffectRow []) AST.TBool)
  , ("<=", AST.TFunc [AST.TInt, AST.TInt] (AST.EffectRow []) AST.TBool)
  , (">=", AST.TFunc [AST.TInt, AST.TInt] (AST.EffectRow []) AST.TBool)
  , ("eq?", AST.TFunc [AST.TInt, AST.TInt] (AST.EffectRow []) AST.TBool)
  , ("and", AST.TFunc [AST.TBool, AST.TBool] (AST.EffectRow []) AST.TBool)
  , ("or", AST.TFunc [AST.TBool, AST.TBool] (AST.EffectRow []) AST.TBool)
  , ("not", AST.TFunc [AST.TBool] (AST.EffectRow []) AST.TBool)
  , ("println", AST.TFunc [AST.TString] (AST.EffectRow [AST.EffIO]) AST.TUnit)
  , ("string-append", AST.TFunc [AST.TString, AST.TString] (AST.EffectRow []) AST.TString)
  ]

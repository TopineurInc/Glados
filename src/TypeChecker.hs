{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TypeChecker - Type inference and checking
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker
  ( typeCheck
  , inferType
  , typeCheckProgram
  , TypeEnv
  , emptyTypeEnv
  , TypeError(..)
  ) where

import AST
import Control.Monad (replicateM, zipWithM_, foldM)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Type environment mapping variable names to types
type TypeEnv = Map.Map Name Type

-- Type variable counter for generating fresh type variables
type TVarCounter = Int

-- Substitution: maps type variables to types
type Subst = Map.Map String Type

-- Type errors
data TypeError
  = UnificationError Type Type
  | OccursCheckError String Type
  | UnboundVariable Name
  | ArityMismatch Int Int
  | TypeAnnotationMismatch Type Type
  | UnsupportedOperation String
  deriving (Eq, Show)

-- Type inference state
data TIState = TIState
  { tiCounter :: TVarCounter
  , tiSubst :: Subst
  } deriving (Show)

emptyTIState :: TIState
emptyTIState = TIState 0 Map.empty

newtype TI a = TI { unTI :: ExceptT TypeError (State TIState) a }
  deriving (Functor, Applicative, Monad, MonadState TIState, MonadError TypeError)

runTI :: TI a -> (Either TypeError a, TIState)
runTI ti = runState (runExceptT (unTI ti)) emptyTIState

emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.fromList
  -- Built-in functions
  [ ("+", TFun [TInt, TInt] TInt)
  , ("-", TFun [TInt, TInt] TInt)
  , ("*", TFun [TInt, TInt] TInt)
  , ("div", TFun [TInt, TInt] TInt)
  , ("mod", TFun [TInt, TInt] TInt)
  , ("<", TFun [TInt, TInt] TBool)
  , ("<=", TFun [TInt, TInt] TBool)
  , (">", TFun [TInt, TInt] TBool)
  , (">=", TFun [TInt, TInt] TBool)
  , ("not", TFun [TBool] TBool)
  , ("and", TFun [TBool, TBool] TBool)
  , ("or", TFun [TBool, TBool] TBool)
  , ("print", TFun [TString] TUnit)
  , ("println", TFun [TString] TUnit)
  , ("display", TFun [TString] TUnit)
  , ("input", TFun [] TString)
  , ("read-line", TFun [] TString)
  , ("string->number", TFun [TString] TInt)
  , ("number->string", TFun [TInt] TString)
  , ("string-length", TFun [TString] TInt)
  , ("string-append", TFun [TString, TString] TString)
  , ("substring", TFun [TString, TInt, TInt] TString)
  ]

-- Generate a fresh type variable
freshTVar :: TI Type
freshTVar = do
  s <- get
  let n = tiCounter s
  put s { tiCounter = n + 1 }
  return $ TVar ("t" ++ show n)

-- Apply substitution to a type
applySubst :: Subst -> Type -> Type
applySubst _ TInt = TInt
applySubst _ TFloat = TFloat
applySubst _ TBool = TBool
applySubst _ TString = TString
applySubst _ TUnit = TUnit
applySubst s (TVar name) = Map.findWithDefault (TVar name) name s
applySubst s (TFun params ret) = TFun (map (applySubst s) params) (applySubst s ret)
applySubst s (TList t) = TList (applySubst s t)
applySubst s (TTuple ts) = TTuple (map (applySubst s) ts)
applySubst _ (TObject name) = TObject name

-- Get free type variables in a type
ftv :: Type -> Set.Set String
ftv TInt = Set.empty
ftv TFloat = Set.empty
ftv TBool = Set.empty
ftv TString = Set.empty
ftv TUnit = Set.empty
ftv (TVar name) = Set.singleton name
ftv (TFun params ret) = Set.unions (ftv ret : map ftv params)
ftv (TList t) = ftv t
ftv (TTuple ts) = Set.unions (map ftv ts)
ftv (TObject _) = Set.empty

-- Occurs check: does a type variable occur in a type?
occursIn :: String -> Type -> Bool
occursIn name t = Set.member name (ftv t)

-- Unification: make two types equal
unify :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- gets tiSubst
  let t1' = applySubst s t1
  let t2' = applySubst s t2
  unify' t1' t2'
  where
    unify' :: Type -> Type -> TI ()
    unify' TInt TInt = return ()
    unify' TFloat TFloat = return ()
    unify' TBool TBool = return ()
    unify' TString TString = return ()
    unify' TUnit TUnit = return ()
    unify' (TVar name) t = varBind name t
    unify' t (TVar name) = varBind name t
    unify' (TFun params1 ret1) (TFun params2 ret2)
      | length params1 == length params2 = do
          zipWithM_ unify params1 params2
          unify ret1 ret2
      | otherwise = throwError $ ArityMismatch (length params1) (length params2)
    unify' (TList ta) (TList tb) = unify ta tb
    unify' (TTuple ts1) (TTuple ts2)
      | length ts1 == length ts2 = zipWithM_ unify ts1 ts2
      | otherwise = throwError $ UnificationError (TTuple ts1) (TTuple ts2)
    unify' (TObject n1) (TObject n2)
      | n1 == n2 = return ()
      | otherwise = throwError $ UnificationError (TObject n1) (TObject n2)
    unify' ta tb = throwError $ UnificationError ta tb

    varBind :: String -> Type -> TI ()
    varBind name t
      | TVar name == t = return ()
      | occursIn name t = throwError $ OccursCheckError name t
      | otherwise = do
          s <- gets tiSubst
          modify $ \st -> st { tiSubst = Map.insert name t s }

-- Type inference for expressions
inferExpr :: TypeEnv -> Expr -> TI Type

inferExpr _ (EInt _) = return TInt

inferExpr _ (EFloat _) = return TFloat

inferExpr _ (EBool _) = return TBool

inferExpr _ (EString _) = return TString

inferExpr _ EUnit = return TUnit

inferExpr env (EVar name) =
  case Map.lookup name env of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable name

inferExpr env (EIf cond thenE elseE) = do
  condT <- inferExpr env cond
  unify condT TBool
  thenT <- inferExpr env thenE
  elseT <- inferExpr env elseE
  unify thenT elseT
  return thenT

inferExpr env (ELambda params retTypeAnn body _anns) = do
  -- Create type variables for parameters or use annotations
  paramTypes <- mapM createParamType params
  let paramNames = map fst params
  let env' = Map.union (Map.fromList (zip paramNames paramTypes)) env
  
  bodyType <- inferExpr env' body
  
  -- Check return type annotation if provided
  case retTypeAnn of
    Just annotatedRet -> unify bodyType annotatedRet
    Nothing -> return ()
  
  s <- gets tiSubst
  let finalParamTypes = map (applySubst s) paramTypes
  let finalBodyType = applySubst s bodyType
  
  return $ TFun finalParamTypes finalBodyType
  where
    createParamType (_, Just t) = return t
    createParamType (_, Nothing) = freshTVar

inferExpr env (EApp func args) = do
  -- Special handling for polymorphic built-in functions
  case (func, args) of
    (EVar "show", [arg]) -> do
      -- show: ∀a. a -> String
      -- Accept any type and return String
      _ <- inferExpr env arg
      return TString
    
    (EVar "eq?", [arg1, arg2]) -> do
      -- eq?: ∀a. a -> a -> Bool
      -- Both arguments must have the same type
      t1 <- inferExpr env arg1
      t2 <- inferExpr env arg2
      unify t1 t2
      return TBool
    
    _ -> do
      funcType <- inferExpr env func
      argTypes <- mapM (inferExpr env) args
      resultType <- freshTVar
      unify funcType (TFun argTypes resultType)
      s <- gets tiSubst
      return $ applySubst s resultType

inferExpr env (EDefine _name expr _anns) = do
  _ <- inferExpr env expr
  -- Return TUnit; EList will handle adding the name to the environment
  return TUnit

inferExpr _ (EList []) = return TUnit

inferExpr env (EList exprs) = do
  -- Thread environment through definitions in the list
  (_, lastType) <- foldM processExpr (env, TUnit) exprs
  return lastType
  where
    processExpr (currentEnv, _) (EDefine name expr _anns) = do
      exprT <- inferExpr currentEnv expr
      let newEnv = Map.insert name exprT currentEnv
      return (newEnv, TUnit)
    processExpr (currentEnv, _) expr = do
      t <- inferExpr currentEnv expr
      return (currentEnv, t)

inferExpr _ (EQuote _) = return TUnit

inferExpr env (EWhile cond body) = do
  condT <- inferExpr env cond
  unify condT TBool
  _ <- inferExpr env body
  return TUnit

inferExpr env (EFor var start end body) = do
  startT <- inferExpr env start
  endT <- inferExpr env end
  unify startT TInt
  unify endT TInt
  let env' = Map.insert var TInt env
  _ <- inferExpr env' body
  return TUnit

inferExpr env (ERange start end) = do
  startT <- inferExpr env start
  endT <- inferExpr env end
  unify startT TInt
  unify endT TInt
  return $ TList TInt

inferExpr env (EReturn expr) = inferExpr env expr

inferExpr env (EBinOp op left right) = do
  leftT <- inferExpr env left
  rightT <- inferExpr env right
  case op of
    Add -> unify leftT TInt >> unify rightT TInt >> return TInt
    Sub -> unify leftT TInt >> unify rightT TInt >> return TInt
    Mul -> unify leftT TInt >> unify rightT TInt >> return TInt
    Div -> unify leftT TInt >> unify rightT TInt >> return TInt
    Mod -> unify leftT TInt >> unify rightT TInt >> return TInt
    Lt -> unify leftT TInt >> unify rightT TInt >> return TBool
    Lte -> unify leftT TInt >> unify rightT TInt >> return TBool
    Gt -> unify leftT TInt >> unify rightT TInt >> return TBool
    Gte -> unify leftT TInt >> unify rightT TInt >> return TBool
    Eq -> unify leftT rightT >> return TBool
    Neq -> unify leftT rightT >> return TBool
    And -> unify leftT TBool >> unify rightT TBool >> return TBool
    Or -> unify leftT TBool >> unify rightT TBool >> return TBool
    Concat -> unify leftT TString >> unify rightT TString >> return TString

inferExpr env (EUnOp op expr) = do
  exprT <- inferExpr env expr
  case op of
    Not -> unify exprT TBool >> return TBool
    Neg -> unify exprT TInt >> return TInt

inferExpr env (ETuple exprs) = do
  types <- mapM (inferExpr env) exprs
  return $ TTuple types

inferExpr env (ETupleDestruct names tupleExpr body) = do
  tupleT <- inferExpr env tupleExpr
  paramTypes <- replicateM (length names) freshTVar
  unify tupleT (TTuple paramTypes)
  s <- gets tiSubst
  let finalTypes = map (applySubst s) paramTypes
  let env' = Map.union (Map.fromList (zip names finalTypes)) env
  inferExpr env' body

inferExpr env (EIndex expr idx) = do
  exprT <- inferExpr env expr
  idxT <- inferExpr env idx
  case exprT of
    TTuple _ -> do
      unify idxT TInt
      elemT <- freshTVar
      return elemT
    TList elemT -> do
      unify idxT TInt
      return elemT
    _ -> do
      elemT <- freshTVar
      unify exprT (TList elemT)
      unify idxT TInt
      return elemT

inferExpr env (EListLiteral exprs typeAnn) = do
  case exprs of
    [] -> case typeAnn of
      Just t -> return $ TList t
      Nothing -> TList <$> freshTVar
    (e:es) -> do
      elemT <- inferExpr env e
      mapM_ (\ex -> inferExpr env ex >>= unify elemT) es
      return $ TList elemT

inferExpr _ (EObjectDecl name _fields _methods) = return $ TObject name

inferExpr _ (EObjectInst typeName _fieldInits) = return $ TObject typeName

inferExpr env (EMemberAccess obj _memberName) = do
  _ <- inferExpr env obj
  -- For now, we can't infer member types without a full type system
  -- Return a fresh type variable
  freshTVar

inferExpr env (EAssign name expr) = do
  exprT <- inferExpr env expr
  case Map.lookup name env of
    Just varT -> unify varT exprT
    Nothing -> throwError $ UnboundVariable name
  return exprT

inferExpr _ (EPackage _) = return TUnit

inferExpr _ (EImport _) = return TUnit

-- Collect function definitions from a program
collectDefinitions :: Expr -> [(Name, Type)]
collectDefinitions expr = go expr
  where
    go (EList exprs) = concatMap go exprs
    go (EDefine name (ELambda params retTypeAnn _ _) _) =
      -- Only collect if all parameters and return type have annotations
      case (mapM snd params, retTypeAnn) of
        (Just paramTypes, Just retType) -> [(name, TFun paramTypes retType)]
        _ -> []  -- Skip functions without complete type annotations
    go (EDefine _ innerExpr _) = go innerExpr
    go _ = []

-- Build type environment from program
buildTypeEnv :: Expr -> TypeEnv
buildTypeEnv expr =
  let defs = collectDefinitions expr
  in Map.union (Map.fromList defs) emptyTypeEnv

-- Main type checking function
typeCheck :: TypeEnv -> Expr -> Either TypeError Type
typeCheck env expr =
  let (result, _) = runTI (inferExpr env expr)
  in result

-- Type check with automatically built environment
typeCheckProgram :: Expr -> Either TypeError Type
typeCheckProgram expr =
  let env = buildTypeEnv expr
  in typeCheck env expr

-- Infer type with default environment
inferType :: Expr -> Either TypeError Type
inferType = typeCheck emptyTypeEnv

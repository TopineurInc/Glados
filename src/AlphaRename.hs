module AlphaRename
  ( alphaRename
  , resolveNames
  , ResolveEnv
  , emptyResolveEnv
  , Binding(..)
  ) where

import AST
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- | Binding information for variables
data Binding = Binding
  { bindingName :: String     -- Original name
  , bindingId   :: String     -- Unique identifier (after gensym)
  , bindingType :: BindingType
  } deriving (Show, Eq)

-- | Type of binding (for future type checking)
data BindingType
  = LocalVar
  | FunctionParam
  | TopLevel
  | BuiltinFunction
  deriving (Show, Eq)

-- | Environment for name resolution
type ResolveEnv = Map.Map String Binding

-- | Empty resolution environment
emptyResolveEnv :: ResolveEnv
emptyResolveEnv = Map.empty

-- | Alpha-rename variables to avoid name conflicts
alphaRename :: Expr -> CompilerM Expr
alphaRename expr = do
  (renamedExpr, _) <- runStateT (alphaRenameWithEnv expr) emptyResolveEnv
  return renamedExpr

-- | Resolve names in expression with environment
resolveNames :: ResolveEnv -> Expr -> CompilerM (Expr, ResolveEnv)
resolveNames env expr = runStateT (alphaRenameWithEnv expr) env

-- | Alpha-rename with environment in StateT
alphaRenameWithEnv :: Expr -> StateT ResolveEnv (ReaderT CompilerConfig (StateT CompilerState (Writer [String]))) Expr
alphaRenameWithEnv expr = case expr of
  EInt i loc -> return $ EInt i loc
  EBool b loc -> return $ EBool b loc
  EString s loc -> return $ EString s loc

  -- Variable reference - look up in environment
  EVar name loc -> do
    env <- get
    case Map.lookup name env of
      Just binding -> return $ EVar (bindingId binding) loc
      Nothing -> do
        -- Variable not bound - could be builtin or error
        lift $ tell ["Warning: unbound variable " ++ name]
        return $ EVar name loc

  -- List literal
  EList exprs loc -> do
    renamedExprs <- mapM alphaRenameWithEnv exprs
    return $ EList renamedExprs loc

  -- Lambda - introduce new scope
  ELambda params body loc -> do
    -- Save current environment
    oldEnv <- get

    -- Generate unique names for parameters and bind them
    newBindings <- lift $ mapM (\param -> do
      uniqueName <- gensym param
      return (param, Binding param uniqueName FunctionParam)) params

    -- Update environment with parameter bindings
    let newEnv = oldEnv `Map.union` Map.fromList newBindings
    put newEnv

    -- Rename body in new environment
    renamedBody <- alphaRenameWithEnv body

    -- Restore old environment
    put oldEnv

    let renamedParams = map (bindingId . snd) newBindings
    return $ ELambda renamedParams renamedBody loc

  -- Define - bind name in current environment
  EDefine name expr loc -> do
    -- First rename the expression
    renamedExpr <- alphaRenameWithEnv expr

    -- Generate unique name for definition
    uniqueName <- lift $ gensym name
    let binding = Binding name uniqueName TopLevel

    -- Add binding to environment
    modify (Map.insert name binding)

    return $ EDefine uniqueName renamedExpr loc

  -- If expression
  EIf cond thenExpr elseExpr loc -> do
    renamedCond <- alphaRenameWithEnv cond
    renamedThen <- alphaRenameWithEnv thenExpr
    renamedElse <- alphaRenameWithEnv elseExpr
    return $ EIf renamedCond renamedThen renamedElse loc

  -- Function application
  EApp func args loc -> do
    renamedFunc <- alphaRenameWithEnv func
    renamedArgs <- mapM alphaRenameWithEnv args
    return $ EApp renamedFunc renamedArgs loc

  -- Quote - no renaming needed
  EQuote sexpr loc -> return $ EQuote sexpr loc

-- | Create initial environment with built-in functions
builtinEnv :: ResolveEnv
builtinEnv = Map.fromList
  [ ("+", Binding "+" "+" BuiltinFunction)
  , ("-", Binding "-" "-" BuiltinFunction)
  , ("*", Binding "*" "*" BuiltinFunction)
  , ("div", Binding "div" "div" BuiltinFunction)
  , ("mod", Binding "mod" "mod" BuiltinFunction)
  , ("eq?", Binding "eq?" "eq?" BuiltinFunction)
  , ("<", Binding "<" "<" BuiltinFunction)
  ]

-- | Resolve names in a program (list of expressions)
resolveProgram :: [Expr] -> CompilerM ([Expr], ResolveEnv)
resolveProgram exprs = do
  (renamedExprs, finalEnv) <- runStateT (mapM alphaRenameWithEnv exprs) builtinEnv
  return (renamedExprs, finalEnv)

-- | Example of free variable analysis (used later for closure conversion)
freeVars :: Expr -> [String]
freeVars expr = freeVarsWithBound expr []
  where
    freeVarsWithBound :: Expr -> [String] -> [String]
    freeVarsWithBound e bound = case e of
      EVar name _
        | name `elem` bound -> []
        | otherwise -> [name]

      ELambda params body _ ->
        freeVarsWithBound body (params ++ bound)

      EDefine name expr _ ->
        freeVarsWithBound expr (name : bound)

      EIf cond thenE elseE _ ->
        concatMap (`freeVarsWithBound` bound) [cond, thenE, elseE]

      EApp func args _ ->
        concatMap (`freeVarsWithBound` bound) (func : args)

      EList exprs _ ->
        concatMap (`freeVarsWithBound` bound) exprs

      _ -> []

-- | Test function to demonstrate alpha renaming
testAlphaRename :: Expr -> IO ()
testAlphaRename expr = do
  putStrLn $ "Original: " ++ show expr
  putStrLn $ "Free vars: " ++ show (freeVars expr)
  case runCompilerM (alphaRename expr) of
    Right renamed -> do
      putStrLn $ "Renamed: " ++ show renamed
      putStrLn $ "Free vars after: " ++ show (freeVars renamed)
    Left err -> putStrLn $ "Error: " ++ err
  putStrLn ""

-- | Simple test runner
runCompilerM :: CompilerM a -> Either String a
runCompilerM computation =
  case runWriter (runStateT (runReaderT computation defaultConfig) initialState) of
    ((result, _), _) -> Right result

-- Example test cases:
-- Lambda with parameter shadowing:
-- testAlphaRename (ELambda ["x"] (ELambda ["x"] (EVar "x" Nothing) Nothing) Nothing)
-- Expected: ELambda ["x#1"] (ELambda ["x#2"] (EVar "x#2" Nothing) Nothing) Nothing

-- Function definition:
-- testAlphaRename (EDefine "f" (ELambda ["x"] (EVar "x" Nothing) Nothing) Nothing)
-- Expected: EDefine "f#1" (ELambda ["x#1"] (EVar "x#1" Nothing) Nothing) Nothing
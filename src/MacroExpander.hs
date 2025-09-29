module MacroExpander
  ( expandMacros
  , MacroEnv
  , defaultMacroEnv
  , addHostMacro
  ) where

import AST
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- | Macro environment containing host-defined macros
type MacroEnv = Map.Map String (SExpr -> CompilerM SExpr)

-- | Default macro environment with built-in macros
defaultMacroEnv :: MacroEnv
defaultMacroEnv = Map.fromList
  [ ("when", whenMacro)
  , ("unless", unlessMacro)
  , ("cond", condMacro)
  ]

-- | Add a host macro to the environment
addHostMacro :: String -> (SExpr -> CompilerM SExpr) -> MacroEnv -> MacroEnv
addHostMacro name macro env = Map.insert name macro env

-- | Expand macros in an S-expression
expandMacros :: MacroEnv -> SExpr -> CompilerM SExpr
expandMacros env sexpr = case sexpr of
  SAtom atom loc -> return $ SAtom atom loc
  SList [] loc -> return $ SList [] loc
  SList (SAtom (ASymbol name) _ : args) loc
    | Just macro <- Map.lookup name env -> do
        expanded <- macro (SList args loc)
        expandMacros env expanded  -- Recursively expand result
  SList exprs loc -> do
    expandedExprs <- mapM (expandMacros env) exprs
    return $ SList expandedExprs loc

-- =============================================================================
-- Built-in Host Macros
-- =============================================================================

-- | when macro: (when condition expr) -> (if condition expr #f)
whenMacro :: SExpr -> CompilerM SExpr
whenMacro (SList [cond, expr] loc) = do
  return $ SList
    [ SAtom (ASymbol "if") Nothing
    , cond
    , expr
    , SAtom (ABool False) Nothing
    ] loc
whenMacro (SList args _) = do
  let msg = "when macro expects exactly 2 arguments, got " ++ show (length args)
  throwCompileError $ MacroExpansionError msg Nothing
whenMacro _ = throwCompileError $ MacroExpansionError "when macro expects a list" Nothing

-- | unless macro: (unless condition expr) -> (if condition #f expr)
unlessMacro :: SExpr -> CompilerM SExpr
unlessMacro (SList [cond, expr] loc) = do
  return $ SList
    [ SAtom (ASymbol "if") Nothing
    , cond
    , SAtom (ABool False) Nothing
    , expr
    ] loc
unlessMacro (SList args _) = do
  let msg = "unless macro expects exactly 2 arguments, got " ++ show (length args)
  throwCompileError $ MacroExpansionError msg Nothing
unlessMacro _ = throwCompileError $ MacroExpansionError "unless macro expects a list" Nothing

-- | cond macro: (cond (test1 expr1) (test2 expr2) ... (else exprN))
--   -> nested if expressions
condMacro :: SExpr -> CompilerM SExpr
condMacro (SList clauses loc) = expandCond clauses loc
  where
    expandCond [] _ = return $ SAtom (ABool False) Nothing
    expandCond [SList [SAtom (ASymbol "else") _, expr] _] _ = return expr
    expandCond (SList [test, expr] _ : rest) originalLoc = do
      restExpanded <- expandCond rest originalLoc
      return $ SList
        [ SAtom (ASymbol "if") Nothing
        , test
        , expr
        , restExpanded
        ] originalLoc
    expandCond (clause : _) _ =
      throwCompileError $ MacroExpansionError ("Invalid cond clause: " ++ show clause) Nothing
condMacro _ = throwCompileError $ MacroExpansionError "cond macro expects a list of clauses" Nothing

-- | Helper function to throw compilation errors in CompilerM
throwCompileError :: CompileError -> CompilerM a
throwCompileError err = do
  tell [show err]
  error $ show err  -- For now, we'll use error. In a real implementation, we'd use ExceptT

-- | Example macro for testing: (square x) -> (* x x)
squareMacro :: SExpr -> CompilerM SExpr
squareMacro (SList [x] loc) = do
  return $ SList
    [ SAtom (ASymbol "*") Nothing
    , x
    , x
    ] loc
squareMacro (SList args _) = do
  let msg = "square macro expects exactly 1 argument, got " ++ show (length args)
  throwCompileError $ MacroExpansionError msg Nothing
squareMacro _ = throwCompileError $ MacroExpansionError "square macro expects a list" Nothing

-- | Test macro environment with additional examples
testMacroEnv :: MacroEnv
testMacroEnv = Map.insert "square" squareMacro defaultMacroEnv

-- | Example usage and test functions
testMacroExpansion :: String -> SExpr -> IO ()
testMacroExpansion name input = do
  putStrLn $ "Testing macro: " ++ name
  putStrLn $ "Input: " ++ show input
  -- Note: In real usage, this would run in CompilerM monad
  -- For now, we'll show the expected transformation
  case input of
    SList (SAtom (ASymbol "when") _ : args) loc ->
      case runCompilerM (whenMacro (SList args loc)) of
        Right result -> putStrLn $ "Output: " ++ show result
        Left err -> putStrLn $ "Error: " ++ show err
    _ -> putStrLn "Not a when macro call"
  putStrLn ""

-- | Helper to run CompilerM for testing (simplified)
runCompilerM :: CompilerM a -> Either String a
runCompilerM computation =
  case runWriter (runStateT (runReaderT computation defaultConfig) initialState) of
    ((result, _), logs) -> Right result  -- Simplified - CompilerM doesn't fail in this implementation

-- Example test cases:
-- testMacroExpansion "when" (SList [SAtom (ASymbol "when") Nothing, SAtom (ASymbol "x") Nothing, SAtom (AInteger 42) Nothing] Nothing)
-- Expected: (if x 42 #f)
module Desugar
  ( desugarSExpr
  , desugarToExpr
  , sexprToExpr
  ) where

import AST
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- | Convert S-expression to surface AST, applying desugaring transformations
desugarSExpr :: SExpr -> CompilerM Expr
desugarSExpr sexpr = sexprToExpr =<< desugarToExpr sexpr

-- | Apply desugaring transformations to S-expressions
desugarToExpr :: SExpr -> CompilerM SExpr
desugarToExpr sexpr = case sexpr of
  SAtom atom loc -> return $ SAtom atom loc
  SList [] loc -> return $ SList [] loc

  -- Desugar: (define (f args...) body) -> (define f (lambda (args...) body))
  SList [SAtom (ASymbol "define") _, SList (SAtom (ASymbol fname) _ : args) _, body] loc -> do
    let argNames = map extractSymbol args
    if all isSymbol args
      then do
        desugarBody <- desugarToExpr body
        return $ SList
          [ SAtom (ASymbol "define") Nothing
          , SAtom (ASymbol fname) Nothing
          , SList
              [ SAtom (ASymbol "lambda") Nothing
              , SList (map (\name -> SAtom (ASymbol name) Nothing) argNames) Nothing
              , desugarBody
              ] Nothing
          ] loc
      else error "Function definition arguments must be symbols"

  -- Regular define: (define name expr) - no transformation needed
  SList [SAtom (ASymbol "define") _, SAtom (ASymbol name) _, expr] loc -> do
    desugarExpr <- desugarToExpr expr
    return $ SList
      [ SAtom (ASymbol "define") Nothing
      , SAtom (ASymbol name) Nothing
      , desugarExpr
      ] loc

  -- Desugar let: (let ((var val) ...) body) -> ((lambda (vars...) body) vals...)
  SList [SAtom (ASymbol "let") _, SList bindings _, body] loc -> do
    let (vars, vals) = unzip $ map extractBinding bindings
    desugarBody <- desugarToExpr body
    desugarVals <- mapM desugarToExpr vals
    return $ SList
      ( SList
          [ SAtom (ASymbol "lambda") Nothing
          , SList (map (\v -> SAtom (ASymbol v) Nothing) vars) Nothing
          , desugarBody
          ] Nothing
      : desugarVals
      ) loc

  -- Regular lists - recursively desugar elements
  SList exprs loc -> do
    desugarExprs <- mapM desugarToExpr exprs
    return $ SList desugarExprs loc

-- | Convert S-expression to surface AST expression
sexprToExpr :: SExpr -> CompilerM Expr
sexprToExpr sexpr = case sexpr of
  SAtom (AInteger i) loc -> return $ EInt i loc
  SAtom (ABool b) loc -> return $ EBool b loc
  SAtom (AString s) loc -> return $ EString s loc
  SAtom (ASymbol name) loc -> return $ EVar name loc

  SList [] loc -> return $ EList [] loc

  -- lambda: (lambda (args...) body)
  SList [SAtom (ASymbol "lambda") _, SList args _, body] loc -> do
    let argNames = map extractSymbol args
    if all isSymbol args
      then do
        bodyExpr <- sexprToExpr body
        return $ ELambda argNames bodyExpr loc
      else error "Lambda arguments must be symbols"

  -- define: (define name expr)
  SList [SAtom (ASymbol "define") _, SAtom (ASymbol name) _, expr] loc -> do
    exprAST <- sexprToExpr expr
    return $ EDefine name exprAST loc

  -- if: (if condition then else)
  SList [SAtom (ASymbol "if") _, cond, thenExpr, elseExpr] loc -> do
    condAST <- sexprToExpr cond
    thenAST <- sexprToExpr thenExpr
    elseAST <- sexprToExpr elseExpr
    return $ EIf condAST thenAST elseAST loc

  -- quote: (quote expr)
  SList [SAtom (ASymbol "quote") _, expr] loc ->
    return $ EQuote expr loc

  -- List literal: '(expr...)
  SList (SAtom (ASymbol "quote") _ : [SList exprs _]) loc -> do
    exprList <- mapM sexprToExpr exprs
    return $ EList exprList loc

  -- Function application: (f args...)
  SList (func : args) loc -> do
    funcExpr <- sexprToExpr func
    argExprs <- mapM sexprToExpr args
    return $ EApp funcExpr argExprs loc

  -- Error cases
  SList [SAtom (ASymbol "lambda") _, _, _] _ ->
    error "Invalid lambda form"
  SList [SAtom (ASymbol "define") _, _] _ ->
    error "Invalid define form - missing expression"
  SList [SAtom (ASymbol "if") _, _, _] _ ->
    error "Invalid if form - missing else clause"

-- =============================================================================
-- Helper functions
-- =============================================================================

-- | Check if S-expression is a symbol
isSymbol :: SExpr -> Bool
isSymbol (SAtom (ASymbol _) _) = True
isSymbol _ = False

-- | Extract symbol name from S-expression
extractSymbol :: SExpr -> String
extractSymbol (SAtom (ASymbol name) _) = name
extractSymbol _ = error "Expected symbol"

-- | Extract binding from let expression: (var val) -> (var, val)
extractBinding :: SExpr -> (String, SExpr)
extractBinding (SList [SAtom (ASymbol var) _, val] _) = (var, val)
extractBinding _ = error "Invalid let binding - expected (var val)"

-- | Examples of desugaring transformations:
--
-- Input:  (define (square x) (* x x))
-- Output: (define square (lambda (x) (* x x)))
--
-- Input:  (let ((x 1) (y 2)) (+ x y))
-- Output: ((lambda (x y) (+ x y)) 1 2)
--
-- Input:  (define (factorial n)
--           (if (eq? n 1)
--               1
--               (* n (factorial (- n 1)))))
-- Output: (define factorial
--           (lambda (n)
--             (if (eq? n 1)
--                 1
--                 (* n (factorial (- n 1))))))

-- | Test helper function
testDesugar :: SExpr -> IO ()
testDesugar input = do
  putStrLn $ "Input: " ++ show input
  case runCompilerM (desugarSExpr input) of
    Right result -> putStrLn $ "Output: " ++ show result
    Left err -> putStrLn $ "Error: " ++ err
  putStrLn ""

-- | Simple runner for testing
runCompilerM :: CompilerM a -> Either String a
runCompilerM computation =
  case runWriter (runStateT (runReaderT computation defaultConfig) initialState) of
    ((result, _), _) -> Right result

-- Example test cases:x
-- testDesugar (SList [SAtom (ASymbol "define") Nothing,
--                     SList [SAtom (ASymbol "square") Nothing, SAtom (ASymbol "x") Nothing] Nothing,
--                     SList [SAtom (ASymbol "*") Nothing, SAtom (ASymbol "x") Nothing, SAtom (ASymbol "x") Nothing] Nothing] Nothing)
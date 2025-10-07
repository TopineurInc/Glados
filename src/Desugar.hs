{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Desugar
-}

{-# LANGUAGE LambdaCase #-}

module Desugar
  ( desugar
  , sexprToExpr
  ) where

import AST

-- Built-in function names that are always available
builtinNames :: [Name]
builtinNames = ["+", "-", "*", "div", "mod", "eq?", "<", ">", "print", "display", "input", "read-line", 
                "string->number", "number->string", "string-length", "string-append", "substring", 
                "not", "and", "or", "if", "define", "lambda", "let", "letrec", "begin", "quote"]

-- Convert S-expression to AST surface language (Expr)
sexprToExpr :: SExpr -> Either CompileError Expr
sexprToExpr = sexprToExprWithContext []

-- Convert S-expression to AST with variable context
sexprToExprWithContext :: [Name] -> SExpr -> Either CompileError Expr
sexprToExprWithContext boundVars = \case
  SAtom (AInteger n) _ -> Right $ EInt n
  SAtom (ABool b) _ -> Right $ EBool b
  SAtom (AString s) _ -> Right $ EString s
  SAtom (ASymbol s) loc -> 
    if s `elem` builtinNames || s `elem` boundVars
      then Right $ EVar s
      else Left $ SyntaxError ("*** ERROR : variable " ++ s ++ " is not bound. ") loc

  SList [SAtom (ASymbol "quote") _, arg] _ -> Right $ EQuote arg

  SList [SAtom (ASymbol "if") _, cond, thenE, elseE] _ -> do
    c <- sexprToExprWithContext boundVars cond
    t <- sexprToExprWithContext boundVars thenE
    e <- sexprToExprWithContext boundVars elseE
    Right $ EIf c t e
  SList [SAtom (ASymbol "if") _, cond, thenE] loc -> 
    Left $ SyntaxError "if requires both then and else branches" loc

  -- Define with lambda: (define name expr)
  SList [SAtom (ASymbol "define") _, SAtom (ASymbol name) _, expr] _ -> do
    e <- sexprToExprWithContext (name : boundVars) expr
    Right $ EDefine name e

  -- Define with sugared syntax: (define (name args...) body)
  SList [SAtom (ASymbol "define") _, SList (SAtom (ASymbol name) _ : args) _, body] _loc -> do
    params <- mapM extractParam args
    b <- sexprToExprWithContext (name : params ++ boundVars) body
    Right $ EDefine name (ELambda params b)

  -- Lambda: (lambda (args...) body)
  SList [SAtom (ASymbol "lambda") _, SList args _, body] _loc -> do
    params <- mapM extractParam args
    b <- sexprToExprWithContext (params ++ boundVars) body
    Right $ ELambda params b

  -- Let: (let ((x val) ...) body) => ((lambda (x ...) body) val ...)
  SList [SAtom (ASymbol "let") _, SList bindings _, body] _loc -> do
    (names, vals) <- desugarBindings bindings
    b <- sexprToExprWithContext (names ++ boundVars) body
    vals' <- mapM (sexprToExprWithContext boundVars) vals
    Right $ EApp (ELambda names b) vals'

  -- Letrec: (letrec ((f lambda) ...) body)
  -- Desugar to nested defines in a begin block
  SList [SAtom (ASymbol "letrec") _, SList bindings _, body] _loc -> do
    (names, vals) <- desugarBindings bindings
    let newBoundVars = names ++ boundVars
    vals' <- mapM (sexprToExprWithContext newBoundVars) vals
    b <- sexprToExprWithContext newBoundVars body
    -- Create a list of define expressions followed by the body
    let defines = zipWith (\name val -> EDefine name val) names vals'
    Right $ EList (defines ++ [b])

  -- Begin: (begin expr1 expr2 ...) => evaluate all, return last
  SList (SAtom (ASymbol "begin") _ : exprs) _ -> do
    exprs' <- mapM (sexprToExprWithContext boundVars) exprs
    Right $ EList exprs'

  -- Application: (f args...)
  SList (f : args) _ -> do
    func <- sexprToExprWithContext boundVars f
    args' <- mapM (sexprToExprWithContext boundVars) args
    Right $ EApp func args'

  SList [] loc -> Left $ SyntaxError "Empty list not allowed" loc

-- Desugar an Expr (additional pass if needed)
desugar :: Expr -> Either CompileError Expr
desugar expr = Right expr  -- Most desugaring happens in sexprToExpr

extractParam :: SExpr -> Either CompileError Name
extractParam (SAtom (ASymbol name) _) = Right name
extractParam other = Left $ SyntaxError "Parameter must be a symbol" (sexprLoc other)

desugarBindings :: [SExpr] -> Either CompileError ([Name], [SExpr])
desugarBindings bindings = do
  pairs <- mapM extractBinding bindings
  let (names, vals) = unzip pairs
  Right (names, vals)

extractBinding :: SExpr -> Either CompileError (Name, SExpr)
extractBinding (SList [SAtom (ASymbol name) _, val] _) = Right (name, val)
extractBinding other = Left $ SyntaxError "Binding must be (name value)" (sexprLoc other)
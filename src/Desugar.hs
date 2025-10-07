{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Desugar
-}

module Desugar
  ( desugar
  , sexprToExpr
  ) where

import AST

sexprToExpr :: SExpr -> Either CompileError Expr
sexprToExpr sexpr =
  case sexpr of
    SAtom atom _ -> atomToExpr atom
    SList [] loc -> Left $ SyntaxError "Empty list not allowed" loc
    SList (first : rest) loc ->
      case first of
        SAtom (ASymbol keyword) _ ->
          interpretKeyword keyword rest loc
        _ ->
          buildApplication first rest

atomToExpr :: Atom -> Either CompileError Expr
atomToExpr atom =
  case atom of
    AInteger n -> Right $ EInt n
    ABool b -> Right $ EBool b
    AString s -> Right $ EString s
    ASymbol s -> Right $ EVar s

interpretKeyword :: String -> [SExpr] -> Loc -> Either CompileError Expr
interpretKeyword keyword rest loc =
  case keyword of
    "quote" -> quoteForm rest loc
    "if" -> ifForm rest loc
    "define" -> defineForm rest loc
    "lambda" -> lambdaForm rest loc
    "let" -> letForm rest loc
    "letrec" -> letRecForm rest loc
    "begin" -> beginForm rest
    _ -> buildSymbolApplication keyword rest loc

quoteForm :: [SExpr] -> Loc -> Either CompileError Expr
quoteForm [arg] _ = Right $ EQuote arg
quoteForm _ loc = Left $ SyntaxError "quote expects a single argument" loc

ifForm :: [SExpr] -> Loc -> Either CompileError Expr
ifForm [cond, thenE, elseE] _ =
  do
    cond' <- sexprToExpr cond
    then' <- sexprToExpr thenE
    else' <- sexprToExpr elseE
    Right $ EIf cond' then' else'
ifForm _ loc = Left $ SyntaxError "if expects three arguments" loc

defineForm :: [SExpr] -> Loc -> Either CompileError Expr
defineForm parts loc =
  case parts of
    [SAtom (ASymbol name) _, expr] ->
      sexprToExpr expr >>= \expr' -> Right $ EDefine name expr'
    SList (SAtom (ASymbol name) _ : args) _ : body : _ ->
      do
        params <- mapM extractParam args
        body' <- sexprToExpr body
        Right $ EDefine name (ELambda params body')
    _ -> Left $ SyntaxError "Invalid define form" loc

lambdaForm :: [SExpr] -> Loc -> Either CompileError Expr
lambdaForm parts loc =
  case parts of
    [SList args _, body] ->
      do
        params <- mapM extractParam args
        body' <- sexprToExpr body
        Right $ ELambda params body'
    _ -> Left $ SyntaxError "Invalid lambda form" loc

letForm :: [SExpr] -> Loc -> Either CompileError Expr
letForm parts loc =
  case parts of
    [SList bindings _, body] ->
      do
        (names, vals) <- desugarBindings bindings
        body' <- sexprToExpr body
        vals' <- mapM sexprToExpr vals
        Right $ EApp (ELambda names body') vals'
    _ -> Left $ SyntaxError "Invalid let form" loc

letRecForm :: [SExpr] -> Loc -> Either CompileError Expr
letRecForm parts loc =
  case parts of
    [SList bindings _, body] ->
      do
        (names, vals) <- desugarBindings bindings
        vals' <- mapM sexprToExpr vals
        body' <- sexprToExpr body
        let defines = zipWith EDefine names vals'
        Right $ EList (defines ++ [body'])
    _ -> Left $ SyntaxError "Invalid letrec form" loc

beginForm :: [SExpr] -> Either CompileError Expr
beginForm exprs =
  mapM sexprToExpr exprs >>= \exprs' ->
    Right $ EList exprs'

buildSymbolApplication :: String -> [SExpr] -> Loc -> Either CompileError Expr
buildSymbolApplication keyword rest loc =
  buildApplication (SAtom (ASymbol keyword) loc) rest

buildApplication :: SExpr -> [SExpr] -> Either CompileError Expr
buildApplication func args =
  do
    func' <- sexprToExpr func
    args' <- mapM sexprToExpr args
    Right $ EApp func' args'

desugar :: Expr -> Either CompileError Expr
desugar expr = Right expr

extractParam :: SExpr -> Either CompileError Name
extractParam (SAtom (ASymbol name) _) = Right name
extractParam other =
  Left $
    SyntaxError "Parameter must be a symbol" (sexprLoc other)

desugarBindings :: [SExpr] -> Either CompileError ([Name], [SExpr])
desugarBindings bindings =
  do
    pairs <- mapM extractBinding bindings
    let (names, vals) = unzip pairs
    Right (names, vals)

extractBinding :: SExpr -> Either CompileError (Name, SExpr)
extractBinding (SList [SAtom (ASymbol name) _, val] _) = Right (name, val)
extractBinding other =
  Left $
    SyntaxError "Binding must be (name value)" (sexprLoc other)

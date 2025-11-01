{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/topineur/parser/TopineurParserStatements.hs
-}

module TopineurParserStatements
  ( stmtML
  , stmt
  ) where

import Control.Monad (void)
import Text.Parsec
    ( (<|>)
      , choice
      , optionMaybe
      , string
      , try
      )

import AST (SourcePos(..), Loc)
import TopineurParserTypes
import TopineurParserUtils
import TopineurParserSymbols
import TopineurParserExpressions
import TopineurParserPattern

ifStmt :: Parser Stmt
ifStmt = do
  l <- keyword "if"
  c <- expr
  _ <- keyword "then"
  sThen <-
    try inlineSimple <|> do
      ss <- withIndentedBlock simpleStmt
      case ss of
        [x] -> return x
        _ -> let msg = "then-branch must contain exactly one simple statement"
                 in case l of
                      Just p -> fail (msg ++ " at " ++ show (spLine p) ++ ":" ++ show (spCol p))
                      Nothing -> fail msg
  mElse <-
    optionMaybe $ do
      _ <- try (void newline1) <|> sc
      _ <- keyword "else"
      try inlineSimple <|> do
        ss <- withIndentedBlock simpleStmt
        case ss of
          [x] -> return x
          _ -> let msg = "else-branch must contain exactly one simple statement"
                   in case l of
                        Just p -> fail (msg ++ " at " ++ show (spLine p) ++ ":" ++ show (spCol p))
                        Nothing -> fail msg
  return (SIf l c sThen mElse)
  where
    inlineSimple = do
      sc
      simpleStmt

whileStmt :: Parser Stmt
whileStmt = do
  l <- keyword "while"
  c <- expr
  _ <- keyword "do"
  body <- blockIndentedStmts
  _ <- keyword "end"
  return (SWhile l c body)

forStmt :: Parser Stmt
forStmt = do
  l <- keyword "for"
  (_, n) <- ident
  _ <- keyword "in"
  rangeExpr <- expr
  case rangeExpr of
    EBinOp _ ".." a b -> do
      _ <- keyword "do"
      body <- blockIndentedStmts
      _ <- keyword "end"
      return (SFor l n (Range a b) body)
    _ -> fail "Expected range expression with '..'"

blockIndentedStmts :: Parser [Stmt]
blockIndentedStmts = withIndentedBlockML stmtML

lvalue :: Parser (LValue, Loc)
lvalue =
  try
    ( do
        base <- term
        _ <- symbol "."
        (l, n) <- ident
        return (LMember l base n, l)
    )
    <|> do
      (l, n) <- ident
      return (LVar l n, l)


stmt :: Parser Stmt
stmt =
  choice
    [ try $ do
        l <- keyword "top"
        STop l <$> expr
    , try $ do
        l <- keyword "let"
        pat <- patternP
        _ <- symbol "="
        SLet l pat <$> expr
    , try ifStmt
    , try whileStmt
    , try forStmt
    , try $ do
        (lv, l0) <- lvalue
        _ <- symbol "="
        SAssign l0 lv <$> expr
    , try $ do
        e <- expr
        return (SExpression (locOfE e) e)
    ]

stmtML :: Parser Stmt
stmtML =
  choice
    [ try $ do
        l <- keywordML "top"
        STop l <$> exprML
    , try $ do
        l <- keywordML "let"
        pat <- patternP
        _ <- symbolML "="
        SLet l pat <$> exprML
    , try ifStmtML
    , try whileStmtML
    , try forStmtML
    , try $ do
        (lv, l0) <- lvalueML
        _ <- symbolML "="
        SAssign l0 lv <$> exprML
    , do
        e <- exprML
        return (SExpression (locOfE e) e)
    ]

lvalueML :: Parser (LValue, Loc)
lvalueML =
  try
    ( do
        base <- termML
        _ <- symbolML "."
        (l, n) <- identML
        return (LMember l base n, l)
    )
    <|> do
      (l, n) <- identML
      return (LVar l n, l)

ifStmtML :: Parser Stmt
ifStmtML = do
  l <- keywordML "if"
  c <- exprML
  _ <- keywordML "then"
  sThen <-
    try inlineSimpleML <|> do
      ss <- withIndentedBlock simpleStmtML
      case ss of
        [x] -> return x
        _ -> let msg = "then-branch must contain exactly one simple statement"
                 in case l of
                      Just p -> fail (msg ++ " at " ++ show (spLine p) ++ ":" ++ show (spCol p))
                      Nothing -> fail msg
  mElse <-
    optionMaybe $ do
      _ <- try (void newline1) <|> sc
      _ <- keywordML "else"
      try inlineSimpleML <|> do
        ss <- withIndentedBlock simpleStmtML
        case ss of
          [x] -> return x
          _ -> let msg = "else-branch must contain exactly one simple statement"
                   in case l of
                        Just p -> fail (msg ++ " at " ++ show (spLine p) ++ ":" ++ show (spCol p))
                        Nothing -> fail msg
  return (SIf l c sThen mElse)
  where
    inlineSimpleML = do
      sc
      simpleStmtML

whileStmtML :: Parser Stmt
whileStmtML = do
  l <- keywordML "while"
  c <- exprML
  _ <- keywordML "do"
  body <- blockIndentedStmts
  newline1
  _ <- string "end"
  return (SWhile l c body)

forStmtML :: Parser Stmt
forStmtML = do
  l <- keywordML "for"
  (_, n) <- identML
  _ <- keywordML "in"
  rangeExpr <- exprML
  case rangeExpr of
    EBinOp _ ".." a b -> do
      _ <- keywordML "do"
      body <- blockIndentedStmts
      newline1
      _ <- string "end"
      return (SFor l n (Range a b) body)
    _ -> fail "Expected range expression with '..'"

simpleStmt :: Parser SimpleStmt
simpleStmt =
  choice
    [ try $ do
        l <- keyword "top"
        SSTop l <$> expr
    , try $ do
        l <- keyword "let"
        pat <- patternP
        _ <- symbol "="
        SSLet l pat <$> expr
    , try $ do
        (lv, l) <- lvalue
        _ <- symbol "="
        SSAssign l lv <$> expr
    , do
        e <- expr
        return (SSExpression (locOfE e) e)
    ]

simpleStmtML :: Parser SimpleStmt
simpleStmtML =
  choice
    [ try $ do
        l <- keywordML "top"
        SSTop l <$> exprML
    , try $ do
        l <- keywordML "let"
        pat <- patternP
        _ <- symbolML "="
        SSLet l pat <$> exprML
    , try $ do
        (lv, l) <- lvalueML
        _ <- symbolML "="
        SSAssign l lv <$> exprML
    , do
        e <- exprML
        return (SSExpression (locOfE e) e)
    ]

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
      , option
      , string
      , try
      , getPosition
      , between
      , lookAhead
      , char
      , many
      , noneOf
      , sepBy
      , notFollowedBy
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
    try inlineStmt <|> do
      ss <- withIndentedBlock stmt
      case ss of
        [x] -> return x
        _ -> let msg = "then-branch must contain exactly one statement"
                 in case l of
                      Just p -> fail (msg ++ " at " ++ show (spLine p) ++ ":" ++ show (spCol p))
                      Nothing -> fail msg
  mElse <-
    optionMaybe $ do
      _ <- try (void newline1) <|> sc
      _ <- keyword "else"
      try inlineStmt <|> do
        ss <- withIndentedBlock stmt
        case ss of
          [x] -> return x
          _ -> let msg = "else-branch must contain exactly one statement"
                   in case l of
                        Just p -> fail (msg ++ " at " ++ show (spLine p) ++ ":" ++ show (spCol p))
                        Nothing -> fail msg
  return (SIf l c sThen mElse)
  where
    inlineStmt = do
      sc
      stmt

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
        base <- basicTerm
        _ <- symbol "."
        (l, n) <- ident
        return (LMember l base n, l)
    )
    <|> try
    ( do
        -- Parse base that can include indexing, but stop before final index
        -- We parse basicTerm and then manually apply postfix operations,
        -- but stop if we see another [ followed by ] and then =
        base <- basicTerm
        baseWithPostfix <- parsePostfixForLValue base
        pos <- getPosition
        idx <- between (symbol "[") (symbol "]") expr
        let l = toLoc pos
        return (LIndex l baseWithPostfix idx, l)
    )
    <|> do
      (l, n) <- ident
      return (LVar l n, l)

-- Parse postfix operations for lvalue, stopping before final index
parsePostfixForLValue :: Expression -> Parser Expression
parsePostfixForLValue e0 =
  choice
    [ try $ do
        pos <- getPosition
        notFollowedBy (string "..")
        _ <- char '.'
        sc
        (_, n) <- ident
        args <- option [] callArgs
        let l = toLoc pos
        let base = EMember l e0 n
        let res = if null args then base else EMethodCall l e0 n args
        parsePostfixForLValue res
    , try $ do
        args <- callArgs
        let l = locOfE e0 in parsePostfixForLValue (ECall l e0 args)
    , try $ do
        -- Check if next is [ followed by ] and then =
        -- If so, stop here (don't consume the index)
        _ <- lookAhead $ do
          _ <- symbol "["
          _ <- many (noneOf "]")
          _ <- symbol "]"
          _ <- lookAhead (symbol "=")
          return ()
        return e0
    , try $ do
        pos <- getPosition
        idx <- between (symbol "[") (symbol "]") expr
        let l = toLoc pos
        parsePostfixForLValue (EIndex l e0 idx)
    , return e0
    ]
  where
    callArgs = between (symbol "(") (symbol ")") (argExpr `sepBy` symbol ",")
    argExpr = do
      scML
      exprML

parsePostfixForLValueML :: Expression -> Parser Expression
parsePostfixForLValueML e0 =
  choice
    [ try $ do
        pos <- getPosition
        notFollowedBy (string "..")
        _ <- char '.'
        scn
        (_, n) <- identML
        args <- option [] callArgsML
        let l = toLoc pos
        let base = EMember l e0 n
        let res = if null args then base else EMethodCall l e0 n args
        parsePostfixForLValueML res
    , try $ do
        args <- callArgsML
        let l = locOfE e0 in parsePostfixForLValueML (ECall l e0 args)
    , try $ do
        -- Check if next is [ followed by ] and then =
        -- If so, stop here (don't consume the index)
        _ <- lookAhead $ do
          _ <- symbolML "["
          _ <- many (noneOf "]")
          _ <- symbolML "]"
          _ <- lookAhead (symbolML "=")
          return ()
        return e0
    , try $ do
        pos <- getPosition
        idx <- between (symbolML "[") (symbolML "]") exprML
        let l = toLoc pos
        parsePostfixForLValueML (EIndex l e0 idx)
    , return e0
    ]
  where
    callArgsML = between (symbolML "(") (symbolML ")") (exprML `sepBy` symbolML ",")

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
        base <- basicTermML
        _ <- symbolML "."
        (l, n) <- identML
        return (LMember l base n, l)
    )
    <|> try
    ( do
        -- Parse base that can include indexing, but stop before final index
        base <- basicTermML
        baseWithPostfix <- parsePostfixForLValueML base
        pos <- getPosition
        idx <- between (symbolML "[") (symbolML "]") exprML
        let l = toLoc pos
        return (LIndex l baseWithPostfix idx, l)
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
    try inlineStmtML <|> do
      ss <- withIndentedBlock stmtML
      case ss of
        [x] -> return x
        _ -> let msg = "then-branch must contain exactly one statement"
                 in case l of
                      Just p -> fail (msg ++ " at " ++ show (spLine p) ++ ":" ++ show (spCol p))
                      Nothing -> fail msg
  mElse <-
    optionMaybe $ do
      _ <- try (void newline1) <|> sc
      _ <- keywordML "else"
      try inlineStmtML <|> do
        ss <- withIndentedBlock stmtML
        case ss of
          [x] -> return x
          _ -> let msg = "else-branch must contain exactly one statement"
                   in case l of
                        Just p -> fail (msg ++ " at " ++ show (spLine p) ++ ":" ++ show (spCol p))
                        Nothing -> fail msg
  return (SIf l c sThen mElse)
  where
    inlineStmtML = do
      sc
      stmtML

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

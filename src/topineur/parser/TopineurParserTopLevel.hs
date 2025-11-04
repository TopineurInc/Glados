{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/topineur/parser/TopineurParserTopLevel.hs
-}

module TopineurParserTopLevel
  ( packageLine
  , importLine
  , decl
  ) where

import Control.Applicative (many)
import Data.List (intercalate)
import Text.Parsec
    ( (<|>)
      , between
      , char
      , getPosition
      , lookAhead
      , manyTill
      , option
      , optionMaybe
      , optional
      , sepBy
      , try
      )

import AST (Name)
import TopineurParserTypes
import TopineurParserUtils
import TopineurParserStatements
import TopineurParserSymbols
import TopineurParserExpressions
import TopineurParserPattern

blockBraced :: Parser Block
blockBraced = do
  pos <- getPosition
  _ <- symbol "{"
  _ <- newline1
  ss <- many (whitespaceWithComments *> stmt <* whitespaceWithComments)
  whitespaceWithComments
  _ <- symbol "}"
  return (Block (toLoc pos) ss)

decorator :: Parser Decorator
decorator = do
  pos <- getPosition
  _ <- symbol "@"
  (_, n) <- ident
  args <-
    option
      []
      ( try (between (symbol "(") (symbol ")") (expr `sepBy` symbol ",")) )
  return (Decorator (toLoc pos) n args)

defDecl :: Parser Decl
defDecl = do
  decs <- many decorator
  whitespaceWithComments
  l <- keyword "def"
  (_, n) <- ident
  ps <- between (symbol "(") (symbol ")") (param `sepBy` symbol ",")
  rt <- optionMaybe (symbol ":" >> typeAnn)
  DFunc l decs n ps rt <$> blockBraced

objectTypeDecl :: Parser Decl
objectTypeDecl = do
  _ <- keyword "object"
  _ <- keyword "type"
  (l, n) <- ident
  _ <- symbolML "{"
  whitespaceWithComments
  mems <- manyTill (whitespaceWithComments *> objMember <* whitespaceWithComments) (lookAhead (symbolML "}"))
  _ <- symbolML "}"
  return (DObjectType l n mems)

objMember :: Parser ObjMember
objMember =
  try
    ( do
        (l, n) <- identML
        _ <- symbolML ":"
        t <- typeAnn
        dv <- optionMaybe (symbolML "=" >> exprML)
        return (OMField l n t dv)
    )
    <|> do OMFunc <$> defDecl

letDecl :: Parser Decl
letDecl = do
  l <- keyword "let"
  pat <- patternP
  _ <- symbol "="
  DLet l pat <$> exprML

decl :: Parser Decl
decl = try defDecl <|> try objectTypeDecl <|> letDecl

packageLine :: Parser Name
packageLine = do
  _ <- keyword "package"
  (_, n) <- ident
  optional newline1
  return n

importLine :: Parser Name
importLine = do
  _ <- keyword "import"
  (_, n) <- identPath
  optional newline1
  return n
  where
    identPath = do
      (l, a) <- ident
      as <- many (char '.' >> fmap snd ident)
      sc
      return (l, intercalate "." (a : as))

{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/topineur/parser/TopineurParserSymbols.hs
-}

module TopineurParserSymbols
  ( isIdentStart
  , isIdentChar
  , reservedWords
  , ident
  , identML
  , upperIdent
  , selfKw
  , keyword
  , keywordML
  ) where

import Text.Parsec
import Data.Char (isAlphaNum, isLetter, isAsciiUpper)

import TopineurParserTypes
import TopineurParserUtils
import AST (Loc, Name)

isIdentStart :: Char -> Bool
isIdentStart c = isLetter c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

reservedWords :: [String]
reservedWords = ["for", "in", "do", "end", "if", "then", "else", "while", "let", "top", "def", "object", "type", "self", "package", "import", "fun", "and", "or", "not"]

ident :: Parser (Loc, Name)
ident = lexeme $ do
  pos <- getPosition
  h <- satisfy isIdentStart <?> "identifier"
  t <- many (satisfy isIdentChar)
  let name = h : t
  if name `elem` reservedWords
    then fail $ "keyword " ++ name ++ " cannot be used as identifier"
    else return (toLoc pos, name)

identML :: Parser (Loc, Name)
identML = lexemeML $ do
  pos <- getPosition
  h <- satisfy isIdentStart <?> "identifier"
  t <- many (satisfy isIdentChar)
  let name = h : t
  if name `elem` reservedWords
    then fail $ "keyword " ++ name ++ " cannot be used as identifier"
    else return (toLoc pos, name)

upperIdent :: Parser (Loc, Name)
upperIdent = lexeme $ do
  pos <- getPosition
  h <- satisfy isAsciiUpper <?> "type identifier"
  t <- many (satisfy isIdentChar)
  return (toLoc pos, h : t)


selfKw :: Parser Loc
selfKw = lexeme $ do
  pos <- getPosition
  _ <- try (string "self" >> notFollowedBy (satisfy isIdentChar))
  return (toLoc pos)

keyword :: String -> Parser Loc
keyword kw = lexeme $ do
  pos <- getPosition
  _ <- try (string kw >> notFollowedBy (satisfy isIdentChar))
  return (toLoc pos)

keywordML :: String -> Parser Loc
keywordML kw = lexemeML $ do
  pos <- getPosition
  _ <- try (string kw >> notFollowedBy (satisfy isIdentChar))
  return (toLoc pos)

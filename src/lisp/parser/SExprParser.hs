{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- SExprParser
-}

module SExprParser
  ( parseSExpr
  , parseSExprs
  , parseFromString
  , parseBool
  , parseSymbol
  , parseAtom
  , parseInteger
  , parseFloat
  , parseString
  , Parser
  ) where

import AST
import Text.Parsec
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)

parseComment :: Parser ()
parseComment = void $ char ';' >> many (noneOf "\n\r") >> optional (oneOf "\n\r")

optionalWhitespace :: Parser ()
optionalWhitespace = void $ many (void (oneOf " \t\n\r") <|> parseComment)

parseString :: Parser String
parseString = between (char '"') (char '"') (many stringChar)
  where
    stringChar = escapedChar <|> noneOf "\""
    escapedChar = char '\\' >> escapeChar
    escapeChar =
      (char '"' >> return '"') <|>
      (char '\\' >> return '\\') <|>
      (char 'n' >> return '\n') <|>
      (char 't' >> return '\t') <|>
      (char 'r' >> return '\r')

parseInteger :: Parser Integer
parseInteger = try $ do
  sign <- optionMaybe $ do
    _ <- char '-'
    _ <- lookAhead digit
    return ()
  digits <- many1 digit
  let num = read digits
  return $ case sign of
    Nothing -> num
    Just _  -> -num

parseFloat :: Parser Double
parseFloat = try $ do
  sign <- optionMaybe (char '-')
  intPart <- many1 digit
  _ <- char '.'
  fracPart <- many1 digit
  let number = read (intPart ++ "." ++ fracPart)
  return $ case sign of
    Nothing -> number
    Just _  -> -number

parseBool :: Parser Bool
parseBool =
  (try (string "#t") >> return True) <|>
  (try (string "#f") >> return False)

parseSymbol :: Parser String
parseSymbol = do
  first <- satisfy (\c -> isAlpha c || c `elem` "+-*/<>=!?_")
  rest <- many (satisfy (\c -> isAlphaNum c || c `elem` "+-*/<>=!?-_"))
  return (first : rest)

parseAtom :: Parser Atom
parseAtom =
  (AFloat <$> parseFloat) <|>
  (AInteger <$> parseInteger) <|>
  (ABool <$> parseBool) <|>
  (AString <$> parseString) <|>
  (ASymbol <$> parseSymbol)

parseSExpr :: Parser SExpr
parseSExpr = between optionalWhitespace optionalWhitespace parseSExprContent
  where
    parseSExprContent =
      getPosition >>= \pos -> parseList pos <|> parseAtomExpr pos
    parseAtomExpr pos =
      SAtom <$> parseAtom <*> pure (Just $ createSourcePos pos)
    parseList pos =
      SList <$> listContents <*> pure (Just $ createSourcePos pos)
    listContents = between (char '(') (char ')') (many parseSExpr)
    createSourcePos pos = AST.SourcePos (sourceLine pos) (sourceColumn pos)

parseSExprs :: Parser [SExpr]
parseSExprs = do
  optionalWhitespace
  exprs <- many parseSExpr
  optionalWhitespace
  eof
  return exprs

parseFromString :: String -> Either CompileError [SExpr]
parseFromString input =
  case parse parseSExprs "" input of
    Left err -> Left $ ParseError (show err) Nothing
    Right exprs -> Right exprs

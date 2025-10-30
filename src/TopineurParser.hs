{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Minimal Topineur parser (skeleton)
-}

module TopineurParser
  ( TopModule(..)
  , parseTopineur
  , parseTopineurFromString
  ) where

import AST (CompileError(..))
import Text.Parsec
import Control.Monad (void)

-- | Minimal representation until the full grammar is implemented
data TopModule = TopModule
  { topPackage :: Maybe String
  } deriving (Eq, Show)

type Parser = Parsec String ()

-- Public API
parseTopineur :: Parser TopModule
parseTopineur = do
  optionalWhitespace
  pkg <- optionMaybe parsePackageHeader
  -- For now we only parse the header and ignore the rest
  return (TopModule pkg)

parseTopineurFromString :: String -> Either CompileError TopModule
parseTopineurFromString input =
  case parse parseTopineur "<topineur>" input of
    Left err  -> Left  $ ParseError (show err) Nothing
    Right ast -> Right ast

-- Internals

-- Comments are lines starting with "|-" (like in examples/all.top)
-- We treat them as whitespace, along with standard spaces/newlines.
optionalWhitespace :: Parser ()
optionalWhitespace = void $ many $ choice
  [ void space
  , void newline
  , lineComment
  ]

lineComment :: Parser ()
lineComment = try $ do
  _ <- string "|-"
  _ <- many (noneOf "\n\r")
  optional (oneOf "\n\r")
  return ()

symbol :: String -> Parser String
symbol s = do
  optionalWhitespace
  r <- string s
  optionalWhitespace
  return r

identifier :: Parser String
identifier = do
  optionalWhitespace
  first <- letter <|> char '_'
  rest  <- many (alphaNum <|> char '_' <|> char '-')
  optionalWhitespace
  return (first:rest)

parsePackageHeader :: Parser String
parsePackageHeader = do
  _ <- symbol "package"
  name <- identifier
  return name

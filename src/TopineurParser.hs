{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Minimal Topineur parser (skeleton)
-}

-- | Parser for Topineur header and type annotations (work-in-progress).
module TopineurParser
  ( TopModule(..)
  , TType(..)
  , parseTopineur
  , parseTopineurFromString
  , parseType
  , parseTypeFromString
  ) where

import AST (CompileError(..))
import Text.Parsec
import Control.Monad (void)

-- | Minimal representation until the full grammar is implemented.
data TopModule = TopModule
  { topPackage :: Maybe String
  } deriving (Eq, Show)

type Parser = Parsec String ()

-- Public API
parseTopineur :: Parser TopModule
parseTopineur = do
  optionalWhitespace
  pkg <- optionMaybe parsePackageHeader
  return (TopModule pkg)

parseTopineurFromString :: String -> Either CompileError TopModule
parseTopineurFromString input =
  case parse parseTopineur "<topineur>" input of
    Left err  -> Left  $ ParseError (show err) Nothing
    Right ast -> Right ast

-- Whitespace and comments. Lines starting with "|-" are comments.
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

-- Types (annotations)

data TType
  = TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TVar String
  | TList TType
  | TTuple [TType]
  | TCustom String [TType]
  deriving (Eq, Show)

parseTypeFromString :: String -> Either CompileError TType
parseTypeFromString s =
  case parse (optionalWhitespace >> parseType <* optionalWhitespace <* eof) "<type>" s of
    Left err -> Left $ ParseError (show err) Nothing
    Right t  -> Right t

parseType :: Parser TType
parseType = parseListType <|> parseTupleType <|> parseSimpleType

parseSimpleType :: Parser TType
parseSimpleType = do
  optionalWhitespace
  name <- typeIdent
  gens <- optionMaybe (brackets (commaSep parseType))
  let mkCustom n ps = case n of
        "List"  -> case ps of
                      [t] -> TList t
                      _   -> TCustom n ps
        "Tuple" -> TTuple ps
        "Int"   -> TInt
        "Float" -> TFloat
        "Bool"  -> TBool
        "String"-> TString
        "Unit"  -> TUnit
        _ | isTypeVar n -> TVar n
          | otherwise   -> TCustom n ps
  return $ mkCustom name (maybe [] id gens)

parseListType :: Parser TType
parseListType = try $ do
  _ <- string "List"
  _ <- optionalWhitespace
  t <- brackets parseType
  return (TList t)

parseTupleType :: Parser TType
parseTupleType = try $ do
  _ <- string "Tuple"
  _ <- optionalWhitespace
  ts <- brackets (commaSep1 parseType)
  return (TTuple ts)

-- Helpers
brackets :: Parser a -> Parser a
brackets p = do
  optionalWhitespace
  _ <- char '['
  optionalWhitespace
  x <- p
  optionalWhitespace
  _ <- char ']'
  optionalWhitespace
  return x

commaSep :: Parser a -> Parser [a]
commaSep p = commaSep1 p <|> pure []

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = do
  x <- p
  xs <- many (optionalWhitespace >> char ',' >> optionalWhitespace >> p)
  return (x:xs)

typeIdent :: Parser String
typeIdent = do
  optionalWhitespace
  first <- letter
  rest <- many (alphaNum <|> oneOf "_")
  optionalWhitespace
  return (first:rest)

isTypeVar :: String -> Bool
isTypeVar [c] = c >= 'A' && c <= 'Z'
isTypeVar _   = False

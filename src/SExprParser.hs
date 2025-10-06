{-# LANGUAGE DeriveFunctor #-}

module SExprParser
  ( parseSExpr
  , parseSExprs
  , parseFromString
  , Parser
  ) where

import AST
import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)

-- | Parser state with position tracking
data ParseState = ParseState
  { psInput :: String
  , psLine  :: Int
  , psCol   :: Int
  } deriving (Show, Eq)

-- | Parser monad with position tracking
newtype Parser a = Parser
  { runParser :: ParseState -> Either CompileError (a, ParseState)
  } deriving (Functor)

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  (<*>) = ap

instance Monad Parser where
  Parser p >>= f = Parser $ \s -> do
    (x, s') <- p s
    runParser (f x) s'

instance Alternative Parser where
  empty = Parser $ \s -> Left $ ParseError "empty" (Just $ SourcePos (psLine s) (psCol s))
  Parser p1 <|> Parser p2 = Parser $ \s ->
    case p1 s of
      Left _ -> p2 s
      Right result -> Right result

-- | Get current position
getCurrentPos :: Parser SourcePos
getCurrentPos = Parser $ \s -> Right (SourcePos (psLine s) (psCol s), s)

-- | Advance position by one character
advancePos :: Char -> ParseState -> ParseState
advancePos '\n' s = s { psLine = psLine s + 1, psCol = 1 }
advancePos _    s = s { psCol = psCol s + 1 }

-- | Parse any character
anyChar :: Parser Char
anyChar = Parser $ \s ->
  case psInput s of
    []     -> Left $ ParseError "unexpected end of input" (Just $ SourcePos (psLine s) (psCol s))
    (c:cs) -> Right (c, advancePos c (s { psInput = cs }))

-- | Parse specific character
char :: Char -> Parser Char
char expected = do
  c <- anyChar
  if c == expected
    then return c
    else Parser $ \s -> Left $ ParseError ("expected '" ++ [expected] ++ "', got '" ++ [c] ++ "'") (Just $ SourcePos (psLine s) (psCol s))

-- | Parse character satisfying predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = do
  c <- anyChar
  if predicate c
    then return c
    else Parser $ \s -> Left $ ParseError ("character does not satisfy predicate") (Just $ SourcePos (psLine s) (psCol s))

-- | Skip whitespace
skipWhitespace :: Parser ()
skipWhitespace = void $ many (satisfy isSpace)

-- | Parse string literal
parseString :: Parser String
parseString = do
  _ <- char '"'
  chars <- many stringChar
  _ <- char '"'
  return chars
  where
    stringChar = satisfy (/= '"') <|> (char '\\' >> char '"' >> return '"')

-- | Parse integer
parseInteger :: Parser Integer
parseInteger = do
  sign <- optional (char '-')
  digits <- some (satisfy isDigit)
  let num = read digits
  return $ case sign of
    Nothing -> num
    Just _  -> -num

-- | Parse boolean
parseBool :: Parser Bool
parseBool =
  (string "#t" >> return True) <|>
  (string "#f" >> return False)

-- | Parse string literal
string :: String -> Parser String
string [] = return []
string (x:xs) = do
  _ <- char x
  _ <- string xs
  return (x:xs)

-- | Parse symbol (identifier)
parseSymbol :: Parser String
parseSymbol = do
  first <- satisfy (\c -> isAlpha c || c `elem` "+-*/<>=!?")
  rest <- many (satisfy (\c -> isAlphaNum c || c `elem` "+-*/<>=!?-"))
  return (first : rest)

-- | Parse atom
parseAtom :: Parser Atom
parseAtom =
  (AInteger <$> parseInteger) <|>
  (ABool <$> parseBool) <|>
  (AString <$> parseString) <|>
  (ASymbol <$> parseSymbol)

-- | Parse S-expression
parseSExpr :: Parser SExpr
parseSExpr = do
  skipWhitespace
  pos <- getCurrentPos
  result <- parseList pos <|> parseAtomExpr pos
  skipWhitespace
  return result
  where
    parseAtomExpr pos = do
      atom <- parseAtom
      return $ SAtom atom (Just pos)

    parseList pos = do
      _ <- char '('
      skipWhitespace
      exprs <- many parseSExpr
      skipWhitespace
      _ <- char ')'
      return $ SList exprs (Just pos)

-- | Parse multiple S-expressions
parseSExprs :: Parser [SExpr]
parseSExprs = do
  skipWhitespace
  exprs <- many parseSExpr
  skipWhitespace
  return exprs

-- | Parse from string with error handling
parseFromString :: String -> Either CompileError [SExpr]
parseFromString input = do
  let initState = ParseState input 1 1
  case runParser parseSExprs initState of
    Left err -> Left err
    Right (exprs, finalState) ->
      if null (psInput finalState)
        then Right exprs
        else Left $ ParseError "unexpected characters at end of input"
                               (Just $ SourcePos (psLine finalState) (psCol finalState))

-- | Test examples

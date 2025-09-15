{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/Parser.hs
-}

module Parser
    ( parseMany
    , parseChar
    , parseNotChar
    , parseAnyChar
    , parseAnd
    , parseAndWith
    , parseString
    , parseSpecificString
    , skipWS
    , token
    , empty
    , pure
    ) where

import Control.Applicative

newtype Parser a =
    Parser
        { runParser :: String -> Maybe (a, String)
        }

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \s ->
            case p s of
                Nothing -> Nothing
                Just (a, s') -> Just (f a, s')

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    Parser pf <*> Parser pa =
        Parser $ \s ->
            case pf s of
                Nothing -> Nothing
                Just (f, s'1) ->
                    case pa s'1 of
                        Nothing -> Nothing
                        Just (a, s'2) -> Just (f a, s'2)

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
    return = pure
    Parser p >>= f =
        Parser $ \s ->
            case p s of
                Nothing -> Nothing
                Just (a, s') -> runParser (f a) s'

instance MonadFail Parser where
    fail _ = empty

parseChar :: Char -> Parser Char
parseChar c =
    Parser $ \input ->
        case input of
            "" -> Nothing
            (x:xs)
                | x == c -> Just (x, xs)
                | otherwise -> Nothing

parseNotChar :: Char -> Parser Char
parseNotChar c =
    Parser $ \input ->
        case input of
            "" -> Nothing
            (x:xs)
                | x /= c -> Just (x, xs)
                | otherwise -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar cs =
    Parser $ \input ->
        case input of
            "" -> Nothing
            (x:xs)
                | x `elem` cs -> Just (x, xs)
                | otherwise -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr (Parser p1) (Parser p2) = Parser $ \s -> p1 s <|> p2 s

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd (Parser pa) (Parser pb) =
    Parser $ \s -> do
        (x, s') <- pa s
        (y, s'') <- pb s'
        return ((x, y), s'')

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f (Parser pa) (Parser pb) =
    Parser $ \s -> do
        (x, s') <- pa s
        (y, s'') <- pb s'
        return (f x y, s'')

parseMany :: Parser a -> Parser [a]
parseMany p = parseOr (parseAndWith (:) p (parseMany p)) (pure [])

parseString :: Parser String
parseString = do
    skipWS
    parseChar '"'
    str <- parseMany (parseNotChar '"')
    parseChar '"'
    skipWS
    pure str

skipWS :: Parser String
skipWS = parseMany (parseAnyChar " \t\n\r")

token :: Parser a -> Parser a
token p = skipWS *> p <* skipWS

parseSpecificString :: String -> Parser String
parseSpecificString "" = pure ""
parseSpecificString (c:cs) = (:) <$> parseChar c <*> parseSpecificString cs

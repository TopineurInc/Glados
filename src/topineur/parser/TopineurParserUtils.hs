module TopineurParserUtils
  ( noTabs
  , failAt
  , comment
  , scn
  , whitespaceWithComments
  , sc
  , scML
  , lexeme
  , lexemeML
  , symbol
  , symbolML
  , newline1
  , toLoc
  , withIndentedBlock
  ) where

import Control.Monad (void, when)
import qualified Text.Parsec as P
import Text.Parsec

import AST (Loc, SourcePos(..))
import TopineurParserTypes

toLoc :: P.SourcePos -> Loc
toLoc p = Just (SourcePos (sourceLine p) (sourceColumn p - 1))

noTabs :: Parser ()
noTabs = do
  pos <- getPosition
  let go = do
        _ <- try (char '\t')
        failAt pos "Tab characters are not allowed; use spaces."
      in return () <|> go

failAt :: P.SourcePos -> String -> Parser a
failAt p msg = fail (msg ++ " at " ++ show (sourceLine p) ++ ":" ++ show (sourceColumn p))

comment :: Parser ()
comment = do
  _ <- try (string "|-")
  _ <- manyTill anyChar (try newline <|> (eof >> return '\n'))
  return ()

scn :: Parser ()
scn = skipMany (noTabs *> void (oneOf " \r"))

whitespaceWithComments :: Parser ()
whitespaceWithComments = skipMany (noTabs *> (void (oneOf " \r\n") <|> comment))

sc :: Parser ()
sc = skipMany (noTabs *> (void (oneOf " \t\r") <|> lineCommentNoNewline))
  where
    lineCommentNoNewline =
      try (string "|-") >> skipMany (satisfy (/= '\n'))

scML :: Parser ()
scML = skipMany (noTabs *> (void (oneOf " \t\r\n") <|> comment))

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  sc
  return x

lexemeML :: Parser a -> Parser a
lexemeML p = do
  x <- p
  scML
  return x

symbol :: String -> Parser String
symbol = lexeme . string

symbolML :: String -> Parser String
symbolML = lexemeML . string

newline1 :: Parser ()
newline1 = do
  _ <- many1 (char '\n')
  scn


withIndentedBlock :: Parser a -> Parser [a]
withIndentedBlock p = do
  st <- getState
  let baseCol = getBaseIndentCol st
  _ <- parseBlockNewlines
  col <- parseIndentation
  unit <- getOrSetIndentUnit st baseCol col
  ensureIndentIsValid baseCol col unit
  many1 (parseIndentedLine col p)

getBaseIndentCol :: IndentState -> Int
getBaseIndentCol st =
  case isStack st of
    (top:_) -> top
    []      -> 0

parseBlockNewlines :: Parser String
parseBlockNewlines = many1 (char '\n')

parseIndentation :: Parser Int
parseIndentation = length <$> many (char ' ')

getOrSetIndentUnit :: IndentState -> Int -> Int -> Parser Int
getOrSetIndentUnit st baseCol col =
  case isUnit st of
    Nothing ->
      let diff = col - baseCol in
      if diff == 2 || diff == 4
        then do
          putState st { isUnit = Just diff, isStack = col : isStack st }
          return diff
        else do
          pos <- getPosition
          failAt pos "Indentation must be 2 or 4 spaces and consistent across the file."
    Just u -> return u

ensureIndentIsValid :: Int -> Int -> Int -> Parser ()
ensureIndentIsValid baseCol col unit =
  when (col - baseCol /= unit && col - baseCol /= unit * 2) $ do
    pos <- getPosition
    failAt pos ("Expected indentation of exactly " ++ show unit ++ " or " ++ show (unit * 2) ++ " spaces.")

parseIndentedLine :: Int -> Parser a -> Parser a
parseIndentedLine targetCol p = do
  x <- p
  choice
    [ try $ do
        _ <- newline
        n <- length <$> many (char ' ')
        sc
        if n == targetCol
          then return x
          else fail "__DEDENT__"
    , return x
    ]

{- OLD VERSION

withIndentedBlock :: Parser a -> Parser [a]
withIndentedBlock p = do
  st <- getState
  let baseCol = case isStack st of
                  (top:_) -> top
                  [] -> 0
  _ <- many1 (char '\n')
  col <- countSpacesAtLineStart
  unit <- case isUnit st of
    Nothing ->
      if col - baseCol == 2 || col - baseCol == 4
        then do
          putState st { isUnit = Just (col - baseCol), isStack = col : isStack st }
          return (col - baseCol)
        else do
          pos <- getPosition
          failAt pos
            "Indentation must be 2 or 4 spaces and consistent across the file."
    Just u -> return u
  when (col - baseCol /= unit && col - baseCol /= unit * 2) $ do
    pos <- getPosition
    failAt pos ("Expected indentation of exactly " ++ show unit ++ " or " ++ show (unit * 2) ++ " spaces.")
  many1 (indentedLine col p)
  where
    countSpacesAtLineStart = do
      n <- length <$> many (char ' ')
      return n
    indentedLine targetCol p' = do
      x <- p'
      choice
        [ try $ do
            _ <- newline
            n <- length <$> many (char ' ')
            sc
            if n == targetCol
              then return x
              else fail "__DEDENT__"
        , return x
        ]
-}


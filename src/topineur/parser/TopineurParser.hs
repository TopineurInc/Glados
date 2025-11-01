{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/topineur/parser/TopineurParser.hs
-}

module TopineurParser
  ( parseTopineurSource
  ) where

import Control.Applicative (many)
import qualified Text.Parsec as P
import Text.Parsec
  ( ParseError
    , eof
    , runParser
    , try
    , (<?>)
    )

import AST (CompileError(..))
import TopineurParserTypes
import TopineurParserUtils
import TopineurParserTopLevel

topineur :: Parser Topineur
topineur = do
  whitespaceWithComments
  pkg <- packageLine <?> "package declaration"
  whitespaceWithComments
  imps <- many (try importLine)
  whitespaceWithComments
  ds <- many (decl <* whitespaceWithComments)
  eof
  return (Topineur pkg imps ds)

findFirstPos :: ParseError -> Maybe P.SourcePos
findFirstPos _ = Nothing

parseTopineurSource :: String -> Either CompileError Topineur
parseTopineurSource src =
  case runParser topineur initState "source.topineur" src of
    Left pe ->
      let msg = show pe
          loc = (toLoc =<< findFirstPos pe)
       in Left (ParseError msg loc)
    Right ast -> Right ast

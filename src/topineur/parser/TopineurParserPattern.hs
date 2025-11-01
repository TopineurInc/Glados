{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/topineur/parser/TopineurParserPattern.hs
-}

module TopineurParserPattern
  ( typeAnn
  , param
  , patternP
  ) where

import Text.Parsec
import Data.Maybe (fromMaybe)

import TopineurParserTypes
import TopineurParserSymbols
import TopineurParserUtils

typeAnn :: Parser TypeAnn
typeAnn = try tupleType <|> try genericType <|> try upperType <|> identType
  where
    identType = do
      (l, n) <- ident
      return (TIdent l n)
    upperType = do
      (l, n) <- upperIdent
      return (TUpperIdent l n)
    genericType = do
      (l, n) <- ident
      _ <- symbol "["
      ts <- typeAnn `sepBy1` symbol ","
      _ <- symbol "]"
      return (TGeneric l n ts)
    tupleType = do
      l0 <- keyword "Tuple"
      _ <- symbol "["
      ts <- typeAnn `sepBy1` symbol ","
      _ <- symbol "]"
      return (TTuple l0 ts)

param :: Parser Param
param = try tupleParam <|> singleParam
  where
    singleParam = do
      (l, n) <- ident
      mt <- optionMaybe (symbol ":" >> typeAnn)
      return (PVar l n (fromMaybe (TIdent l "Any") mt))
    tupleParam = do
      pos <- getPosition
      _ <- symbol "("
      ps <- param `sepBy1` symbol ","
      _ <- symbol ")"
      return (PTuple (toLoc pos) ps)

patternP :: Parser Pattern
patternP = try tuplePat <|> varPat
  where
    varPat = do
      (l, n) <- ident
      mt <- optionMaybe (symbol ":" >> typeAnn)
      return (PVarPat l n mt)
    tuplePat = do
      pos <- getPosition
      _ <- symbol "("
      ps <- patternP `sepBy1` symbol ","
      _ <- symbol ")"
      return (PTuplePat (toLoc pos) ps)

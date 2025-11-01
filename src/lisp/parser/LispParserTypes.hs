{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/lisp/parser/Types.hs
-}

{-# LANGUAGE DeriveGeneric #-}

module LispParserTypes
  ( Parser
  , SExpr(..)
  , Atom(..)
  , sexprLoc
  , Expr(..)
  ) where

import AST (Loc)
import Text.Parsec
import GHC.Generics (Generic)

type Name = String

data SExpr
  = SAtom Atom Loc
  | SList [SExpr] Loc
  deriving (Eq, Show, Generic)

data Atom
  = AInteger Integer
  | AFloat Double
  | ABool Bool
  | ASymbol String
  | AString String
  deriving (Eq, Show, Generic)

sexprLoc :: SExpr -> Loc
sexprLoc (SAtom _ loc) = loc
sexprLoc (SList _ loc) = loc

data Expr
  = EInt Integer
  | EFloat Double
  | EBool Bool
  | EString String
  | EVar Name
  | EList [Expr]
  | ELambda [Name] Expr
  | EDefine Name Expr
  | EIf Expr Expr Expr
  | EApp Expr [Expr]
  | EQuote SExpr
  deriving (Eq, Show, Generic)

type Parser = Parsec String ()

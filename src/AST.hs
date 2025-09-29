{-# LANGUAGE DeriveGeneric #-}

module AST
  ( SourcePos(..)
  , Loc
  , SExpr(..)
  , Atom(..)
  , CompileError(..)
  , sexprLoc
  ) where

import GHC.Generics (Generic)

data SourcePos = SourcePos
  { spLine :: Int
  , spCol  :: Int
  } deriving (Eq, Show, Ord, Generic)

type Loc = Maybe SourcePos

data SExpr
  = SAtom Atom Loc
  | SList [SExpr] Loc
  deriving (Eq, Show, Generic)

data Atom
  = AInteger Integer
  | ABool Bool
  | ASymbol String
  | AString String
  deriving (Eq, Show, Generic)

data CompileError
  = ParseError String Loc
  | SyntaxError String Loc
  deriving (Eq, Show, Generic)

sexprLoc :: SExpr -> Loc
sexprLoc (SAtom _ loc) = loc
sexprLoc (SList _ loc) = loc

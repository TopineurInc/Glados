{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TopineurParser
-}

{-# LANGUAGE DeriveGeneric #-}

module TopineurParser
  (
  ) where

import AST
import Control.Applicative ((<|>), many)
import Control.Monad (void, when)
import Data.Char (isAlphaNum, isLetter, isSpace)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Text.Parsec

data Topineur = Topineur
  { tPackage :: Name
  , tImports :: [Name]
  , tDecls   :: [Decl]
  } deriving (Eq, Show, Generic)

data Decl
  = DLet Loc Pattern (Maybe TypeAnn) Expression
  | DFunc Loc [Decorator] Name [Param] (Maybe TypeAnn) Block
  | DObjectType Loc Name [ObjMember]
  deriving (Eq, Show, Generic)

data Decorator = Decorator Loc Name [Expression]
  deriving (Eq, Show, Generic)

data ObjMember
  = OMField Loc Name TypeAnn (Maybe Expression)
  | OMFunc Decl -- reuse DFunc form for methods
  deriving (Eq, Show, Generic)

data Param
  = PVar Loc Name TypeAnn
  | PTuple Loc [Param]
  deriving (Eq, Show, Generic)

data Pattern
  = PVarPat Loc Name
  | PTuplePat Loc [Pattern]
  deriving (Eq, Show, Generic)

data Block = Block Loc [Stmt]
  deriving (Eq, Show, Generic)

data Stmt
  = STop Loc Expression
  | SLet Loc Pattern (Maybe TypeAnn) Expression
  | SIf Loc Expression SimpleStmt (Maybe SimpleStmt)
  | SWhile Loc Expression [Stmt]
  | SFor Loc Name Range [Stmt]
  | SAssign Loc LValue Expression
  | SExpression Loc Expression
  deriving (Eq, Show, Generic)

data SimpleStmt
  = SSLet Loc Pattern (Maybe TypeAnn) Expression
  | SSAssign Loc LValue Expression
  | SSTop Loc Expression
  | SSExpression Loc Expression
  deriving (Eq, Show, Generic)

data LValue
  = LVar Loc Name
  | LMember Loc Expression Name
  deriving (Eq, Show, Generic)

data Range = Range Expression Expression
  deriving (Eq, Show, Generic)

data Expression
  = EVar Loc Name
  | ESelf Loc
  | EInt Loc Integer
  | EFloat Loc Double
  | EString Loc String
  | ETuple Loc [Expression]
  | EArray Loc [Expression]
  | EObject Loc Name [FieldAssign]
  | ECall Loc Expression [Expression]
  | EMethodCall Loc Expression Name [Expression]
  | EMember Loc Expression Name
  | EIf Loc Expression Expression (Maybe Expression)
  | ELet Loc Name Expression Expression
  | ELambda Loc [Param] (Maybe TypeAnn) Expression
  | EBinOp Loc String Expression Expression
  | EParens Loc Expression
  deriving (Eq, Show, Generic)

data FieldAssign = FieldAssign Loc (Either (Expression, Name) Name) Expression
  deriving (Eq, Show, Generic)

data TypeAnn
  = TIdent Loc Name
  | TUpperIdent Loc Name
  | TGeneric Loc Name [TypeAnn]
  | TTuple Loc [TypeAnn]
  deriving (Eq, Show, Generic)

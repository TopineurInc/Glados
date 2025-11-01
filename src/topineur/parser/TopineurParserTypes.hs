{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/topineur/parser/TopineurParserTypes.hs
-}

{-# LANGUAGE DeriveGeneric #-}

module TopineurParserTypes
  ( Parser
  , locOfE
  , Topineur(..)
  , Decl(..)
  , Decorator(..)
  , ObjMember(..)
  , Param(..)
  , Pattern(..)
  , Block(..)
  , Stmt(..)
  , SimpleStmt(..)
  , LValue(..)
  , Range(..)
  , Expression(..)
  , FieldAssign(..)
  , TypeAnn(..)
  , IndentState(..)
  , initState
  ) where

import AST (Name, Loc, SourcePos)
import GHC.Generics (Generic)
import Text.Parsec

data Topineur = Topineur
  { tPackage :: Name
  , tImports :: [Name]
  , tDecls   :: [Decl]
  } deriving (Eq, Show, Generic)

data Decl
  = DLet Loc Pattern Expression
  | DFunc Loc [Decorator] Name [Param] (Maybe TypeAnn) Block
  | DObjectType Loc Name [ObjMember]
  deriving (Eq, Show, Generic)

data Decorator = Decorator Loc Name [Expression]
  deriving (Eq, Show, Generic)

data ObjMember
  = OMField Loc Name TypeAnn (Maybe Expression)
  | OMFunc Decl
  deriving (Eq, Show, Generic)

data Param
  = PVar Loc Name TypeAnn
  | PTuple Loc [Param]
  deriving (Eq, Show, Generic)

data Pattern
  = PVarPat Loc Name (Maybe TypeAnn)
  | PTuplePat Loc [Pattern]
  deriving (Eq, Show, Generic)

data Block = Block Loc [Stmt]
  deriving (Eq, Show, Generic)

data Stmt
  = STop Loc Expression
  | SLet Loc Pattern Expression
  | SIf Loc Expression SimpleStmt (Maybe SimpleStmt)
  | SWhile Loc Expression [Stmt]
  | SFor Loc Name Range [Stmt]
  | SAssign Loc LValue Expression
  | SExpression Loc Expression
  deriving (Eq, Show, Generic)

-- TODO: Remove SimpleStmt, there should only be Stmt
data SimpleStmt
  = SSLet Loc Pattern Expression
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

locOfE :: Expression -> Loc
locOfE e =
  case e of
    EVar l _ -> l
    ESelf l -> l
    EInt l _ -> l
    EFloat l _ -> l
    EString l _ -> l
    ETuple l _ -> l
    EArray l _ -> l
    EObject l _ _ -> l
    ECall l _ _ -> l
    EMethodCall l _ _ _ -> l
    EMember l _ _ -> l
    EIf l _ _ _ -> l
    ELet l _ _ _ -> l
    ELambda l _ _ _ -> l
    EBinOp l _ _ _ -> l
    EParens l _ -> l

data IndentState = IndentState
  { isUnit   :: Maybe Int
  , isStack  :: [Int]
  } deriving (Show, Eq)

initState :: IndentState
initState = IndentState { isUnit = Nothing, isStack = [0] }

type Parser a = Parsec String IndentState a

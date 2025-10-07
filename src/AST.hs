{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- AST
-}

{-# LANGUAGE DeriveGeneric #-}

module AST
  ( SourcePos(..)
  , Loc
  , SExpr(..)
  , Atom(..)
  , CompileError(..)
  , sexprLoc
  , Name
  , Expr(..)
  , ANF(..)
  , Constant(..)
  , Instr(..)
  , CodeObject(..)
  , Value(..)
  , Frame(..)
  , VMState(..)
  , Label
  ) where

import GHC.Generics (Generic)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

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

type Name = String

data Expr
  = EInt Integer
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

data ANF
  = AVar Name
  | AConst Constant
  | ALet Name ANF ANF
  | AIf ANF ANF ANF
  | ACall Name [Name]
  | ALambdaFlat [Name] [Instr]
  deriving (Eq, Show, Generic)

data Constant
  = CInt Integer
  | CBool Bool
  | CString String
  | CFuncRef Name
  deriving (Eq, Show, Generic)

type Label = String

data Instr
  = IConst Int
  | ILoad Int
  | IStore Int
  | IPrim String
  | ICall Int Name
  | ITailCall Int Name
  | IReturn
  | IJump Int
  | IJumpIfFalse Int
  | IPop
  | INop
  | IMakeClosure Name [Int]
  | ILoadClosure Int
  | IStoreClosure Int
  deriving (Eq, Show, Generic)

data CodeObject = CodeObject
  { coName :: Name
  , coArity :: Int
  , coMaxLocals :: Int
  , coConsts :: Vector.Vector Constant
  , coInstrs :: Vector.Vector Instr
  , coLabelMap :: Map.Map Label Int
  } deriving (Eq, Show, Generic)

data Value
  = VInt Integer
  | VBool Bool
  | VString String
  | VClosure Name [Value]
  | VBuiltin Name ([Value] -> IO Value)

instance Eq Value where
  (VInt a) == (VInt b) = a == b
  (VBool a) == (VBool b) = a == b
  (VString a) == (VString b) = a == b
  (VClosure n1 env1) == (VClosure n2 env2) = n1 == n2 && env1 == env2
  (VBuiltin n1 _) == (VBuiltin n2 _) = n1 == n2
  _ == _ = False

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VString s) = show s
  show (VClosure name env) = "VClosure " ++ name ++ " " ++ show env
  show (VBuiltin name _) = "VBuiltin " ++ name

data Frame = Frame
  { fLocals :: Vector.Vector (Maybe Value)
  , fStack :: [Value]
  , fCode :: CodeObject
  , fPC :: Int
  } deriving (Eq, Show, Generic)

data VMState = VMState
  { vFrames :: [Frame]
  , vGlobals :: Map.Map Name Value
  , vCodeObjects :: Map.Map Name CodeObject
  , vBuiltins :: Map.Map Name Value
  } deriving (Eq, Show, Generic)

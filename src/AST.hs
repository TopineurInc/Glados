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
  -- Topineur additions
  , Type(..)
  , EffectRow(..)
  , Effect(..)
  , ObjectDef(..)
  , MethodDef(..)
  , MethodSig(..)
  , TraitDef(..)
  , TraitImpl(..)
  , Pattern(..)
  , VTable
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
  | AFloat Double
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
  -- Topineur additions
  | EObjectDef ObjectDef
  | ETraitDef TraitDef
  | ETraitImpl TraitImpl
  | EObjectLit Name [(Name, Expr)]        -- Point { x = 1.0, y = 2.0 }
  | EMethodCall Expr Name [Expr]          -- obj.method(args)
  | EFieldAccess Expr Name                -- obj.field
  | ETyped Expr Type                      -- expr : Type
  | ELinearBind Name Expr Expr            -- let !lin x = expr in body
  | EMatch Expr [(Pattern, Expr)]         -- match expr { case pattern => expr ... }
  | EBlock [Expr]                         -- { stmt; stmt; expr }
  | ELet Name (Maybe Type) Expr Expr      -- let x : Type = expr in body
  deriving (Eq, Show, Generic)

-- Topineur Type System
data Type
  = TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TObject Name                          -- Point, Counter, etc.
  | TLinear Type                          -- !lin T (linear ownership)
  | TFunc [Type] EffectRow Type           -- (T1, T2, ...) !{effects} -> T
  | TVar Name                             -- Type variable (for inference)
  | TList Type                            -- [T]
  | TTuple [Type]                         -- (T1, T2, ...)
  | TLazy Type                            -- Lazy[T]
  deriving (Eq, Show, Generic)

-- Topineur Effect System
data EffectRow = EffectRow [Effect]
  deriving (Eq, Show, Generic)

data Effect
  = EffIO
  | EffState
  | EffNetwork
  | EffException
  | EffAsync
  | EffCustom Name
  deriving (Eq, Show, Generic)

-- Topineur Object Definitions
data ObjectDef = ObjectDef
  { objName :: Name
  , objTypeParams :: [Name]               -- Type parameters [T, U, ...]
  , objFields :: [(Name, Type)]           -- Fields: name : type
  , objMethods :: [MethodDef]             -- Methods
  } deriving (Eq, Show, Generic)

data MethodDef = MethodDef
  { methName :: Name
  , methParams :: [(Name, Type)]
  , methReturnType :: Type
  , methEffects :: EffectRow
  , methBody :: Expr
  } deriving (Eq, Show, Generic)

-- Topineur Trait System
data TraitDef = TraitDef
  { traitName :: Name
  , traitTypeParams :: [Name]
  , traitBounds :: [(Name, Name)]         -- Type bounds: T: Trait
  , traitMethods :: [MethodSig]
  } deriving (Eq, Show, Generic)

data MethodSig = MethodSig
  { sigName :: Name
  , sigParams :: [(Name, Type)]
  , sigReturnType :: Type
  , sigEffects :: EffectRow
  } deriving (Eq, Show, Generic)

data TraitImpl = TraitImpl
  { implTrait :: Name
  , implForType :: Type
  , implWhereClauses :: [(Name, Name)]    -- where T: Trait
  , implMethods :: [MethodDef]
  } deriving (Eq, Show, Generic)

-- Pattern Matching
data Pattern
  = PLit Constant                         -- 42, "hello", #t
  | PVar Name                             -- x
  | PConstructor Name [Pattern]           -- Some(x), Point(x, y)
  | PWildcard                             -- _
  | PTuple [Pattern]                      -- (x, y, z)
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
  | CFloat Double
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
  -- Topineur instructions
  | IMakeObject Name Int                  -- Create object with N fields
  | IGetField Name                        -- Get field from object
  | ISetField Name                        -- Set field (for mutable objects)
  | IMethodCall Name Int                  -- Call method with N args
  | ILoadDict Name                        -- Load trait dictionary
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
  | VFloat Double
  | VBool Bool
  | VString String
  | VClosure Name [Value]
  | VBuiltin Name ([Value] -> IO Value)
  | VVoid
  -- Topineur additions
  | VObject Name (Map.Map Name Value) VTable    -- Object with fields and vtable
  | VTraitDict Name (Map.Map Name Value)        -- Trait dictionary
  | VList [Value]                               -- List of values
  | VTuple [Value]                              -- Tuple of values

-- VTable for object method dispatch
type VTable = Map.Map Name Value

instance Eq Value where
  (VInt a) == (VInt b) = a == b
  (VFloat a) == (VFloat b) = a == b
  (VBool a) == (VBool b) = a == b
  (VString a) == (VString b) = a == b
  (VClosure n1 env1) == (VClosure n2 env2) = n1 == n2 && env1 == env2
  (VBuiltin n1 _) == (VBuiltin n2 _) = n1 == n2
  VVoid == VVoid = True
  (VObject n1 f1 _) == (VObject n2 f2 _) = n1 == n2 && f1 == f2
  (VTraitDict n1 d1) == (VTraitDict n2 d2) = n1 == n2 && d1 == d2
  (VList vs1) == (VList vs2) = vs1 == vs2
  (VTuple vs1) == (VTuple vs2) = vs1 == vs2
  _ == _ = False

instance Show Value where
  show (VInt i) = show i
  show (VFloat f) = show f
  show (VBool b) = show b
  show (VString s) = show s
  show (VClosure name env) = "VClosure " ++ name ++ " " ++ show env
  show (VBuiltin name _) = "VBuiltin " ++ name
  show VVoid = "#<void>"
  show (VObject name fields _) = "VObject " ++ name ++ " " ++ show fields
  show (VTraitDict name _) = "VTraitDict " ++ name
  show (VList vs) = "[" ++ unwords (map show vs) ++ "]"
  show (VTuple vs) = "(" ++ unwords (map show vs) ++ ")"

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

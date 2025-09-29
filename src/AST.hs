{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module AST
  ( -- * Source Position and Location
    SourcePos(..)
  , Loc
  , Name
  , Label
    -- * S-Expressions
  , SExpr(..)
  , Atom(..)
    -- * Surface AST
  , Expr(..)
    -- * ANF (A-Normal Form)
  , ANF(..)
    -- * Code Generation
  , Constant(..)
  , CodeObject(..)
  , Instr(..)
    -- * Runtime Values
  , Value(..)
  , Frame(..)
  , VMState(..)
  , Builtin(..)
    -- * Compiler Configuration
  , MacroMode(..)
  , CompilerConfig(..)
  , CompilerState(..)
    -- * Monad Types
  , CompilerM
  , EvalM
    -- * Error Types
  , CompileError(..)
  , RuntimeError(..)
    -- * Utility Functions
  , exprLoc
  , sexprLoc
  , gensym
  , addConstant
  , defaultConfig
  , initialState
  , initialVMState
  ) where

import qualified Data.Map as Map
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except

-- | Source position for diagnostics
data SourcePos = SourcePos
  { spLine :: Int
  , spCol  :: Int
  } deriving (Eq, Show, Ord, Generic)

-- | Optional source location
type Loc = Maybe SourcePos

-- | Name type for variables and functions
type Name = String

-- | Label for jump instructions
type Label = String

-- =============================================================================
-- 1. SExpr - Immediate representation after parsing
-- =============================================================================

-- | S-Expression representation (language-agnostic)
data SExpr
  = SAtom Atom Loc
  | SList [SExpr] Loc
  deriving (Eq, Show, Generic)

-- | Atomic values in S-expressions
data Atom
  = AInteger Integer
  | ABool Bool
  | ASymbol String
  | AString String
  deriving (Eq, Show, Generic)

-- =============================================================================
-- 2. Surface AST (Expr) - Primary types after desugaring
-- =============================================================================

-- | Surface AST expression types
data Expr
  = EInt Integer Loc
  | EBool Bool Loc
  | EString String Loc
  | EVar Name Loc                    -- variable / symbol
  | EList [Expr] Loc                 -- literal list (optional)
  | ELambda [Name] Expr Loc          -- (lambda (args) body)
  | EDefine Name Expr Loc            -- (define name expr)
  | EIf Expr Expr Expr Loc           -- (if cond then else)
  | EApp Expr [Expr] Loc             -- (f arg1 arg2 ...)
  | EQuote SExpr Loc                 -- (quote ...)
  deriving (Eq, Show, Generic)

-- =============================================================================
-- 3. ANF (A-Normal Form) - Optional intermediate representation
-- =============================================================================

-- | ANF for easier stack-based code generation
data ANF
  = AVar Name Loc
  | AConst Constant Loc
  | ALet Name ANF ANF Loc            -- let x = expr in body
  | AIf ANF ANF ANF Loc
  | ACall Name [Name] Loc            -- call to variable name with arg names
  | ALambdaFlat [Name] [Instr] Loc   -- directly lower to CodeObject
  deriving (Eq, Show, Generic)

-- =============================================================================
-- 4. Constants and CodeObject (JVM-style IR)
-- =============================================================================

-- | Runtime constants
data Constant
  = CInt Integer
  | CBool Bool
  | CString String
  | CFuncRef Name                    -- reference to function
  deriving (Eq, Show, Generic)

-- | Code object representing a compiled function
data CodeObject = CodeObject
  { coName      :: Name              -- unique identifier (fn#123) or user name
  , coArity     :: Int               -- number of arguments expected
  , coMaxLocals :: Int               -- size of locals (args + temporaries)
  , coConsts    :: Vector Constant   -- constant pool
  , coInstrs    :: Vector Instr      -- instructions (PC-indexed)
  , coLabelMap  :: Map.Map Label Int -- label -> pc (resolved)
  } deriving (Eq, Show, Generic)

-- =============================================================================
-- 5. Instruction Set (Stack Machine)
-- =============================================================================

-- | Stack machine instructions
data Instr
  = IConst Int                       -- push coConsts[idx] -> stack +1
  | ILoad Int                        -- push locals[slot] -> stack +1
  | IStore Int                       -- pop -> locals[slot] -> stack -1
  | IPrim String                     -- pop N args (according to op) -> push result
  | ICall Int Name                   -- pop arity args -> create new Frame
  | ITailCall Int Name               -- pop arity args -> reuse current frame (TCO)
  | IReturn                          -- pop result -> destroy frame -> push result on caller
  | IJump Int                        -- set pc = targetPC
  | IJumpIfFalse Int                 -- pop cond (bool) -> if false pc = targetPC else pc++
  | INop                             -- no-op
  | IMakeClosure Name [Int]          -- create closure capturing envSlots -> push closure
  | ILoadClosure Int                 -- load from closure environment
  | IStoreClosure Int                -- store to closure environment
  | ILabel Label                     -- pseudo-instruction for labels (removed after linking)
  deriving (Eq, Show, Generic)

-- =============================================================================
-- 6. Value Model & Runtime
-- =============================================================================

-- | Runtime values
data Value
  = VInt Integer
  | VBool Bool
  | VString String
  | VClosure Name [Value]            -- reference to CodeObject name + env vector
  | VBuiltin Name ([Value] -> EvalM Value) -- primitives

instance Eq Value where
  (VInt a) == (VInt b) = a == b
  (VBool a) == (VBool b) = a == b
  (VString a) == (VString b) = a == b
  (VClosure n1 env1) == (VClosure n2 env2) = n1 == n2 && env1 == env2
  (VBuiltin n1 _) == (VBuiltin n2 _) = n1 == n2
  _ == _ = False

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = if b then "#t" else "#f"
  show (VString s) = "\"" ++ s ++ "\""
  show (VClosure name env) = "<closure:" ++ name ++ ":" ++ show (length env) ++ ">"
  show (VBuiltin name _) = "<builtin:" ++ name ++ ">"

-- =============================================================================
-- 7. Frame and VM State
-- =============================================================================

-- | Execution frame
data Frame = Frame
  { fLocals :: Vector (Maybe Value)  -- size = coMaxLocals
  , fStack  :: [Value]               -- operand stack per frame
  , fCode   :: CodeObject
  , fPC     :: Int
  } deriving (Eq, Show, Generic)

-- | VM state
data VMState = VMState
  { vFrames     :: [Frame]                    -- top = current frame
  , vGlobals    :: Map.Map Name Value         -- top-level binds
  , vCodeObjects :: Map.Map Name CodeObject   -- loader registry
  , vBuiltins   :: Map.Map Name Builtin       -- primitives like +,-,eq?
  } deriving (Show, Generic)

-- | Builtin function type
data Builtin = Builtin
  { builtinName :: Name
  , builtinArity :: Int
  , builtinImpl :: [Value] -> EvalM Value
  }

instance Show Builtin where
  show (Builtin name arity _) = "<builtin:" ++ name ++ "/" ++ show arity ++ ">"

instance Eq Builtin where
  b1 == b2 = builtinName b1 == builtinName b2

-- =============================================================================
-- 8. Compiler Configuration and State
-- =============================================================================

-- | Macro expansion mode
data MacroMode
  = HostMacros                       -- macros defined in Host (Haskell)
  | RuntimeMacros                    -- macros written in LISP
  deriving (Eq, Show, Generic)

-- | Compiler configuration
data CompilerConfig = CompilerConfig
  { ccMacroMode :: MacroMode
  , ccTCOEnabled :: Bool             -- Tail Call Optimization
  , ccDebugMode :: Bool
  } deriving (Eq, Show, Generic)

-- | Compiler state
data CompilerState = CompilerState
  { csGensymCounter :: Int
  , csConstantPool :: [Constant]
  , csCodeObjects :: Map.Map Name CodeObject
  , csCurrentCode :: Maybe CodeObject
  } deriving (Eq, Show, Generic)

-- =============================================================================
-- 9. Monad Stack and Error Handling
-- =============================================================================

-- | Compiler monad stack
type CompilerM a = ReaderT CompilerConfig (StateT CompilerState (Writer [String])) a

-- | Evaluation monad for runtime
type EvalM a = ExceptT String IO a

-- | Compilation errors
data CompileError
  = ParseError String Loc
  | SyntaxError String Loc
  | TypeError String Loc
  | UnboundVariable Name Loc
  | ArityMismatch Int Int Loc        -- expected actual
  | MacroExpansionError String Loc
  deriving (Eq, Show, Generic)

-- | Runtime errors
data RuntimeError
  = DivisionByZero Loc
  | UnboundVariableRuntime Name
  | TypeMismatch String String       -- expected actual
  | ArityMismatchRuntime Int Int     -- expected actual
  | StackUnderflow
  | InvalidInstruction Instr
  deriving (Eq, Show, Generic)

-- =============================================================================
-- 10. Utility functions
-- =============================================================================

-- | Extract location from expressions
exprLoc :: Expr -> Loc
exprLoc expr = case expr of
  EInt _ loc -> loc
  EBool _ loc -> loc
  EString _ loc -> loc
  EVar _ loc -> loc
  EList _ loc -> loc
  ELambda _ _ loc -> loc
  EDefine _ _ loc -> loc
  EIf _ _ _ loc -> loc
  EApp _ _ loc -> loc
  EQuote _ loc -> loc

-- | Extract location from S-expressions
sexprLoc :: SExpr -> Loc
sexprLoc (SAtom _ loc) = loc
sexprLoc (SList _ loc) = loc

-- | Generate a unique name
gensym :: String -> CompilerM String
gensym prefix = do
  counter <- gets csGensymCounter
  modify $ \s -> s { csGensymCounter = counter + 1 }
  return $ prefix ++ "#" ++ show counter

-- | Add a constant to the pool and return its index
addConstant :: Constant -> CompilerM Int
addConstant constant = do
  pool <- gets csConstantPool
  let idx = length pool
  modify $ \s -> s { csConstantPool = pool ++ [constant] }
  return idx

-- | Default compiler configuration
defaultConfig :: CompilerConfig
defaultConfig = CompilerConfig
  { ccMacroMode = HostMacros
  , ccTCOEnabled = True
  , ccDebugMode = False
  }

-- | Initial compiler state
initialState :: CompilerState
initialState = CompilerState
  { csGensymCounter = 0
  , csConstantPool = []
  , csCodeObjects = Map.empty
  , csCurrentCode = Nothing
  }

-- | Initial VM state
initialVMState :: VMState
initialVMState = VMState
  { vFrames = []
  , vGlobals = Map.empty
  , vCodeObjects = Map.empty
  , vBuiltins = Map.empty
  }
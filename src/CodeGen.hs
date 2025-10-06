{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen
  ( generateCode
  , generateCodeWithDefs
  , CodeGenState(..)
  , emptyCodeGenState
  ) where

import AST
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Set as Set

data CodeGenState = CodeGenState
  { cgsCounter :: Int
  , cgsConstants :: [Constant]
  , cgsInstructions :: [Instr]
  , cgsLabels :: Map.Map Label Int
  , cgsCodeObjects :: Map.Map Name CodeObject
  , cgsLocalMap :: Map.Map Name Int
  , cgsMaxLocals :: Int
  } deriving (Show)

emptyCodeGenState :: CodeGenState
emptyCodeGenState = CodeGenState 0 [] [] Map.empty Map.empty Map.empty 0

newtype CodeGenM a = CodeGenM { unCodeGenM :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)

runCodeGen :: CodeGenM a -> CodeGenState -> (a, CodeGenState)
runCodeGen m s = runState (unCodeGenM m) s

-- Generate bytecode from an expression
generateCode :: Name -> Expr -> Either CompileError CodeObject
generateCode name expr = do
  let (mainCode, _) = generateCodeWithDefs name expr
  mainCode

-- Generate bytecode and return both main code and all nested definitions
generateCodeWithDefs :: Name -> Expr -> (Either CompileError CodeObject, Map.Map Name CodeObject)
generateCodeWithDefs name expr =
  let (_, finalState) = runCodeGen (compileExpr expr >> emit IReturn) emptyCodeGenState
      instrs = Vector.fromList (reverse $ cgsInstructions finalState)
      consts = Vector.fromList (reverse $ cgsConstants finalState)
      mainCode = Right $ CodeObject
        { coName = name
        , coArity = getArity expr
        , coMaxLocals = cgsMaxLocals finalState
        , coConsts = consts
        , coInstrs = instrs
        , coLabelMap = cgsLabels finalState
        }
  in (mainCode, cgsCodeObjects finalState)

getArity :: Expr -> Int
getArity (ELambda params _) = length params
getArity _ = 0

-- Emit an instruction
emit :: Instr -> CodeGenM ()
emit instr = modify $ \s -> s { cgsInstructions = instr : cgsInstructions s }

-- Add a constant to the pool and return its index
addConst :: Constant -> CodeGenM Int
addConst c = do
  s <- get
  let consts = cgsConstants s
  let idx = length consts
  put s { cgsConstants = c : consts }
  return idx

-- Allocate a local slot
allocLocal :: Name -> CodeGenM Int
allocLocal name = do
  s <- get
  let localMap = cgsLocalMap s
  let idx = Map.size localMap
  put s { cgsLocalMap = Map.insert name idx localMap
        , cgsMaxLocals = max (idx + 1) (cgsMaxLocals s)
        }
  return idx

-- Get local slot for a name
getLocal :: Name -> CodeGenM (Maybe Int)
getLocal name = do
  localMap <- gets cgsLocalMap
  return $ Map.lookup name localMap

-- Compile an expression (leaves result on stack)
compileExpr :: Expr -> CodeGenM ()
compileExpr (EInt n) = do
  idx <- addConst (CInt n)
  emit (IConst idx)

compileExpr (EBool b) = do
  idx <- addConst (CBool b)
  emit (IConst idx)

compileExpr (EString s) = do
  idx <- addConst (CString s)
  emit (IConst idx)

compileExpr (EVar name) = do
  mSlot <- getLocal name
  case mSlot of
    Just slot -> emit (ILoad slot)
    Nothing -> do
      -- Assume it's a global/builtin
      idx <- addConst (CFuncRef name)
      emit (IConst idx)

compileExpr (EIf cond thenE elseE) = do
  compileExpr cond
  emit (IJumpIfFalse 0)  -- Placeholder
  elseLabelIdx <- gets (length . cgsInstructions)

  compileExpr thenE
  emit (IJump 0)  -- Placeholder
  endLabelIdx <- gets (length . cgsInstructions)

  -- Patch else jump
  patchJump (elseLabelIdx - 1) endLabelIdx

  compileExpr elseE

  -- Patch end jump
  finalIdx <- gets (length . cgsInstructions)
  patchJump (endLabelIdx - 1) finalIdx

compileExpr (ELambda _ _) = do
  -- For now, generate a simple closure (simplified)
  -- In full implementation, this would create a separate CodeObject
  return ()

compileExpr (EApp (EVar funcName) args) = do
  -- Compile arguments (push onto stack)
  mapM_ compileExpr args
  -- Check if it's a builtin primitive
  case funcName of
    "+" -> emit (IPrim "+")
    "-" -> emit (IPrim "-")
    "*" -> emit (IPrim "*")
    "div" -> emit (IPrim "div")
    "mod" -> emit (IPrim "mod")
    "eq?" -> emit (IPrim "eq?")
    "<" -> emit (IPrim "<")
    ">" -> emit (IPrim ">")
    "print" -> emit (IPrim "print")
    _ -> emit (ICall (length args) funcName)

compileExpr (EApp func args) = do
  -- More complex application
  compileExpr func
  mapM_ compileExpr args
  emit (ICall (length args) "<lambda>")

compileExpr (EDefine name expr) = do
  case expr of
    ELambda params body -> do
      -- Compile the lambda as a separate code object
      codeObj <- compileLambda name params body
      -- Register the code object
      modify $ \s -> s { cgsCodeObjects = Map.insert name codeObj (cgsCodeObjects s) }
      -- Push a function reference and store it
      idx <- addConst (CFuncRef name)
      emit (IConst idx)
      slot <- allocLocal name
      emit (IStore slot)
      -- Push a dummy value so EList's IPop has something to pop
      dummyIdx <- addConst (CInt 0)
      emit (IConst dummyIdx)
    _ -> do
      compileExpr expr
      slot <- allocLocal name
      emit (IStore slot)
      -- Push a dummy value so EList's IPop has something to pop
      dummyIdx <- addConst (CInt 0)
      emit (IConst dummyIdx)

compileExpr (EList exprs) = do
  -- Begin block: evaluate all expressions, keep only the last value
  case exprs of
    [] -> return ()  -- Empty begin evaluates to nothing
    [e] -> compileExpr e  -- Single expression
    (e:es) -> do
      compileExpr e  -- Evaluate first expression
      emit (IPop)    -- Pop its value (we don't need it)
      compileExpr (EList es)  -- Continue with rest

compileExpr (EQuote _) = do
  -- Quote: return the quoted value as-is
  return ()

-- Patch a jump instruction
patchJump :: Int -> Int -> CodeGenM ()
patchJump instrIdx target = do
  s <- get
  let instrs = cgsInstructions s
  let len = length instrs
  -- Instructions are stored in reverse, so convert index
  let reverseIdx = len - instrIdx - 1
  if reverseIdx >= 0 && reverseIdx < len
    then do
      let (before, instr:after) = splitAt reverseIdx instrs
      let patchedInstr = case instr of
            IJumpIfFalse _ -> IJumpIfFalse target
            IJump _ -> IJump target
            other -> other
      put s { cgsInstructions = before ++ (patchedInstr : after) }
    else return ()

-- Compile a lambda as a separate code object
compileLambda :: Name -> [Name] -> Expr -> CodeGenM CodeObject
compileLambda name params body = do
  -- Save current state
  currentState <- get
  -- Create a fresh state for the lambda with the parameters as locals
  let freshState = emptyCodeGenState
  let (_, lambdaState) = runCodeGen (compileLambdaBody params body) freshState
  -- Restore current state but keep the nested code objects
  put currentState
  -- Build the code object for this lambda
  let instrs = Vector.fromList (reverse $ cgsInstructions lambdaState)
  let consts = Vector.fromList (reverse $ cgsConstants lambdaState)
  let codeObj = CodeObject
        { coName = name
        , coArity = length params
        , coMaxLocals = cgsMaxLocals lambdaState
        , coConsts = consts
        , coInstrs = instrs
        , coLabelMap = cgsLabels lambdaState
        }
  -- Merge nested code objects from lambda into current state
  modify $ \s -> s { cgsCodeObjects = Map.union (cgsCodeObjects s) (cgsCodeObjects lambdaState) }
  return codeObj

-- Compile lambda body with parameters allocated
compileLambdaBody :: [Name] -> Expr -> CodeGenM ()
compileLambdaBody params body = do
  -- Allocate locals for parameters
  mapM_ allocLocal params
  -- Compile body
  compileExpr body
  -- Return the result
  emit IReturn
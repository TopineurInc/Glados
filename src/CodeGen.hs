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

generateCode :: Name -> Expr -> Either CompileError CodeObject
generateCode name expr = do
  let (mainCode, _) = generateCodeWithDefs name expr
  mainCode

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

emit :: Instr -> CodeGenM ()
emit instr = modify $ \s -> s { cgsInstructions = instr : cgsInstructions s }

addConst :: Constant -> CodeGenM Int
addConst c = do
  s <- get
  let consts = cgsConstants s
  let idx = length consts
  put s { cgsConstants = c : consts }
  return idx

allocLocal :: Name -> CodeGenM Int
allocLocal name = do
  s <- get
  let localMap = cgsLocalMap s
  let idx = Map.size localMap
  put s { cgsLocalMap = Map.insert name idx localMap
        , cgsMaxLocals = max (idx + 1) (cgsMaxLocals s)
        }
  return idx

getLocal :: Name -> CodeGenM (Maybe Int)
getLocal name = do
  localMap <- gets cgsLocalMap
  return $ Map.lookup name localMap

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
      idx <- addConst (CFuncRef name)
      emit (IConst idx)

compileExpr (EIf cond thenE elseE) = do
  compileExpr cond
  emit (IJumpIfFalse 0)
  elseLabelIdx <- gets (length . cgsInstructions)

  compileExpr thenE
  emit (IJump 0)
  endLabelIdx <- gets (length . cgsInstructions)

  patchJump (elseLabelIdx - 1) endLabelIdx

  compileExpr elseE

  finalIdx <- gets (length . cgsInstructions)
  patchJump (endLabelIdx - 1) finalIdx

compileExpr (ELambda _ _) = return ()

compileExpr (EApp (EVar funcName) args) = do
  mapM_ compileExpr args
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
    "display" -> emit (IPrim "display")
    "input" -> emit (IPrim "input")
    "read-line" -> emit (IPrim "read-line")
    "string->number" -> emit (IPrim "string->number")
    "number->string" -> emit (IPrim "number->string")
    "string-length" -> emit (IPrim "string-length")
    "string-append" -> emit (IPrim "string-append")
    "substring" -> emit (IPrim "substring")
    "not" -> emit (IPrim "not")
    "and" -> emit (IPrim "and")
    "or" -> emit (IPrim "or")
    _ -> emit (ICall (length args) funcName)

compileExpr (EApp func args) = do
  compileExpr func
  mapM_ compileExpr args
  emit (ICall (length args) "<lambda>")

compileExpr (EDefine name expr) = do
  case expr of
    ELambda params body -> do
      codeObj <- compileLambda name params body
      modify $ \s -> s { cgsCodeObjects = Map.insert name codeObj (cgsCodeObjects s) }
      idx <- addConst (CFuncRef name)
      emit (IConst idx)
      slot <- allocLocal name
      emit (IStore slot)
      dummyIdx <- addConst (CInt 0)
      emit (IConst dummyIdx)
    _ -> do
      compileExpr expr
      slot <- allocLocal name
      emit (IStore slot)
      dummyIdx <- addConst (CInt 0)
      emit (IConst dummyIdx)

compileExpr (EList exprs) = case exprs of
  [] -> return ()
  [e] -> compileExpr e
  (e:es) -> do
    compileExpr e
    emit (IPop)
    compileExpr (EList es)

compileExpr (EQuote _) = return ()

patchJump :: Int -> Int -> CodeGenM ()
patchJump instrIdx target = do
  s <- get
  let instrs = cgsInstructions s
  let len = length instrs
  let reverseIdx = len - instrIdx - 1
  if reverseIdx >= 0 && reverseIdx < len
    then case splitAt reverseIdx instrs of
      (before, instr:after) -> do
        let patchedInstr = case instr of
              IJumpIfFalse _ -> IJumpIfFalse target
              IJump _ -> IJump target
              other -> other
        put s { cgsInstructions = before ++ (patchedInstr : after) }
      _ -> return ()
    else return ()

compileLambda :: Name -> [Name] -> Expr -> CodeGenM CodeObject
compileLambda name params body = do
  currentState <- get
  let freshState = emptyCodeGenState
  let (_, lambdaState) = runCodeGen (compileLambdaBody params body) freshState
  put currentState
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
  modify $ \s -> s { cgsCodeObjects = Map.union (cgsCodeObjects s) (cgsCodeObjects lambdaState) }
  return codeObj

compileLambdaBody :: [Name] -> Expr -> CodeGenM ()
compileLambdaBody params body = do
  mapM_ allocLocal params
  compileExpr body
  emit IReturn
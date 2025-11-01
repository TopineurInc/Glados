{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- CodeGen
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen
  ( generateCode
  , generateCodeWithDefs
  , CodeGenState(..)
  , emptyCodeGenState
  ) where

import AST
import Control.Monad.State
import Control.Monad (forM_)
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

freshName :: String -> CodeGenM Name
freshName prefix = do
  n <- gets cgsCounter
  modify $ \s -> s { cgsCounter = n + 1 }
  return (prefix ++ show n)

runCodeGen :: CodeGenM a -> CodeGenState -> (a, CodeGenState)
runCodeGen m s = runState (unCodeGenM m) s

generateCode :: Name -> Expr -> Either CompileError CodeObject
generateCode name expr =
  let (mainCode, _) = generateCodeWithDefs name expr
  in mainCode

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
getArity (ELambda params _ _ _) = length params
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

compileExpr (EFloat n) = do
  idx <- addConst (CFloat n)
  emit (IConst idx)

compileExpr (EBool b) = do
  idx <- addConst (CBool b)
  emit (IConst idx)

compileExpr (EString s) = do
  idx <- addConst (CString s)
  emit (IConst idx)

compileExpr EUnit = do
  idx <- addConst (CInt 0)
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

compileExpr (ELambda params _retType body _) = do
  lambdaName <- freshName "lambda#"
  codeObj <- compileLambda lambdaName (map fst params) body
  modify $ \s -> s { cgsCodeObjects = Map.insert lambdaName codeObj (cgsCodeObjects s) }
  idx <- addConst (CFuncRef lambdaName)
  emit (IConst idx)

compileExpr (EApp (EVar funcName) args) =
  mapM_ compileExpr args >> emitCall
  where
    emitCall = case funcName of
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

compileExpr (EApp func args) =
  compileExpr func
    >> mapM_ compileExpr args
    >> emit (ICall (length args) "<lambda>")

compileExpr (EDefine name expr _) =
  case expr of
    ELambda params _retType body _ann -> do
      codeObj <- compileLambda name (map fst params) body
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
  (e:es) ->
    compileExpr e
      >> emit IPop
      >> compileExpr (EList es)

compileExpr (EQuote _) = return ()

-- Control Flow: Loops
compileExpr (EWhile cond body) = do
  loopStart <- gets (length . cgsInstructions)
  compileExpr cond
  emit (IJumpIfFalse 0)
  exitJumpIdx <- gets (length . cgsInstructions)
  
  compileExpr body
  emit IPop
  
  emit (IJump loopStart)
  
  exitIdx <- gets (length . cgsInstructions)
  patchJump (exitJumpIdx - 1) exitIdx
  
  -- Push unit value as while result
  idx <- addConst (CInt 0)
  emit (IConst idx)

compileExpr (EFor var start end body) = do
  compileExpr start
  iterSlot <- allocLocal var
  emit (IStore iterSlot)
  
  loopStart <- gets (length . cgsInstructions)
  emit (ILoad iterSlot)
  compileExpr end
  emit (IPrim "<")
  emit (IJumpIfFalse 0)
  exitJumpIdx <- gets (length . cgsInstructions)
  
  compileExpr body
  emit IPop
  
  emit (ILoad iterSlot)
  idx <- addConst (CInt 1)
  emit (IConst idx)
  emit (IPrim "+")
  emit (IStore iterSlot)
  
  emit (IJump loopStart)
  
  exitIdx <- gets (length . cgsInstructions)
  patchJump (exitJumpIdx - 1) exitIdx
  
  -- Push unit value as for result
  unitIdx <- addConst (CInt 0)
  emit (IConst unitIdx)

compileExpr (ERange start end) = do
  compileExpr start
  compileExpr end
  emit IRangeCreate

compileExpr (EReturn expr) = do
  compileExpr expr
  emit IReturn

-- Operators
compileExpr (EBinOp op left right) = do
  compileExpr left
  compileExpr right
  case op of
    Add -> emit (IPrim "+")
    Sub -> emit (IPrim "-")
    Mul -> emit (IPrim "*")
    Div -> emit (IPrim "div")
    Mod -> emit (IPrim "mod")
    Lt -> emit (IPrim "<")
    Lte -> emit (IPrim "<=")
    Gt -> emit (IPrim ">")
    Gte -> emit (IPrim ">=")
    Eq -> emit (IPrim "eq?")
    Neq -> do
      emit (IPrim "eq?")
      emit (IPrim "not")
    And -> emit (IPrim "and")
    Or -> emit (IPrim "or")
    Concat -> emit (IPrim "string-append")

compileExpr (EUnOp op expr) = do
  case op of
    Not -> do
      compileExpr expr
      emit (IPrim "not")
    Neg -> do
      idx <- addConst (CInt 0)
      emit (IConst idx)
      compileExpr expr
      emit (IPrim "-")

-- Tuples
compileExpr (ETuple exprs) = do
  mapM_ compileExpr exprs
  emit (ITupleCreate (length exprs))

compileExpr (ETupleDestruct names tupleExpr body) = do
  compileExpr tupleExpr
  slots <- mapM allocLocal names
  forM_ (zip slots [0..]) $ \(slot, idx) -> do
    emit (ITupleGet idx)
    emit (IStore slot)
  compileExpr body

compileExpr (EIndex tuple (EInt n)) = do
  compileExpr tuple
  emit (ITupleGet (fromInteger n))

compileExpr (EIndex expr idx) = do
  compileExpr expr
  compileExpr idx
  emit IListGet

-- Lists
compileExpr (EListLiteral exprs _type) = do
  mapM_ compileExpr exprs
  emit (IListCreate (length exprs))

-- Objects
compileExpr (EObjectDecl _name _fields _methods) = do
  -- For now, object declarations just push a unit value
  -- The actual object type system will be handled by the type checker
  -- and objects are instantiated with EObjectInst
  idx <- addConst (CInt 0)
  emit (IConst idx)

compileExpr (EObjectInst typeName fieldInits) = do
  forM_ fieldInits $ \(_fieldName, expr) -> compileExpr expr
  emit (IObjectCreate typeName)

compileExpr (EMemberAccess obj memberName) = do
  compileExpr obj
  emit (IMemberGet memberName)

-- Assignment
compileExpr (EAssign name expr) = do
  compileExpr expr
  mSlot <- getLocal name
  case mSlot of
    Just slot -> emit (IAssign slot)
    Nothing -> error $ "Assignment to undefined variable: " ++ name
  compileExpr (EVar name)

-- Package and Import (no-ops for now)
compileExpr (EPackage _name) = do
  idx <- addConst (CInt 0)
  emit (IConst idx)

compileExpr (EImport _name) = do
  idx <- addConst (CInt 0)
  emit (IConst idx)

patchJump :: Int -> Int -> CodeGenM ()
patchJump instrIdx target = do
  s <- get
  let instrs = cgsInstructions s
  let len = length instrs
  let reverseIdx = len - instrIdx - 1
  if reverseIdx >= 0 && reverseIdx < len
    then case splitAt reverseIdx instrs of
      (before, instr:after) ->
        let patchedInstr = case instr of
              IJumpIfFalse _ -> IJumpIfFalse target
              IJump _ -> IJump target
              other -> other
        in put s { cgsInstructions = before ++ (patchedInstr : after) }
      _ -> return ()
    else return ()

compileLambda :: Name -> [Name] -> Expr -> CodeGenM CodeObject
compileLambda name params body = do
  currentState <- get
  let freshState = emptyCodeGenState { cgsCounter = cgsCounter currentState }
  let (_, lambdaState) = runCodeGen (compileLambdaBody params body) freshState
  put currentState { cgsCounter = cgsCounter lambdaState }
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
compileLambdaBody params body =
  mapM_ allocLocal params
    >> compileExpr body
    >> emit IReturn

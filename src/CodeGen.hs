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

compileExpr (EFloat n) = do
  idx <- addConst (CFloat n)
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

compileExpr (ELambda params body) = do
  lambdaName <- freshName "lambda#"
  codeObj <- compileLambda lambdaName params body
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
      "<=" -> emit (IPrim "<=")
      ">=" -> emit (IPrim ">=")
      "print" -> emit (IPrim "print")
      "println" -> emit (IPrim "println")
      "display" -> emit (IPrim "display")
      "show" -> emit (IPrim "show")
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
      "format" -> emit (IPrim "format")
      _ -> emit (ICall (length args) funcName)

compileExpr (EApp func args) =
  compileExpr func
    >> mapM_ compileExpr args
    >> emit (ICall (length args) "<lambda>")

compileExpr (EDefine name expr) =
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
  (e:es) ->
    compileExpr e
      >> emit IPop
      >> compileExpr (EList es)

compileExpr (EQuote _) = return ()

-- Topineur constructs
compileExpr (EBlock exprs) = case exprs of
  [] -> do
    idx <- addConst (CInt 0)
    emit (IConst idx)
  [e] -> compileExpr e
  (e:es) ->
    compileExpr e
      >> emit IPop
      >> compileExpr (EBlock es)

compileExpr (ELet name maybeType valExpr body) = do
  compileExpr valExpr
  slot <- allocLocal name
  emit (IStore slot)
  compileExpr body

compileExpr (ETyped expr _) = compileExpr expr

compileExpr (EFieldAccess objExpr fieldName) = do
  compileExpr objExpr
  emit (IGetField fieldName)

compileExpr (EObjectLit objName fields) = do
  mapM_ (\(_, expr) -> compileExpr expr) fields
  emit (IMakeObject objName (length fields))

compileExpr (EMethodCall objExpr methodName args) = do
  mapM_ compileExpr args
  compileExpr objExpr
  emit (IMethodCall methodName (length args))

compileExpr (EObjectDef objDef) = do
  -- For now, compile object definitions as no-op
  -- In full implementation, we'd register the object type
  idx <- addConst (CInt 0)
  emit (IConst idx)

compileExpr (ETraitDef _) = do
  -- Trait definitions are compile-time only
  idx <- addConst (CInt 0)
  emit (IConst idx)

compileExpr (ETraitImpl _) = do
  -- Trait implementations are compile-time only
  idx <- addConst (CInt 0)
  emit (IConst idx)

compileExpr (ELinearBind name valExpr body) = do
  -- For now, treat linear bindings like regular let bindings
  compileExpr valExpr
  slot <- allocLocal name
  emit (IStore slot)
  compileExpr body

compileExpr (EMatch scrutinee cases) = do
  -- Simplified pattern matching - compile first matching case
  -- Full implementation would need proper pattern compilation
  compileExpr scrutinee
  emit IPop  -- Pop the scrutinee for now
  case cases of
    [] -> do
      idx <- addConst (CInt 0)
      emit (IConst idx)
    ((_, body):_) -> compileExpr body

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

{-# LANGUAGE LambdaCase #-}

module VM
  ( runVM
  , execVM
  , initVMState
  , VMError(..)
  ) where

import AST
import Builtins
import qualified Data.Map as Map
import qualified Data.Vector as Vector

data VMError
  = StackUnderflow
  | InvalidInstruction String
  | UndefinedFunction Name
  | TypeError String
  | RuntimeError String
  deriving (Eq, Show)

initVMState :: VMState
initVMState = VMState
  { vFrames = []
  , vGlobals = Map.empty
  , vCodeObjects = Map.empty
  , vBuiltins = builtins
  }

execVM :: VMState -> CodeObject -> IO (Either VMError Value)
execVM vmState code = do
  let initialFrame = Frame
        { fLocals = Vector.replicate (coMaxLocals code) Nothing
        , fStack = []
        , fCode = code
        , fPC = 0
        }
  let vmState' = vmState { vFrames = [initialFrame] }
  runVM vmState'

runVM :: VMState -> IO (Either VMError Value)
runVM vmState = case vFrames vmState of
  [] -> return $ Left $ RuntimeError "No frames to execute"
  (frame:_) -> do
    let pc = fPC frame
    let code = fCode frame
    let instrs = coInstrs code

    if pc >= Vector.length instrs
      then return $ Left $ RuntimeError "PC out of bounds"
      else do
        let instr = instrs Vector.! pc
        result <- executeInstr vmState frame instr
        case result of
          Left err -> return $ Left err
          Right (vmState', Nothing) -> runVM vmState'
          Right (_, Just val) -> return $ Right val

updateFrame :: VMState -> Frame -> VMState
updateFrame vmState newFrame = case vFrames vmState of
  (_:rest) -> vmState { vFrames = newFrame : rest }
  [] -> vmState
executeInstr :: VMState -> Frame -> Instr -> IO (Either VMError (VMState, Maybe Value))
executeInstr vmState frame instr = case instr of
  IConst idx -> do
    let consts = coConsts (fCode frame)
    if idx >= Vector.length consts
      then return $ Left $ RuntimeError "Constant index out of bounds"
      else do
        let constValue = consts Vector.! idx
        let val = constantToValue constValue
        let frame' = frame { fStack = val : fStack frame, fPC = fPC frame + 1 }
        return $ Right (updateFrame vmState frame', Nothing)

  ILoad slot -> do
    let locals = fLocals frame
    if slot >= Vector.length locals
      then return $ Left $ RuntimeError "Local slot out of bounds"
      else case locals Vector.! slot of
        Nothing -> return $ Left $ RuntimeError "Uninitialized local variable"
        Just val -> do
          let frame' = frame { fStack = val : fStack frame, fPC = fPC frame + 1 }
          return $ Right (updateFrame vmState frame', Nothing)

  IStore slot -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (val:stack') -> do
      let locals = fLocals frame
      let locals' = locals Vector.// [(slot, Just val)]
      let frame' = frame { fStack = stack', fLocals = locals', fPC = fPC frame + 1 }
      return $ Right (updateFrame vmState frame', Nothing)

  IPrim op -> executePrim vmState frame op

  ICall arity funcName -> executeCall vmState frame arity funcName False

  ITailCall arity funcName -> executeCall vmState frame arity funcName True

  IReturn -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (val:_) -> case vFrames vmState of
      [] -> return $ Left $ RuntimeError "No frames for return"
      [_] -> return $ Right (vmState, Just val)
      (_:callerFrame:rest) -> do
        let callerFrame' = callerFrame { fStack = val : fStack callerFrame, fPC = fPC callerFrame + 1 }
        return $ Right (vmState { vFrames = callerFrame' : rest }, Nothing)

  IJump target -> do
    let frame' = frame { fPC = target }
    return $ Right (updateFrame vmState frame', Nothing)

  IJumpIfFalse target -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (VBool False : stack') -> do
      let frame' = frame { fStack = stack', fPC = target }
      return $ Right (updateFrame vmState frame', Nothing)
    (_:stack') -> do
      let frame' = frame { fStack = stack', fPC = fPC frame + 1 }
      return $ Right (updateFrame vmState frame', Nothing)

  IPop -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (_:stack') -> do
      let frame' = frame { fStack = stack', fPC = fPC frame + 1 }
      return $ Right (updateFrame vmState frame', Nothing)

  INop -> do
    let frame' = frame { fPC = fPC frame + 1 }
    return $ Right (updateFrame vmState frame', Nothing)

  _ -> return $ Left $ InvalidInstruction "Instruction not implemented"

executePrim :: VMState -> Frame -> String -> IO (Either VMError (VMState, Maybe Value))
executePrim vmState frame op =
  case Map.lookup op (vBuiltins vmState) of
    Nothing -> return $ Left $ UndefinedFunction op
    Just (VBuiltin _ func) -> case fStack frame of
      (arg2:arg1:stack') -> do
        result <- func [arg1, arg2]
        let frame' = frame { fStack = result : stack', fPC = fPC frame + 1 }
        return $ Right (updateFrame vmState frame', Nothing)
      _ -> return $ Left StackUnderflow
    Just _ -> return $ Left $ TypeError "Not a builtin function"
executeCall :: VMState -> Frame -> Int -> Name -> Bool -> IO (Either VMError (VMState, Maybe Value))
executeCall vmState frame arity funcName isTail = do
  let (args, stack') = splitAt arity (fStack frame)
  if length args < arity
    then return $ Left StackUnderflow
    else case Map.lookup funcName (vBuiltins vmState) of
      Just (VBuiltin _ func) -> do
        result <- func (reverse args)
        let frame' = frame { fStack = result : stack', fPC = fPC frame + 1 }
        case vFrames vmState of
          (_:rest) -> return $ Right (vmState { vFrames = frame' : rest }, Nothing)
          [] -> return $ Left $ RuntimeError "No frames for builtin call"

      Just _ -> return $ Left $ TypeError "Not a builtin function"

      Nothing -> case Map.lookup funcName (vCodeObjects vmState) of
        Just code -> do
          let newLocals = Vector.replicate (coMaxLocals code) Nothing
          let argsVector = Vector.fromList (map Just (reverse args))
          let newLocals' = newLocals Vector.// zip [0..] (Vector.toList argsVector)
          let newFrame = Frame
                { fLocals = newLocals'
                , fStack = []
                , fCode = code
                , fPC = 0
                }
          let frame' = frame { fStack = stack' }
          case vFrames vmState of
            (_:rest) ->
              let vmState' = if isTail
                    then vmState { vFrames = newFrame : rest }
                    else vmState { vFrames = newFrame : frame' : rest }
              in return $ Right (vmState', Nothing)
            [] -> return $ Left $ RuntimeError "No frames for function call"

        Nothing -> return $ Left $ UndefinedFunction funcName
constantToValue :: Constant -> Value
constantToValue (CInt n) = VInt n
constantToValue (CBool b) = VBool b
constantToValue (CString s) = VString s
constantToValue (CFuncRef name) = VBuiltin name undefined
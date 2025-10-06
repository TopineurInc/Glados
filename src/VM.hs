{-# LANGUAGE LambdaCase #-}

module VM
  ( runVM
  , execVM
  , initVMState
  , VMError(..)
  ) where

import AST
import Builtins
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

data VMError
  = StackUnderflow
  | InvalidInstruction String
  | UndefinedFunction Name
  | TypeError String
  | RuntimeError String
  deriving (Eq, Show)

-- Initialize VM state with builtins
initVMState :: VMState
initVMState = VMState
  { vFrames = []
  , vGlobals = Map.empty
  , vCodeObjects = Map.empty
  , vBuiltins = builtins
  }

-- Execute a code object in the VM
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

-- Main VM execution loop
runVM :: VMState -> IO (Either VMError Value)
runVM vmState = case vFrames vmState of
  [] -> return $ Left $ RuntimeError "No frames to execute"
  (frame:rest) -> do
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
          Right (vmState', Nothing) -> runVM vmState'  -- Continue
          Right (_, Just val) -> return $ Right val    -- Return value

-- Execute a single instruction
executeInstr :: VMState -> Frame -> Instr -> IO (Either VMError (VMState, Maybe Value))
executeInstr vmState frame instr = case instr of
  IConst idx -> do
    let consts = coConsts (fCode frame)
    if idx >= Vector.length consts
      then return $ Left $ RuntimeError "Constant index out of bounds"
      else do
        let const = consts Vector.! idx
        let val = constantToValue const
        let frame' = frame { fStack = val : fStack frame, fPC = fPC frame + 1 }
        return $ Right (vmState { vFrames = frame' : tail (vFrames vmState) }, Nothing)

  ILoad slot -> do
    let locals = fLocals frame
    if slot >= Vector.length locals
      then return $ Left $ RuntimeError "Local slot out of bounds"
      else case locals Vector.! slot of
        Nothing -> return $ Left $ RuntimeError "Uninitialized local variable"
        Just val -> do
          let frame' = frame { fStack = val : fStack frame, fPC = fPC frame + 1 }
          return $ Right (vmState { vFrames = frame' : tail (vFrames vmState) }, Nothing)

  IStore slot -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (val:stack') -> do
      let locals = fLocals frame
      let locals' = locals Vector.// [(slot, Just val)]
      let frame' = frame { fStack = stack', fLocals = locals', fPC = fPC frame + 1 }
      return $ Right (vmState { vFrames = frame' : tail (vFrames vmState) }, Nothing)

  IPrim op -> do
    result <- executePrim vmState frame op
    return result

  ICall arity funcName -> do
    result <- executeCall vmState frame arity funcName False
    return result

  ITailCall arity funcName -> do
    result <- executeCall vmState frame arity funcName True
    return result

  IReturn -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (val:_) -> case tail (vFrames vmState) of
      [] -> return $ Right (vmState, Just val)  -- Top-level return
      (callerFrame:rest) -> do
        let callerFrame' = callerFrame { fStack = val : fStack callerFrame, fPC = fPC callerFrame + 1 }
        return $ Right (vmState { vFrames = callerFrame' : rest }, Nothing)

  IJump target -> do
    let frame' = frame { fPC = target }
    return $ Right (vmState { vFrames = frame' : tail (vFrames vmState) }, Nothing)

  IJumpIfFalse target -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (VBool False : stack') -> do
      let frame' = frame { fStack = stack', fPC = target }
      return $ Right (vmState { vFrames = frame' : tail (vFrames vmState) }, Nothing)
    (_:stack') -> do
      let frame' = frame { fStack = stack', fPC = fPC frame + 1 }
      return $ Right (vmState { vFrames = frame' : tail (vFrames vmState) }, Nothing)

  IPop -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (_:stack') -> do
      let frame' = frame { fStack = stack', fPC = fPC frame + 1 }
      return $ Right (vmState { vFrames = frame' : tail (vFrames vmState) }, Nothing)

  INop -> do
    let frame' = frame { fPC = fPC frame + 1 }
    return $ Right (vmState { vFrames = frame' : tail (vFrames vmState) }, Nothing)

  _ -> return $ Left $ InvalidInstruction "Instruction not implemented"

-- Execute a primitive operation
executePrim :: VMState -> Frame -> String -> IO (Either VMError (VMState, Maybe Value))
executePrim vmState frame op =
  case Map.lookup op (vBuiltins vmState) of
    Nothing -> return $ Left $ UndefinedFunction op
    Just (VBuiltin _ func) -> do
      -- Arity depends on the operation (simplified: assume 2 args for now)
      case fStack frame of
        (arg2:arg1:stack') -> do
          result <- func [arg1, arg2]
          let frame' = frame { fStack = result : stack', fPC = fPC frame + 1 }
          return $ Right (vmState { vFrames = frame' : tail (vFrames vmState) }, Nothing)
        _ -> return $ Left StackUnderflow
    Just _ -> return $ Left $ TypeError "Not a builtin function"

-- Execute a function call
executeCall :: VMState -> Frame -> Int -> Name -> Bool -> IO (Either VMError (VMState, Maybe Value))
executeCall vmState frame arity funcName isTail = do
  -- Pop arguments from stack
  let (args, stack') = splitAt arity (fStack frame)
  if length args < arity
    then return $ Left StackUnderflow
    else do
      -- Look up function (in globals, builtins, or code objects)
      case Map.lookup funcName (vBuiltins vmState) of
        Just (VBuiltin _ func) -> do
          result <- func (reverse args)
          let frame' = frame { fStack = result : stack', fPC = fPC frame + 1 }
          return $ Right (vmState { vFrames = frame' : tail (vFrames vmState) }, Nothing)

        Nothing -> case Map.lookup funcName (vCodeObjects vmState) of
          Just code -> do
            -- Create new frame
            let newLocals = Vector.replicate (coMaxLocals code) Nothing
            let argsVector = Vector.fromList (map Just (reverse args))
            let newLocals' = newLocals Vector.// zip [0..] (Vector.toList argsVector)
            let newFrame = Frame
                  { fLocals = newLocals'
                  , fStack = []
                  , fCode = code
                  , fPC = 0
                  }
            -- Update caller frame to remove arguments from stack
            let frame' = frame { fStack = stack' }
            let vmState' = if isTail
                  then vmState { vFrames = newFrame : tail (vFrames vmState) }
                  else vmState { vFrames = newFrame : frame' : tail (vFrames vmState) }
            return $ Right (vmState', Nothing)

          Nothing -> return $ Left $ UndefinedFunction funcName

-- Convert constant to runtime value
constantToValue :: Constant -> Value
constantToValue (CInt n) = VInt n
constantToValue (CBool b) = VBool b
constantToValue (CString s) = VString s
constantToValue (CFuncRef name) = VBuiltin name undefined  -- Placeholder
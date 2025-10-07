{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- VM
-}

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
execVM vmState code =
  let initialFrame = Frame
        { fLocals = Vector.replicate (coMaxLocals code) Nothing
        , fStack = []
        , fCode = code
        , fPC = 0
        }
      vmState' = vmState { vFrames = [initialFrame] }
  in runVM vmState'

runVM :: VMState -> IO (Either VMError Value)
runVM vmState =
  case vFrames vmState of
    [] -> pure $ Left $ RuntimeError "No frames to execute"
    (frame:_) -> stepVM vmState frame

stepVM :: VMState -> Frame -> IO (Either VMError Value)
stepVM vmState frame =
  case fetchInstr frame of
    Left err -> pure $ Left err
    Right instr ->
      executeInstr vmState frame instr >>= handleStep

handleStep :: Either VMError (VMState, Maybe Value) -> IO (Either VMError Value)
handleStep (Left err) = pure $ Left err
handleStep (Right (newState, Nothing)) = runVM newState
handleStep (Right (_, Just val)) = pure $ Right val

fetchInstr :: Frame -> Either VMError Instr
fetchInstr frame =
  let pc = fPC frame
      code = fCode frame
      instrs = coInstrs code
  in if pc >= Vector.length instrs
       then Left $ RuntimeError "PC out of bounds"
       else Right (instrs Vector.! pc)

updateFrame :: VMState -> Frame -> VMState
updateFrame vmState newFrame =
  case vFrames vmState of
    (_:rest) -> vmState { vFrames = newFrame : rest }
    [] -> vmState

executeInstr :: VMState -> Frame -> Instr -> IO (Either VMError (VMState, Maybe Value))
executeInstr vmState frame instr =
  case instr of
    IConst idx -> pure $ executeConst vmState frame idx
    ILoad slot -> pure $ executeLoad vmState frame slot
    IStore slot -> pure $ executeStore vmState frame slot
    IPrim op -> executePrim vmState frame op
    ICall arity name -> executeCall vmState frame arity name False
    ITailCall arity name -> executeCall vmState frame arity name True
    _ -> executeControlInstr vmState frame instr

executeControlInstr :: VMState -> Frame -> Instr -> IO (Either VMError (VMState, Maybe Value))
executeControlInstr vmState frame instr =
  case instr of
    IReturn -> pure $ executeReturn vmState frame
    IJump target -> pure $ executeJump vmState frame target
    IJumpIfFalse target -> pure $ executeConditionalJump vmState frame target
    IPop -> pure $ executePop vmState frame
    INop -> pure $ executeNop vmState frame
    _ -> pure $ Left $ InvalidInstruction "Instruction not implemented"
    _ -> pure $ Left $ InvalidInstruction "Instruction not implemented"

executeConst :: VMState -> Frame -> Int -> Either VMError (VMState, Maybe Value)
executeConst vmState frame idx =
  let consts = coConsts (fCode frame)
  in if idx >= Vector.length consts
       then Left $ RuntimeError "Constant index out of bounds"
       else
         let constValue = consts Vector.! idx
             val = resolveConst vmState constValue
             frame' = pushValue val (advancePC frame)
         in Right (updateFrame vmState frame', Nothing)

executeLoad :: VMState -> Frame -> Int -> Either VMError (VMState, Maybe Value)
executeLoad vmState frame slot =
  let locals = fLocals frame
  in if slot >= Vector.length locals
       then Left $ RuntimeError "Local slot out of bounds"
       else case locals Vector.! slot of
         Nothing -> Left $ RuntimeError "Uninitialized local variable"
         Just val ->
           let frame' = pushValue val (advancePC frame)
           in Right (updateFrame vmState frame', Nothing)

executeStore :: VMState -> Frame -> Int -> Either VMError (VMState, Maybe Value)
executeStore vmState frame slot =
  case fStack frame of
    [] -> Left StackUnderflow
    (val:stack') ->
      let locals = fLocals frame
          locals' = locals Vector.// [(slot, Just val)]
          frame' = advancePC frame { fStack = stack', fLocals = locals' }
      in Right (updateFrame vmState frame', Nothing)

executeReturn :: VMState -> Frame -> Either VMError (VMState, Maybe Value)
executeReturn vmState frame =
  case fStack frame of
    [] -> Left StackUnderflow
    (val:_) -> resumeCaller vmState val

executeJump :: VMState -> Frame -> Int -> Either VMError (VMState, Maybe Value)
executeJump vmState frame target =
  let frame' = frame { fPC = target }
  in Right (updateFrame vmState frame', Nothing)

executeConditionalJump :: VMState -> Frame -> Int -> Either VMError (VMState, Maybe Value)
executeConditionalJump vmState frame target =
  case fStack frame of
    [] -> Left StackUnderflow
    (VBool False : stack') ->
      let frame' = frame { fStack = stack', fPC = target }
      in Right (updateFrame vmState frame', Nothing)
    (_:stack') ->
      let frame' = advancePC frame { fStack = stack' }
      in Right (updateFrame vmState frame', Nothing)

executePop :: VMState -> Frame -> Either VMError (VMState, Maybe Value)
executePop vmState frame =
  case fStack frame of
    [] -> Left StackUnderflow
    (_:stack') ->
      let frame' = advancePC frame { fStack = stack' }
      in Right (updateFrame vmState frame', Nothing)

executeNop :: VMState -> Frame -> Either VMError (VMState, Maybe Value)
executeNop vmState frame =
  Right (updateFrame vmState (advancePC frame), Nothing)

executePrim :: VMState -> Frame -> String -> IO (Either VMError (VMState, Maybe Value))
executePrim vmState frame op =
  case Map.lookup op (vBuiltins vmState) of
    Nothing -> pure $ Left $ UndefinedFunction op
    Just (VBuiltin _ func) ->
      applyBuiltinResult vmState frame (builtinArity op) func
    Just _ -> pure $ Left $ TypeError "Not a builtin function"

executeCall :: VMState -> Frame -> Int -> Name -> Bool -> IO (Either VMError (VMState, Maybe Value))
executeCall vmState frame arity funcName isTail
  | length args < arity = pure $ Left StackUnderflow
  | otherwise =
      dispatchFunctionCall vmState frame funcName isTail
        reversedArgs
        remainingStack
  where
    (args, remainingStack) = splitAt arity (fStack frame)
    reversedArgs = reverse args

builtinArity :: String -> Int
builtinArity op =
  Map.findWithDefault 2 op builtinArityTable

builtinArityTable :: Map.Map String Int
builtinArityTable = Map.fromList (defaultArityPairs ++ specialArityPairs)

defaultArityPairs :: [(String, Int)]
defaultArityPairs = map (\op -> (op, 2)) twoArgBuiltinNames

twoArgBuiltinNames :: [String]
twoArgBuiltinNames =
  arithmeticArityNames
    ++ logicArityNames

arithmeticArityNames :: [String]
arithmeticArityNames = ["+", "-", "*", "div", "mod", "eq?", "<", ">"]

logicArityNames :: [String]
logicArityNames = ["and", "or"]

specialArityPairs :: [(String, Int)]
specialArityPairs =
  map (
    op -> (op, 1)) oneArgBuiltinNames
    ++ [ ("input", 0)
       , ("read-line", 0)
       , ("string-append", 2)
       , ("substring", 3)
       ]

oneArgBuiltinNames :: [String]
oneArgBuiltinNames =
  [ "print"
  , "display"
  , "string->number"
  , "number->string"
  , "string-length"
  , "not"
  ]

resolveConst :: VMState -> Constant -> Value
resolveConst vmState constValue =
  case constValue of
    CFuncRef name ->
      case Map.lookup name (vBuiltins vmState) of
        Just v -> v
        Nothing -> constantToValue constValue
    _ -> constantToValue constValue

advancePC :: Frame -> Frame
advancePC frame = frame { fPC = fPC frame + 1 }

pushValue :: Value -> Frame -> Frame
pushValue val frame = frame { fStack = val : fStack frame }

resumeCaller :: VMState -> Value -> Either VMError (VMState, Maybe Value)
resumeCaller vmState val =
  case vFrames vmState of
    [] -> Left $ RuntimeError "No frames for return"
    [_] -> Right (vmState, Just val)
    (_:callerFrame:rest) ->
      Right (resumeWithCaller vmState callerFrame rest val, Nothing)

resumeWithCaller :: VMState -> Frame -> [Frame] -> Value -> VMState
resumeWithCaller vmState callerFrame rest val =
  let callerFrame' =
        callerFrame
          { fStack = val : fStack callerFrame
          , fPC = fPC callerFrame + 1
          }
  in vmState { vFrames = callerFrame' : rest }

buildCallFrame :: CodeObject -> [Value] -> Frame
buildCallFrame code revArgs =
  let emptyLocals = Vector.replicate (coMaxLocals code) Nothing
      filledLocals = emptyLocals Vector.// zip [0..] (map Just revArgs)
  in Frame
       { fLocals = filledLocals
       , fStack = []
       , fCode = code
       , fPC = 0
       }

pushResultOntoFrame :: Frame -> [Value] -> Value -> Frame
pushResultOntoFrame frame stack' result =
  advancePC frame { fStack = result : stack' }

dispatchFunctionCall
  :: VMState
  -> Frame
  -> Name
  -> Bool
  -> [Value]
  -> [Value]
  -> IO (Either VMError (VMState, Maybe Value))
dispatchFunctionCall vmState frame funcName isTail revArgs stack' =
  case Map.lookup funcName (vBuiltins vmState) of
    Just (VBuiltin _ func) -> runBuiltinCall vmState frame revArgs stack' func
    Just _ -> pure $ Left $ TypeError "Not a builtin function"
    Nothing ->
      let userResult =
            callUserFunction vmState frame funcName isTail revArgs stack'
      in pure userResult

runBuiltinCall
  :: VMState
  -> Frame
  -> [Value]
  -> [Value]
  -> ([Value] -> IO Value)
  -> IO (Either VMError (VMState, Maybe Value))
runBuiltinCall vmState frame revArgs stack' func =
  case vFrames vmState of
    (_:rest) ->
      func revArgs >>= 
        \result ->
          pure $ Right (updateAfterBuiltin vmState frame rest result stack')
    [] -> pure $ Left $ RuntimeError "No frames for builtin call"

updateAfterBuiltin
  :: VMState
  -> Frame
  -> [Frame]
  -> Value
  -> [Value]
  -> (VMState, Maybe Value)
updateAfterBuiltin vmState frame rest result stack' =
  let updatedFrame = pushResultOntoFrame frame stack' result
  in (vmState { vFrames = updatedFrame : rest }, Nothing)

callUserFunction
  :: VMState
  -> Frame
  -> Name
  -> Bool
  -> [Value]
  -> [Value]
  -> Either VMError (VMState, Maybe Value)
callUserFunction vmState frame funcName isTail revArgs stack' =
  case Map.lookup funcName (vCodeObjects vmState) of
    Just code -> prepareUserCall vmState frame code isTail revArgs stack'
    Nothing -> Left $ UndefinedFunction funcName

prepareUserCall
  :: VMState
  -> Frame
  -> CodeObject
  -> Bool
  -> [Value]
  -> [Value]
  -> Either VMError (VMState, Maybe Value)
prepareUserCall vmState frame code isTail revArgs stack' =
  case vFrames vmState of
    (_:rest) ->
      let frames' = buildFrameStack frame code isTail revArgs stack' rest
      in Right (vmState { vFrames = frames' }, Nothing)
    [] -> Left $ RuntimeError "No frames for function call"

buildFrameStack
  :: Frame
  -> CodeObject
  -> Bool
  -> [Value]
  -> [Value]
  -> [Frame]
  -> [Frame]
buildFrameStack frame code isTail revArgs stack' rest =
  let newFrame = buildCallFrame code revArgs
      callerFrame = frame { fStack = stack' }
  in if isTail
       then newFrame : rest
       else newFrame : callerFrame : rest

applyBuiltinResult
  :: VMState
  -> Frame
  -> Int
  -> ([Value] -> IO Value)
  -> IO (Either VMError (VMState, Maybe Value))
applyBuiltinResult vmState frame arity func =
  let (args, stack') = splitAt arity (fStack frame)
  in if length args < arity
       then pure $ Left StackUnderflow
       else func (reverse args) >>= \result ->
              let frame' = advancePC frame { fStack = result : stack' }
              in pure $ Right (updateFrame vmState frame', Nothing)

constantToValue :: Constant -> Value
constantToValue (CInt n) = VInt n
constantToValue (CBool b) = VBool b
constantToValue (CString s) = VString s
constantToValue (CFuncRef name) = VBuiltin name undefined

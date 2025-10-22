{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- VM
-}

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
runVM vmState = case vFrames vmState of
  [] -> return $ Left $ RuntimeError "No frames to execute"
  (frame:_) ->
    let pc = fPC frame
        code = fCode frame
        instrs = coInstrs code
    in if pc >= Vector.length instrs
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
  IConst idx ->
    let consts = coConsts (fCode frame)
    in if idx >= Vector.length consts
      then return $ Left $ RuntimeError "Constant index out of bounds"
      else
        let constValue = consts Vector.! idx
            val = case constValue of
              CFuncRef name -> maybe (constantToValue constValue) id (Map.lookup name (vBuiltins vmState))
              _ -> constantToValue constValue
            frame' = frame { fStack = val : fStack frame, fPC = fPC frame + 1 }
        in return $ Right (updateFrame vmState frame', Nothing)

  ILoad slot ->
    let locals = fLocals frame
    in if slot >= Vector.length locals
      then return $ Left $ RuntimeError "Local slot out of bounds"
      else case locals Vector.! slot of
        Nothing -> return $ Left $ RuntimeError "Uninitialized local variable"
        Just val ->
          let frame' = frame { fStack = val : fStack frame, fPC = fPC frame + 1 }
          in return $ Right (updateFrame vmState frame', Nothing)

  IStore slot -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (val:stack') ->
      let locals = fLocals frame
          locals' = locals Vector.// [(slot, Just val)]
          frame' = frame { fStack = stack', fLocals = locals', fPC = fPC frame + 1 }
      in return $ Right (updateFrame vmState frame', Nothing)

  IPrim op -> executePrim vmState frame op

  ICall arity funcName -> executeCall vmState frame arity funcName False

  ITailCall arity funcName -> executeCall vmState frame arity funcName True

  IReturn -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (val:_) -> case vFrames vmState of
      [] -> return $ Left $ RuntimeError "No frames for return"
      [_] -> return $ Right (vmState, Just val)
      (_:callerFrame:rest) ->
        let callerFrame' = callerFrame { fStack = val : fStack callerFrame, fPC = fPC callerFrame + 1 }
        in return $ Right (vmState { vFrames = callerFrame' : rest }, Nothing)

  IJump target ->
    let frame' = frame { fPC = target }
    in return $ Right (updateFrame vmState frame', Nothing)

  IJumpIfFalse target -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (VBool False : stack') ->
      let frame' = frame { fStack = stack', fPC = target }
      in return $ Right (updateFrame vmState frame', Nothing)
    (VVoid : stack') ->
      let frame' = frame { fStack = stack', fPC = target }
      in return $ Right (updateFrame vmState frame', Nothing)
    (_:stack') ->
      let frame' = frame { fStack = stack', fPC = fPC frame + 1 }
      in return $ Right (updateFrame vmState frame', Nothing)

  IPop -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (_:stack') ->
      let frame' = frame { fStack = stack', fPC = fPC frame + 1 }
      in return $ Right (updateFrame vmState frame', Nothing)

  INop ->
    let frame' = frame { fPC = fPC frame + 1 }
    in return $ Right (updateFrame vmState frame', Nothing)

  -- Topineur object instructions
  IMakeObject objName numFields ->
    let (fieldValues, stack') = splitAt numFields (fStack frame)
    in if length fieldValues < numFields
      then return $ Left StackUnderflow
      else
        -- Assume fields are pushed in order: field1, field2, ..., fieldN
        -- We need to extract field names from the code object's constants
        let fields = zip (map (\i -> "field" ++ show i) [0..numFields-1]) (reverse fieldValues)
            obj = VObject objName (Map.fromList fields) Map.empty
            frame' = frame { fStack = obj : stack', fPC = fPC frame + 1 }
        in return $ Right (updateFrame vmState frame', Nothing)

  IGetField fieldName -> case fStack frame of
    [] -> return $ Left StackUnderflow
    (VObject _ fields _ : stack') ->
      case Map.lookup fieldName fields of
        Nothing -> return $ Left $ RuntimeError $ "Field '" ++ fieldName ++ "' not found in object"
        Just val ->
          let frame' = frame { fStack = val : stack', fPC = fPC frame + 1 }
          in return $ Right (updateFrame vmState frame', Nothing)
    (_:_) -> return $ Left $ TypeError "Expected object for field access"

  ISetField fieldName -> case fStack frame of
    (val : VObject objName fields vtable : stack') ->
      let fields' = Map.insert fieldName val fields
          obj' = VObject objName fields' vtable
          frame' = frame { fStack = obj' : stack', fPC = fPC frame + 1 }
      in return $ Right (updateFrame vmState frame', Nothing)
    _ -> return $ Left $ TypeError "Expected value and object for field assignment"

  IMethodCall methodName arity -> case fStack frame of
    stack | length stack < arity + 1 -> return $ Left StackUnderflow
    stack ->
      let (args, obj:stackRest) = splitAt arity stack
      in case obj of
        VObject objName _ vtable ->
          case Map.lookup methodName vtable of
            Nothing -> return $ Left $ RuntimeError $ "Method '" ++ methodName ++ "' not found in object '" ++ objName ++ "'"
            Just (VClosure closureName env) ->
              -- Call the closure with the object as first arg (self) followed by other args
              case Map.lookup closureName (vCodeObjects vmState) of
                Nothing -> return $ Left $ UndefinedFunction closureName
                Just code ->
                  let baseLocals = Vector.replicate (coMaxLocals code) Nothing
                      allArgs = obj : reverse args
                      argValues = map Just allArgs
                      localsWithArgs = baseLocals Vector.// zip [0..] argValues
                      envValues = map Just env
                      envStart = length argValues
                      localsFinal = localsWithArgs Vector.// zip [envStart..] envValues
                      newFrame = Frame
                        { fLocals = localsFinal
                        , fStack = []
                        , fCode = code
                        , fPC = 0
                        }
                      frame' = frame { fStack = stackRest, fPC = fPC frame + 1 }
                  in case vFrames vmState of
                    (_:rest) ->
                      return $ Right (vmState { vFrames = newFrame : frame' : rest }, Nothing)
                    [] -> return $ Left $ RuntimeError "No frames for method call"
            Just _ -> return $ Left $ TypeError "Method is not a closure"
        _ -> return $ Left $ TypeError "Expected object for method call"

  ILoadDict traitName ->
    case Map.lookup traitName (vGlobals vmState) of
      Just dict@(VTraitDict _ _) ->
        let frame' = frame { fStack = dict : fStack frame, fPC = fPC frame + 1 }
        in return $ Right (updateFrame vmState frame', Nothing)
      Just _ -> return $ Left $ TypeError $ "'" ++ traitName ++ "' is not a trait dictionary"
      Nothing -> return $ Left $ RuntimeError $ "Trait dictionary '" ++ traitName ++ "' not found"

  _ -> return $ Left $ InvalidInstruction "Instruction not implemented"

builtinArity :: String -> Int
builtinArity op = case op of
  "+" -> 2
  "-" -> 2
  "*" -> 2
  "div" -> 2
  "mod" -> 2
  "eq?" -> 2
  "<" -> 2
  ">" -> 2
  "<=" -> 2
  ">=" -> 2
  "print" -> 1
  "println" -> 1
  "display" -> 1
  "show" -> 1
  "input" -> 0
  "read-line" -> 0
  "string->number" -> 1
  "number->string" -> 1
  "string-length" -> 1
  "string-append" -> 2
  "substring" -> 3
  "not" -> 1
  "and" -> 2
  "or" -> 2
  "format" -> -1
  _ -> 2

executePrim :: VMState -> Frame -> String -> IO (Either VMError (VMState, Maybe Value))
executePrim vmState frame op =
  case Map.lookup op (vBuiltins vmState) of
    Nothing -> return $ Left $ UndefinedFunction op
    Just (VBuiltin _ func) ->
      let arity = builtinArity op
          (args, stack') = splitAt arity (fStack frame)
      in if length args < arity
        then return $ Left StackUnderflow
        else do
          result <- func (reverse args)
          let frame' = frame { fStack = result : stack', fPC = fPC frame + 1 }
          return $ Right (updateFrame vmState frame', Nothing)
    Just _ -> return $ Left $ TypeError "Not a builtin function"
executeCall :: VMState -> Frame -> Int -> Name -> Bool -> IO (Either VMError (VMState, Maybe Value))
executeCall vmState frame arity funcName isTail =
  let (args, stackAfterArgs) = splitAt arity (fStack frame)
  in if length args < arity
    then return $ Left StackUnderflow
    else case funcName of
      "<lambda>" ->
        callClosure vmState frame args stackAfterArgs isTail
      _ ->
        case Map.lookup funcName (vBuiltins vmState) of
          Just (VBuiltin _ func) -> do
            result <- func (reverse args)
            let frame' = frame { fStack = result : stackAfterArgs, fPC = fPC frame + 1 }
            case vFrames vmState of
              (_:rest) -> return $ Right (vmState { vFrames = frame' : rest }, Nothing)
              [] -> return $ Left $ RuntimeError "No frames for builtin call"
          Just _ -> return $ Left $ TypeError "Not a builtin function"
          Nothing ->
            case Map.lookup funcName (vCodeObjects vmState) of
              Just code ->
                let newLocals = Vector.replicate (coMaxLocals code) Nothing
                    argsVector = Vector.fromList (map Just (reverse args))
                    newLocals' = newLocals Vector.// zip [0..] (Vector.toList argsVector)
                    newFrame = Frame
                      { fLocals = newLocals'
                      , fStack = []
                      , fCode = code
                      , fPC = 0
                      }
                    frame' = frame { fStack = stackAfterArgs }
                in case vFrames vmState of
                  (_:rest) ->
                    let vmState' = if isTail
                          then vmState { vFrames = newFrame : rest }
                          else vmState { vFrames = newFrame : frame' : rest }
                    in return $ Right (vmState', Nothing)
                  [] -> return $ Left $ RuntimeError "No frames for function call"
              Nothing -> return $ Left $ UndefinedFunction funcName

callClosure :: VMState -> Frame -> [Value] -> [Value] -> Bool -> IO (Either VMError (VMState, Maybe Value))
callClosure vmState frame args stackAfterArgs isTail =
  case stackAfterArgs of
    [] -> return $ Left StackUnderflow
    (closureVal:stackRest) ->
      case closureVal of
        VClosure name env ->
          case Map.lookup name (vCodeObjects vmState) of
            Nothing -> return $ Left $ UndefinedFunction name
            Just code ->
              let baseLocals = Vector.replicate (coMaxLocals code) Nothing
                  argValues = map Just (reverse args)
                  localsWithArgs = baseLocals Vector.// zip [0..] argValues
                  envValues = map Just env
                  envStart = length argValues
                  localsFinal = localsWithArgs Vector.// zip [envStart..] envValues
                  newFrame = Frame
                    { fLocals = localsFinal
                    , fStack = []
                    , fCode = code
                    , fPC = 0
                    }
                  frame' = frame { fStack = stackRest }
              in case vFrames vmState of
                (_:rest) ->
                  let vmState' = if isTail
                        then vmState { vFrames = newFrame : rest }
                        else vmState { vFrames = newFrame : frame' : rest }
                  in return $ Right (vmState', Nothing)
                [] -> return $ Left $ RuntimeError "No frames for function call"
        _ -> return $ Left $ TypeError "Attempted to call non-closure value"
constantToValue :: Constant -> Value
constantToValue (CInt n) = VInt n
constantToValue (CFloat n) = VFloat n
constantToValue (CBool b) = VBool b
constantToValue (CString s) = VString s
constantToValue (CFuncRef name) = VClosure name []

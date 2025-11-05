{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Builtins
-}

module Builtins
  ( builtins
  , builtinAdd
  , builtinSub
  , builtinMul
  , builtinDiv
  , builtinMod
  , builtinEq
  , builtinLt
  , builtinLte
  , builtinGt
  , builtinGte
  , builtinPrint
  , builtinPrintln
  , builtinDisplay
  , builtinInput
  , builtinReadLine
  , builtinStringToNumber
  , builtinNumberToString
  , builtinStringLength
  , builtinStringAppend
  , builtinSubstring
  , builtinNot
  , builtinAnd
  , builtinOr
  , builtinFormat
  , builtinShow
  , builtinListLength
  , builtinListAppend
  , builtinListSingle
  , builtinListGet
  , builtinAbs
  , builtinSqrt
  , builtinInt
  , builtinFloat
  , builtinPow
  , builtinFloor
  , builtinCeil
  , builtinRound
  , builtinSin
  , builtinCos
  , builtinTan
  , builtinAsin
  , builtinAcos
  , builtinAtan
  , builtinAtan2
  ) where

import AST
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

builtins :: Map.Map Name Value
builtins = Map.fromList
  [ ("t", VBool True)
  , ("nil", VBool False)
  , ("+", VBuiltin "+" builtinAdd)
  , ("-", VBuiltin "-" builtinSub)
  , ("*", VBuiltin "*" builtinMul)
  , ("div", VBuiltin "div" builtinDiv)
  , ("mod", VBuiltin "mod" builtinMod)
  , ("eq?", VBuiltin "eq?" builtinEq)
  , ("<", VBuiltin "<" builtinLt)
  , ("<=", VBuiltin "<=" builtinLte)
  , (">", VBuiltin ">" builtinGt)
  , (">=", VBuiltin ">=" builtinGte)
  , ("print", VBuiltin "print" builtinPrint)
  , ("println", VBuiltin "println" builtinPrintln)
  , ("display", VBuiltin "display" builtinDisplay)
  , ("input", VBuiltin "input" builtinInput)
  , ("read-line", VBuiltin "read-line" builtinReadLine)
  , ("string->number", VBuiltin "string->number" builtinStringToNumber)
  , ("number->string", VBuiltin "number->string" builtinNumberToString)
  , ("string-length", VBuiltin "string-length" builtinStringLength)
  , ("__string_length", VBuiltin "__string_length" builtinStringLength)
  , ("string-append", VBuiltin "string-append" builtinStringAppend)
  , ("__string_append", VBuiltin "__string_append" builtinStringAppend)
  , ("substring", VBuiltin "substring" builtinSubstring)
  , ("__substring", VBuiltin "__substring" builtinSubstring)
  , ("not", VBuiltin "not" builtinNot)
  , ("and", VBuiltin "and" builtinAnd)
  , ("or", VBuiltin "or" builtinOr)
  , ("format", VBuiltin "format" builtinFormat)
  , ("show", VBuiltin "show" builtinShow)
  , ("list-length", VBuiltin "list-length" builtinListLength)
  , ("__list_length", VBuiltin "__list_length" builtinListLength)
  , ("list-append", VBuiltin "list-append" builtinListAppend)
  , ("__list_append", VBuiltin "__list_append" builtinListAppend)
  , ("list-single", VBuiltin "list-single" builtinListSingle)
  , ("__list_single", VBuiltin "__list_single" builtinListSingle)
  , ("list-get", VBuiltin "list-get" builtinListGet)
  , ("__list_get", VBuiltin "__list_get" builtinListGet)
  , ("abs", VBuiltin "abs" builtinAbs)
  , ("sqrt", VBuiltin "sqrt" builtinSqrt)
  , ("int", VBuiltin "int" builtinInt)
  , ("float", VBuiltin "float" builtinFloat)
  , ("pow", VBuiltin "pow" builtinPow)
  , ("floor", VBuiltin "floor" builtinFloor)
  , ("ceil", VBuiltin "ceil" builtinCeil)
  , ("round", VBuiltin "round" builtinRound)
  , ("sin", VBuiltin "sin" builtinSin)
  , ("cos", VBuiltin "cos" builtinCos)
  , ("tan", VBuiltin "tan" builtinTan)
  , ("asin", VBuiltin "asin" builtinAsin)
  , ("acos", VBuiltin "acos" builtinAcos)
  , ("atan", VBuiltin "atan" builtinAtan)
  , ("atan2", VBuiltin "atan2" builtinAtan2)
  , ("pi", VFloat pi)
  , ("e", VFloat (exp 1))
  ]

builtinAdd :: [Value] -> IO Value
builtinAdd [VInt a, VInt b] = return $ VInt (a + b)
builtinAdd [VFloat a, VFloat b] = return $ VFloat (a + b)
builtinAdd [VInt a, VFloat b] = return $ VFloat (fromInteger a + b)
builtinAdd [VFloat a, VInt b] = return $ VFloat (a + fromInteger b)
builtinAdd _ = error "Type error: + expects two numbers"

builtinSub :: [Value] -> IO Value
builtinSub [VInt a, VInt b] = return $ VInt (a - b)
builtinSub [VFloat a, VFloat b] = return $ VFloat (a - b)
builtinSub [VInt a, VFloat b] = return $ VFloat (fromInteger a - b)
builtinSub [VFloat a, VInt b] = return $ VFloat (a - fromInteger b)
builtinSub _ = error "Type error: - expects two numbers"

builtinMul :: [Value] -> IO Value
builtinMul [VInt a, VInt b] = return $ VInt (a * b)
builtinMul [VFloat a, VFloat b] = return $ VFloat (a * b)
builtinMul [VInt a, VFloat b] = return $ VFloat (fromInteger a * b)
builtinMul [VFloat a, VInt b] = return $ VFloat (a * fromInteger b)
builtinMul _ = error "Type error: * expects two numbers"

builtinDiv :: [Value] -> IO Value
builtinDiv [VInt a, VInt b]
  | b == 0 = error "Division by zero"
  | otherwise = return $ VInt (a `div` b)
builtinDiv _ = error "Type error: div expects two integers"

builtinMod :: [Value] -> IO Value
builtinMod [VInt a, VInt b]
  | b == 0 = error "Division by zero"
  | otherwise = return $ VInt (a `mod` b)
builtinMod _ = error "Type error: mod expects two integers"

builtinEq :: [Value] -> IO Value
builtinEq [VInt a, VInt b] = return $ VBool (a == b)
builtinEq [VFloat a, VFloat b] = return $ VBool (a == b)
builtinEq [VInt a, VFloat b] = return $ VBool (fromInteger a == b)
builtinEq [VFloat a, VInt b] = return $ VBool (a == fromInteger b)
builtinEq [VBool a, VBool b] = return $ VBool (a == b)
builtinEq [VString a, VString b] = return $ VBool (a == b)
builtinEq _ = return $ VBool False

builtinLt :: [Value] -> IO Value
builtinLt [VInt a, VInt b] = return $ VBool (a < b)
builtinLt [VFloat a, VFloat b] = return $ VBool (a < b)
builtinLt [VInt a, VFloat b] = return $ VBool (fromInteger a < b)
builtinLt [VFloat a, VInt b] = return $ VBool (a < fromInteger b)
builtinLt _ = error "Type error: < expects two numbers"

builtinGt :: [Value] -> IO Value
builtinGt [VInt a, VInt b] = return $ VBool (a > b)
builtinGt [VFloat a, VFloat b] = return $ VBool (a > b)
builtinGt [VInt a, VFloat b] = return $ VBool (fromInteger a > b)
builtinGt [VFloat a, VInt b] = return $ VBool (a > fromInteger b)
builtinGt _ = error "Type error: > expects two numbers"

builtinLte :: [Value] -> IO Value
builtinLte [VInt a, VInt b] = return $ VBool (a <= b)
builtinLte [VFloat a, VFloat b] = return $ VBool (a <= b)
builtinLte [VInt a, VFloat b] = return $ VBool (fromInteger a <= b)
builtinLte [VFloat a, VInt b] = return $ VBool (a <= fromInteger b)
builtinLte _ = error "Type error: <= expects two numbers"

builtinGte :: [Value] -> IO Value
builtinGte [VInt a, VInt b] = return $ VBool (a >= b)
builtinGte [VFloat a, VFloat b] = return $ VBool (a >= b)
builtinGte [VInt a, VFloat b] = return $ VBool (fromInteger a >= b)
builtinGte [VFloat a, VInt b] = return $ VBool (a >= fromInteger b)
builtinGte _ = error "Type error: >= expects two numbers"

builtinPrint :: [Value] -> IO Value
builtinPrint [val] =
  putStrLn (showValue val) >> return val
builtinPrint _ = error "Type error: print expects one argument"

builtinPrintln :: [Value] -> IO Value
builtinPrintln [val] = do
  putStrLn (showValue val)
  return VUnit
builtinPrintln _ = error "Type error: println expects one argument"

builtinDisplay :: [Value] -> IO Value
builtinDisplay [val] =
  putStr (showValue val) >> hFlush stdout >> return val
builtinDisplay _ = error "Type error: display expects one argument"

builtinInput :: [Value] -> IO Value
builtinInput [VString prompt] = do
  putStr prompt
  hFlush stdout
  line <- getLine
  return $ VString line
builtinInput [] = do
  line <- getLine
  return $ VString line
builtinInput _ = error "Type error: input expects zero or one argument (prompt)"

builtinReadLine :: [Value] -> IO Value
builtinReadLine [] = do
  line <- getLine
  return $ VString line
builtinReadLine _ = error "Type error: read-line expects no arguments"

builtinStringToNumber :: [Value] -> IO Value
builtinStringToNumber [VString s] =
  case readMaybe s of
    Just n -> return $ VInt n
    Nothing -> case (readMaybe s :: Maybe Double) of
      Just f -> return $ VFloat f
      Nothing -> error $ "Type error: cannot convert '" ++ s ++ "' to number"
builtinStringToNumber _ = error "Type error: string->number expects a string"

builtinNumberToString :: [Value] -> IO Value
builtinNumberToString [VInt n] = return $ VString (show n)
builtinNumberToString [VFloat n] = return $ VString (show n)
builtinNumberToString _ = error "Type error: number->string expects a number"

builtinStringLength :: [Value] -> IO Value
builtinStringLength [VString s] = return $ VInt (toInteger $ length s)
builtinStringLength _ = error "Type error: string-length expects a string"

builtinStringAppend :: [Value] -> IO Value
builtinStringAppend [VString a, VString b] = return $ VString (a ++ b)
builtinStringAppend _ = error "Type error: string-append expects two strings"

builtinSubstring :: [Value] -> IO Value
builtinSubstring [VString s, VInt start, VInt end] =
  let s' = take (fromInteger $ end - start) $ drop (fromInteger start) s
  in return $ VString s'
builtinSubstring _ = error "Type error: substring expects (string, start, end)"

builtinNot :: [Value] -> IO Value
builtinNot [VBool b] = return $ VBool (not b)
builtinNot _ = error "Type error: not expects a boolean"

builtinAnd :: [Value] -> IO Value
builtinAnd [VBool a, VBool b] = return $ VBool (a && b)
builtinAnd _ = error "Type error: and expects two booleans"

builtinOr :: [Value] -> IO Value
builtinOr [VBool a, VBool b] = return $ VBool (a || b)
builtinOr _ = error "Type error: or expects two booleans"

builtinFormat :: [Value] -> IO Value
builtinFormat (dest:VString fmt:args) =
  let formatted = processFormatString fmt args
  in case dest of
       VBool True -> putStr formatted >> hFlush stdout >> return VUnit
       VBool False -> return (VString formatted)
       _ -> error "Type error: format destination must be t or nil"
builtinFormat _ = error "Type error: format expects (destination format-string ...)"

builtinShow :: [Value] -> IO Value
builtinShow [val] = return $ VString $ topineurShow val
builtinShow _ = error "Type error: show expects one argument"

processFormatString :: String -> [Value] -> String
processFormatString [] _ = []
processFormatString ('~':'%':rest) args = '\n' : processFormatString rest args
processFormatString ('~':'a':rest) (arg:args') =
  showValue arg ++ processFormatString rest args'
processFormatString ('~':'s':rest) (arg:args') =
  showValue arg ++ processFormatString rest args'
processFormatString ('~':'A':rest) (arg:args') =
  showValue arg ++ processFormatString rest args'
processFormatString ('~':'S':rest) (arg:args') =
  showValue arg ++ processFormatString rest args'
processFormatString (c:rest) args = c : processFormatString rest args

showValue :: Value -> String
showValue (VInt n) = show n
showValue (VFloat n) = show n
showValue (VBool True) = "#t"
showValue (VBool False) = "#f"
showValue (VString s) = s
showValue (VBuiltin name _) = "<builtin:" ++ name ++ ">"
showValue (VClosure name _) = "<closure:" ++ name ++ ">"
showValue VUnit = "#<void>"
showValue (VList values) = "(" ++ unwords (map showValue values) ++ ")"
showValue (VTuple values) = "#(" ++ unwords (map showValue values) ++ ")"
showValue (VObject name fields) = "#<object:" ++ name ++ " " ++ showFields fields ++ ">"
  where
    showFields [] = ""
    showFields [(k, v)] = k ++ ":" ++ showValue v
    showFields ((k, v):rest) = k ++ ":" ++ showValue v ++ " " ++ showFields rest

topineurShow :: Value -> String
topineurShow (VInt n) = show n
topineurShow (VFloat n) = show n
topineurShow (VBool True) = "true"
topineurShow (VBool False) = "false"
topineurShow (VString s) = s
topineurShow (VBuiltin name _) = "<builtin:" ++ name ++ ">"
topineurShow (VClosure name _) = "<closure:" ++ name ++ ">"
topineurShow VUnit = "#<void>"
topineurShow (VList values) = "(" ++ unwords (map topineurShow values) ++ ")"
topineurShow (VTuple values) = "#(" ++ unwords (map topineurShow values) ++ ")"
topineurShow (VObject name fields) = "#<object:" ++ name ++ " " ++ showFields fields ++ ">"
  where
    showFields [] = ""
    showFields [(k, v)] = k ++ ":" ++ topineurShow v
    showFields ((k, v):rest) = k ++ ":" ++ topineurShow v ++ " " ++ showFields rest

builtinListLength :: [Value] -> IO Value
builtinListLength [VList values] = return $ VInt (toInteger $ length values)
builtinListLength _ = error "Type error: list-length expects a list"

builtinListAppend :: [Value] -> IO Value
builtinListAppend [VList left, VList right] = return $ VList (left ++ right)
builtinListAppend _ = error "Type error: list-append expects two lists"

builtinListSingle :: [Value] -> IO Value
builtinListSingle [val] = return $ VList [val]
builtinListSingle _ = error "Type error: list-single expects one value"

builtinListGet :: [Value] -> IO Value
builtinListGet [VList elements, VInt idx] =
  let i = fromInteger idx
  in if i < 0 || i >= length elements
    then error "Index out of bounds"
    else return (elements !! i)
builtinListGet _ = error "Type error: list-get expects (list, index)"

-- Math builtins
builtinAbs :: [Value] -> IO Value
builtinAbs [VInt n] = return $ VInt (abs n)
builtinAbs [VFloat n] = return $ VFloat (abs n)
builtinAbs _ = error "Type error: abs expects a number"

builtinSqrt :: [Value] -> IO Value
builtinSqrt [VInt n] = return $ VFloat (sqrt (fromInteger n))
builtinSqrt [VFloat n] = return $ VFloat (sqrt n)
builtinSqrt _ = error "Type error: sqrt expects a number"

-- Type conversion builtins
builtinInt :: [Value] -> IO Value
builtinInt [VInt n] = return $ VInt n
builtinInt [VFloat n] = return $ VInt (truncate n)  -- Truncate towards zero
builtinInt [VString s] = case readMaybe s of
  Just n -> return $ VInt n
  Nothing -> error $ "Type error: cannot convert '" ++ s ++ "' to int"
builtinInt _ = error "Type error: int expects a number or string"

builtinFloat :: [Value] -> IO Value
builtinFloat [VFloat n] = return $ VFloat n
builtinFloat [VInt n] = return $ VFloat (fromInteger n)
builtinFloat [VString s] = case (readMaybe s :: Maybe Double) of
  Just n -> return $ VFloat n
  Nothing -> error $ "Type error: cannot convert '" ++ s ++ "' to float"
builtinFloat _ = error "Type error: float expects a number or string"

-- Advanced math builtins
builtinPow :: [Value] -> IO Value
builtinPow [VInt a, VInt b] = return $ VFloat ((fromInteger a) ** (fromInteger b))
builtinPow [VFloat a, VFloat b] = return $ VFloat (a ** b)
builtinPow [VInt a, VFloat b] = return $ VFloat ((fromInteger a) ** b)
builtinPow [VFloat a, VInt b] = return $ VFloat (a ** (fromInteger b))
builtinPow _ = error "Type error: pow expects two numbers"

builtinFloor :: [Value] -> IO Value
builtinFloor [VInt n] = return $ VInt n
builtinFloor [VFloat n] = return $ VInt (floor n)
builtinFloor _ = error "Type error: floor expects a number"

builtinCeil :: [Value] -> IO Value
builtinCeil [VInt n] = return $ VInt n
builtinCeil [VFloat n] = return $ VInt (ceiling n)
builtinCeil _ = error "Type error: ceil expects a number"

builtinRound :: [Value] -> IO Value
builtinRound [VInt n] = return $ VInt n
builtinRound [VFloat n] = return $ VInt (round n)
builtinRound _ = error "Type error: round expects a number"

builtinSin :: [Value] -> IO Value
builtinSin [VInt n] = return $ VFloat (sin (fromInteger n))
builtinSin [VFloat n] = return $ VFloat (sin n)
builtinSin _ = error "Type error: sin expects a number"

builtinCos :: [Value] -> IO Value
builtinCos [VInt n] = return $ VFloat (cos (fromInteger n))
builtinCos [VFloat n] = return $ VFloat (cos n)
builtinCos _ = error "Type error: cos expects a number"

builtinTan :: [Value] -> IO Value
builtinTan [VInt n] = return $ VFloat (tan (fromInteger n))
builtinTan [VFloat n] = return $ VFloat (tan n)
builtinTan _ = error "Type error: tan expects a number"

builtinAsin :: [Value] -> IO Value
builtinAsin [VInt n] = return $ VFloat (asin (fromInteger n))
builtinAsin [VFloat n] = return $ VFloat (asin n)
builtinAsin _ = error "Type error: asin expects a number"

builtinAcos :: [Value] -> IO Value
builtinAcos [VInt n] = return $ VFloat (acos (fromInteger n))
builtinAcos [VFloat n] = return $ VFloat (acos n)
builtinAcos _ = error "Type error: acos expects a number"

builtinAtan :: [Value] -> IO Value
builtinAtan [VInt n] = return $ VFloat (atan (fromInteger n))
builtinAtan [VFloat n] = return $ VFloat (atan n)
builtinAtan _ = error "Type error: atan expects a number"

builtinAtan2 :: [Value] -> IO Value
builtinAtan2 [VInt y, VInt x] = return $ VFloat (atan2 (fromInteger y) (fromInteger x))
builtinAtan2 [VFloat y, VFloat x] = return $ VFloat (atan2 y x)
builtinAtan2 [VInt y, VFloat x] = return $ VFloat (atan2 (fromInteger y) x)
builtinAtan2 [VFloat y, VInt x] = return $ VFloat (atan2 y (fromInteger x))
builtinAtan2 _ = error "Type error: atan2 expects two numbers"

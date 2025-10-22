{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Builtins
-}

module Builtins
  ( builtins
  , builtinAdd, builtinSub, builtinMul, builtinDiv, builtinMod
  , builtinEq, builtinLt, builtinGt, builtinLte, builtinGte
  , builtinPrint, builtinPrintln, builtinDisplay
  , builtinInput, builtinReadLine
  , builtinStringToNumber, builtinNumberToString, builtinShow
  , builtinStringLength, builtinStringAppend, builtinSubstring
  , builtinStringUpper, builtinStringLower
  , builtinNot, builtinAnd, builtinOr
  , builtinFormat
  , builtinAbs, builtinMin, builtinMax, builtinPow, builtinSqrt
  , builtinCons, builtinCar, builtinCdr, builtinList, builtinLength, builtinAppend
  ) where

import AST
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.Char (toUpper, toLower)

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
  , (">", VBuiltin ">" builtinGt)
  , ("<=", VBuiltin "<=" builtinLte)
  , (">=", VBuiltin ">=" builtinGte)
  , ("print", VBuiltin "print" builtinPrint)
  , ("println", VBuiltin "println" builtinPrintln)
  , ("display", VBuiltin "display" builtinDisplay)
  , ("input", VBuiltin "input" builtinInput)
  , ("read-line", VBuiltin "read-line" builtinReadLine)
  , ("string->number", VBuiltin "string->number" builtinStringToNumber)
  , ("number->string", VBuiltin "number->string" builtinNumberToString)
  , ("show", VBuiltin "show" builtinShow)
  , ("string-length", VBuiltin "string-length" builtinStringLength)
  , ("string-append", VBuiltin "string-append" builtinStringAppend)
  , ("substring", VBuiltin "substring" builtinSubstring)
  , ("not", VBuiltin "not" builtinNot)
  , ("and", VBuiltin "and" builtinAnd)
  , ("or", VBuiltin "or" builtinOr)
  , ("format", VBuiltin "format" builtinFormat)
  -- Math functions
  , ("abs", VBuiltin "abs" builtinAbs)
  , ("min", VBuiltin "min" builtinMin)
  , ("max", VBuiltin "max" builtinMax)
  , ("pow", VBuiltin "pow" builtinPow)
  , ("sqrt", VBuiltin "sqrt" builtinSqrt)
  -- List functions
  , ("cons", VBuiltin "cons" builtinCons)
  , ("car", VBuiltin "car" builtinCar)
  , ("cdr", VBuiltin "cdr" builtinCdr)
  , ("list", VBuiltin "list" builtinList)
  , ("length", VBuiltin "length" builtinLength)
  , ("append", VBuiltin "append" builtinAppend)
  -- String functions (additional)
  , ("str-concat", VBuiltin "str-concat" builtinStringAppend)
  , ("str-upper", VBuiltin "str-upper" builtinStringUpper)
  , ("str-lower", VBuiltin "str-lower" builtinStringLower)
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
builtinPrintln [val] =
  putStrLn (showValue val) >> return VVoid
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

builtinShow :: [Value] -> IO Value
builtinShow [val] = return $ VString (showValue val)
builtinShow _ = error "Type error: show expects one argument"

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
       VBool True -> putStr formatted >> hFlush stdout >> return VVoid
       VBool False -> return (VString formatted)
       _ -> error "Type error: format destination must be t or nil"
builtinFormat _ = error "Type error: format expects (destination format-string ...)"

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
showValue VVoid = "#<void>"
showValue (VObject name fields _) = "<object:" ++ name ++ ":" ++ show (Map.size fields) ++ " fields>"
showValue (VTraitDict name _) = "<trait:" ++ name ++ ">"
showValue (VList vs) = "[" ++ unwords (map showValue vs) ++ "]"
showValue (VTuple vs) = "(" ++ unwords (map showValue vs) ++ ")"

-- Math builtins
builtinAbs :: [Value] -> IO Value
builtinAbs [VInt n] = return $ VInt (abs n)
builtinAbs [VFloat n] = return $ VFloat (abs n)
builtinAbs _ = error "Type error: abs expects a number"

builtinMin :: [Value] -> IO Value
builtinMin [VInt a, VInt b] = return $ VInt (min a b)
builtinMin [VFloat a, VFloat b] = return $ VFloat (min a b)
builtinMin [VInt a, VFloat b] = return $ VFloat (min (fromInteger a) b)
builtinMin [VFloat a, VInt b] = return $ VFloat (min a (fromInteger b))
builtinMin _ = error "Type error: min expects two numbers"

builtinMax :: [Value] -> IO Value
builtinMax [VInt a, VInt b] = return $ VInt (max a b)
builtinMax [VFloat a, VFloat b] = return $ VFloat (max a b)
builtinMax [VInt a, VFloat b] = return $ VFloat (max (fromInteger a) b)
builtinMax [VFloat a, VInt b] = return $ VFloat (max a (fromInteger b))
builtinMax _ = error "Type error: max expects two numbers"

builtinPow :: [Value] -> IO Value
builtinPow [VInt a, VInt b] = return $ VFloat (fromInteger a ** fromInteger b)
builtinPow [VFloat a, VFloat b] = return $ VFloat (a ** b)
builtinPow [VInt a, VFloat b] = return $ VFloat (fromInteger a ** b)
builtinPow [VFloat a, VInt b] = return $ VFloat (a ** fromInteger b)
builtinPow _ = error "Type error: pow expects two numbers"

builtinSqrt :: [Value] -> IO Value
builtinSqrt [VInt n] = return $ VFloat (sqrt (fromInteger n))
builtinSqrt [VFloat n] = return $ VFloat (sqrt n)
builtinSqrt _ = error "Type error: sqrt expects a number"

-- List builtins
builtinCons :: [Value] -> IO Value
builtinCons [val, VList vs] = return $ VList (val : vs)
builtinCons [val, VVoid] = return $ VList [val]
builtinCons _ = error "Type error: cons expects (value, list)"

builtinCar :: [Value] -> IO Value
builtinCar [VList (v:_)] = return v
builtinCar [VList []] = error "Runtime error: car on empty list"
builtinCar _ = error "Type error: car expects a list"

builtinCdr :: [Value] -> IO Value
builtinCdr [VList (_:vs)] = return $ VList vs
builtinCdr [VList []] = error "Runtime error: cdr on empty list"
builtinCdr _ = error "Type error: cdr expects a list"

builtinList :: [Value] -> IO Value
builtinList vals = return $ VList vals

builtinLength :: [Value] -> IO Value
builtinLength [VList vs] = return $ VInt (toInteger $ length vs)
builtinLength [VString s] = return $ VInt (toInteger $ length s)
builtinLength _ = error "Type error: length expects a list or string"

builtinAppend :: [Value] -> IO Value
builtinAppend [VList a, VList b] = return $ VList (a ++ b)
builtinAppend [VString a, VString b] = return $ VString (a ++ b)
builtinAppend _ = error "Type error: append expects two lists or two strings"

-- String builtins (additional)
builtinStringUpper :: [Value] -> IO Value
builtinStringUpper [VString s] = return $ VString (map toUpper s)
builtinStringUpper _ = error "Type error: str-upper expects a string"

builtinStringLower :: [Value] -> IO Value
builtinStringLower [VString s] = return $ VString (map toLower s)
builtinStringLower _ = error "Type error: str-lower expects a string"

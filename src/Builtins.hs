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
  , ("string-append", VBuiltin "string-append" builtinStringAppend)
  , ("substring", VBuiltin "substring" builtinSubstring)
  , ("not", VBuiltin "not" builtinNot)
  , ("and", VBuiltin "and" builtinAnd)
  , ("or", VBuiltin "or" builtinOr)
  , ("format", VBuiltin "format" builtinFormat)
  , ("show", VBuiltin "show" builtinShow)
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
builtinShow [val] = return $ VString (showValue val)
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

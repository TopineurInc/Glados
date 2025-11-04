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
  ) where

import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import AST

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
  ]

-- | Helper for numeric binary operations supporting mixed Int/Float types
numericBinOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> String -> [Value] -> IO Value
numericBinOp intOp _ _ [VInt a, VInt b] = pure $ VInt (intOp a b)
numericBinOp _ floatOp _ [VFloat a, VFloat b] = pure $ VFloat (floatOp a b)
numericBinOp _ floatOp _ [VInt a, VFloat b] = pure $ VFloat (floatOp (fromInteger a) b)
numericBinOp _ floatOp _ [VFloat a, VInt b] = pure $ VFloat (floatOp a (fromInteger b))
numericBinOp _ _ name _ = error $ "Type error: " ++ name ++ " expects two numbers"

builtinAdd :: [Value] -> IO Value
builtinAdd = numericBinOp (+) (+) "+"

builtinSub :: [Value] -> IO Value
builtinSub = numericBinOp (-) (-) "-"

builtinMul :: [Value] -> IO Value
builtinMul = numericBinOp (*) (*) "*"

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

-- | Helper for comparison operations
numericCmpOp :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> String -> [Value] -> IO Value
numericCmpOp intCmp _ _ [VInt a, VInt b] = pure $ VBool (intCmp a b)
numericCmpOp _ floatCmp _ [VFloat a, VFloat b] = pure $ VBool (floatCmp a b)
numericCmpOp _ floatCmp _ [VInt a, VFloat b] = pure $ VBool (floatCmp (fromInteger a) b)
numericCmpOp _ floatCmp _ [VFloat a, VInt b] = pure $ VBool (floatCmp a (fromInteger b))
numericCmpOp _ _ name _ = error $ "Type error: " ++ name ++ " expects two numbers"

builtinLt :: [Value] -> IO Value
builtinLt = numericCmpOp (<) (<) "<"

builtinGt :: [Value] -> IO Value
builtinGt = numericCmpOp (>) (>) ">"

builtinLte :: [Value] -> IO Value
builtinLte = numericCmpOp (<=) (<=) "<="

builtinGte :: [Value] -> IO Value
builtinGte = numericCmpOp (>=) (>=) ">="

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
  VString <$> getLine
builtinInput [] = VString <$> getLine
builtinInput _ = error "Type error: input expects zero or one argument (prompt)"

builtinReadLine :: [Value] -> IO Value
builtinReadLine [] = VString <$> getLine
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

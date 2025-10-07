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
  , builtinGt
  , builtinPrint
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
  ) where

import AST
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

builtins :: Map.Map Name Value
builtins = Map.fromList builtinEntries

builtinEntries :: [(Name, Value)]
builtinEntries =
  concat
    [ constantEntries
    , arithmeticEntries
    , comparisonEntries
    , ioEntries
    , stringEntries
    , logicEntries
    , formatEntries
    ]

constantEntries :: [(Name, Value)]
constantEntries =
  [ ("t", VBool True)
  , ("nil", VBool False)
  ]

arithmeticEntries :: [(Name, Value)]
arithmeticEntries =
  map builtinFunction
    [ ("+", builtinAdd)
    , ("-", builtinSub)
    , ("*", builtinMul)
    , ("div", builtinDiv)
    , ("mod", builtinMod)
    ]

comparisonEntries :: [(Name, Value)]
comparisonEntries =
  map builtinFunction
    [ ("eq?", builtinEq)
    , ("<", builtinLt)
    , (">", builtinGt)
    ]

ioEntries :: [(Name, Value)]
ioEntries =
  map builtinFunction
    [ ("print", builtinPrint)
    , ("display", builtinDisplay)
    , ("input", builtinInput)
    , ("read-line", builtinReadLine)
    ]

stringEntries :: [(Name, Value)]
stringEntries =
  map builtinFunction
    [ ("string->number", builtinStringToNumber)
    , ("number->string", builtinNumberToString)
    , ("string-length", builtinStringLength)
    , ("string-append", builtinStringAppend)
    , ("substring", builtinSubstring)
    ]

logicEntries :: [(Name, Value)]
logicEntries =
  map builtinFunction
    [ ("not", builtinNot)
    , ("and", builtinAnd)
    , ("or", builtinOr)
    ]

formatEntries :: [(Name, Value)]
formatEntries =
  map builtinFunction [("format", builtinFormat)]

builtinFunction :: (Name, [Value] -> IO Value) -> (Name, Value)
builtinFunction (name, fn) = (name, VBuiltin name fn)

builtinAdd :: [Value] -> IO Value
builtinAdd [VInt a, VInt b] = pure $ VInt (a + b)
builtinAdd _ = error "Type error: + expects two integers"

builtinSub :: [Value] -> IO Value
builtinSub [VInt a, VInt b] = pure $ VInt (a - b)
builtinSub _ = error "Type error: - expects two integers"

builtinMul :: [Value] -> IO Value
builtinMul [VInt a, VInt b] = pure $ VInt (a * b)
builtinMul _ = error "Type error: * expects two integers"

builtinDiv :: [Value] -> IO Value
builtinDiv [VInt a, VInt b]
  | b == 0 = error "Division by zero"
  | otherwise = pure $ VInt (a `div` b)
builtinDiv _ = error "Type error: div expects two integers"

builtinMod :: [Value] -> IO Value
builtinMod [VInt a, VInt b]
  | b == 0 = error "Division by zero"
  | otherwise = pure $ VInt (a `mod` b)
builtinMod _ = error "Type error: mod expects two integers"

-- Comparison operations
builtinEq :: [Value] -> IO Value
builtinEq [VInt a, VInt b] = pure $ VBool (a == b)
builtinEq [VBool a, VBool b] = pure $ VBool (a == b)
builtinEq [VString a, VString b] = pure $ VBool (a == b)
builtinEq _ = pure $ VBool False

builtinLt :: [Value] -> IO Value
builtinLt [VInt a, VInt b] = pure $ VBool (a < b)
builtinLt _ = error "Type error: < expects two integers"

builtinGt :: [Value] -> IO Value
builtinGt [VInt a, VInt b] = pure $ VBool (a > b)
builtinGt _ = error "Type error: > expects two integers"

builtinPrint :: [Value] -> IO Value
builtinPrint [val] = putStrLn (showValue val) >> pure val
builtinPrint _ = error "Type error: print expects one argument"

builtinDisplay :: [Value] -> IO Value
builtinDisplay [val] =
  putStr (showValue val)
    >> hFlush stdout
    >> pure val
builtinDisplay _ = error "Type error: display expects one argument"

builtinInput :: [Value] -> IO Value
builtinInput [VString prompt] =
  putStr prompt
    >> hFlush stdout
    >> fmap VString getLine
builtinInput [] = VString <$> getLine
builtinInput _ =
  error "Type error: input expects zero or one argument (prompt)"

builtinReadLine :: [Value] -> IO Value
builtinReadLine [] = VString <$> getLine
builtinReadLine _ = error "Type error: read-line expects no arguments"

builtinStringToNumber :: [Value] -> IO Value
builtinStringToNumber [VString s] =
  case readMaybe s of
    Just n -> pure $ VInt n
    Nothing ->
      error $
        "Type error: cannot convert '" ++ s ++ "' to number"
builtinStringToNumber _ = error "Type error: string->number expects a string"

builtinNumberToString :: [Value] -> IO Value
builtinNumberToString [VInt n] = pure $ VString (show n)
builtinNumberToString _ = error "Type error: number->string expects an integer"

builtinStringLength :: [Value] -> IO Value
builtinStringLength [VString s] = pure $ VInt (toInteger $ length s)
builtinStringLength _ = error "Type error: string-length expects a string"

builtinStringAppend :: [Value] -> IO Value
builtinStringAppend [VString a, VString b] = pure $ VString (a ++ b)
builtinStringAppend _ = error "Type error: string-append expects two strings"

builtinSubstring :: [Value] -> IO Value
builtinSubstring [VString s, VInt start, VInt end] =
  let s' = take (fromInteger $ end - start) $ drop (fromInteger start) s
  in pure $ VString s'
builtinSubstring _ =
  error "Type error: substring expects (string, start, end)"

builtinNot :: [Value] -> IO Value
builtinNot [VBool b] = pure $ VBool (not b)
builtinNot _ = error "Type error: not expects a boolean"

builtinAnd :: [Value] -> IO Value
builtinAnd [VBool a, VBool b] = pure $ VBool (a && b)
builtinAnd _ = error "Type error: and expects two booleans"

builtinOr :: [Value] -> IO Value
builtinOr [VBool a, VBool b] = pure $ VBool (a || b)
builtinOr _ = error "Type error: or expects two booleans"

-- Format function (Common Lisp style)
builtinFormat :: [Value] -> IO Value
builtinFormat (dest:VString fmt:args) =
  let formatted = processFormatString fmt args
  in case dest of
       VBool True ->
         putStr formatted
           >> hFlush stdout
           >> pure (VBool False)
       VBool False -> pure $ VString formatted
       _ -> error "Type error: format destination must be t or nil"
builtinFormat _ =
  error "Type error: format expects (destination format-string ...)"

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
showValue (VBool True) = "#t"
showValue (VBool False) = "#f"
showValue (VString s) = s
showValue (VBuiltin name _) = "<builtin:" ++ name ++ ">"
showValue (VClosure name _) = "<closure:" ++ name ++ ">"

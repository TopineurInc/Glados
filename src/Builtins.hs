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
  ) where

import AST
import qualified Data.Map as Map

-- Map of builtin functions
builtins :: Map.Map Name Value
builtins = Map.fromList
  [ ("+", VBuiltin "+" builtinAdd)
  , ("-", VBuiltin "-" builtinSub)
  , ("*", VBuiltin "*" builtinMul)
  , ("div", VBuiltin "div" builtinDiv)
  , ("mod", VBuiltin "mod" builtinMod)
  , ("eq?", VBuiltin "eq?" builtinEq)
  , ("<", VBuiltin "<" builtinLt)
  , (">", VBuiltin ">" builtinGt)
  , ("print", VBuiltin "print" builtinPrint)
  ]

-- Arithmetic operations
builtinAdd :: [Value] -> IO Value
builtinAdd [VInt a, VInt b] = return $ VInt (a + b)
builtinAdd _ = error "Type error: + expects two integers"

builtinSub :: [Value] -> IO Value
builtinSub [VInt a, VInt b] = return $ VInt (a - b)
builtinSub _ = error "Type error: - expects two integers"

builtinMul :: [Value] -> IO Value
builtinMul [VInt a, VInt b] = return $ VInt (a * b)
builtinMul _ = error "Type error: * expects two integers"

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

-- Comparison operations
builtinEq :: [Value] -> IO Value
builtinEq [VInt a, VInt b] = return $ VBool (a == b)
builtinEq [VBool a, VBool b] = return $ VBool (a == b)
builtinEq [VString a, VString b] = return $ VBool (a == b)
builtinEq _ = return $ VBool False

builtinLt :: [Value] -> IO Value
builtinLt [VInt a, VInt b] = return $ VBool (a < b)
builtinLt _ = error "Type error: < expects two integers"

builtinGt :: [Value] -> IO Value
builtinGt [VInt a, VInt b] = return $ VBool (a > b)
builtinGt _ = error "Type error: > expects two integers"

-- I/O operations
builtinPrint :: [Value] -> IO Value
builtinPrint [val] = do
  putStrLn $ show val
  return val
builtinPrint _ = error "Type error: print expects one argument"
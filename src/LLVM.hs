{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- src/LLVM.hs
-}

{-# LANGUAGE DeriveGeneric #-}

module LLVM
  ( LLType(..)
  , toLLVMType
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T

-- LLVM Types (maybe addd more)
data LLType
    = I1                        -- Boolean (no real bool type but int1 is good)
    | I32                       -- 32-bit Integers
    | I64                       -- 64-bit Integers
    | F32                       -- 32-bit Float
    | F64                       -- 64-bit Float (double)
    | Void                      -- Nothing (void)
    | FunctionType LLType       -- Functions Types
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- Convert simple LLType into llvm-hs Type
toLLVMType :: LLType -> AST.Type
toLLVMType I1               = T.i1
toLLVMType I32              = T.i32
toLLVMType I64              = T.i64
toLLVMType F32              = T.float
toLLVMType F64              = T.double
toLLVMType Void             = T.void
toLLVMType (FunctionType t) =
    let ret = toLLVMType t
    in T.FunctionType ret [] False

-- Build a LLVM function Type with LLType and List of LLTypes
mkFunctionType :: LLType -> [LLType] -> AST.Type
mkFunctionType ret params =
  let retT = toLLVMType ret
      paramTs = fmap toLLVMType params
  in T.FunctionType retT paramTs False

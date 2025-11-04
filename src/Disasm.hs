{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Disasm
-}

module Disasm
  ( disassemble
  , disassembleInstr
  , dumpCodeObject
  ) where

import AST
import qualified Data.Vector as Vector

disassemble :: CodeObject -> String
disassemble co = unlines
  [ "CodeObject: " ++ coName co
  , "  Arity: " ++ show (coArity co)
  , "  MaxLocals: " ++ show (coMaxLocals co)
  , "  Annotations: " ++ show (coAnnotations co)
  , "  Constants:"
  , dumpConstants (coConsts co)
  , "  Instructions:"
  , dumpInstructions (coInstrs co)
  ]

dumpConstants :: Vector.Vector Constant -> String
dumpConstants consts =
  unlines $ Vector.toList $ Vector.imap formatConst consts
  where
    formatConst idx c = "    " ++ show idx ++ ": " ++ showConstant c

dumpInstructions :: Vector.Vector Instr -> String
dumpInstructions instrs =
  unlines $ Vector.toList $ Vector.imap formatInstr instrs
  where
    formatInstr pc instr = "    " ++ padLeft 4 (show pc) ++ ": " ++ disassembleInstr instr

disassembleInstr :: Instr -> String
disassembleInstr (IConst idx) = "CONST " ++ show idx
disassembleInstr (ILoad slot) = "LOAD " ++ show slot
disassembleInstr (IStore slot) = "STORE " ++ show slot
disassembleInstr (IPrim op) = "PRIM " ++ op
disassembleInstr (ICall arity name) = "CALL " ++ show arity ++ " " ++ name
disassembleInstr (ITailCall arity name) = "TAILCALL " ++ show arity ++ " " ++ name
disassembleInstr IReturn = "RETURN"
disassembleInstr (IJump target) = "JUMP " ++ show target
disassembleInstr (IJumpIfFalse target) = "JUMP_IF_FALSE " ++ show target
disassembleInstr IPop = "POP"
disassembleInstr INop = "NOP"
disassembleInstr (IMakeClosure name slots) = "MAKE_CLOSURE " ++ name ++ " " ++ show slots
disassembleInstr (ILoadClosure idx) = "LOAD_CLOSURE " ++ show idx
disassembleInstr (IStoreClosure idx) = "STORE_CLOSURE " ++ show idx
disassembleInstr IWhile = "WHILE"
disassembleInstr IFor = "FOR"
disassembleInstr IBreak = "BREAK"
disassembleInstr IContinue = "CONTINUE"
disassembleInstr (ITupleCreate size) = "TUPLE_CREATE " ++ show size
disassembleInstr (ITupleGet idx) = "TUPLE_GET " ++ show idx
disassembleInstr (IListCreate size) = "LIST_CREATE " ++ show size
disassembleInstr IListGet = "LIST_GET"
disassembleInstr IListSet = "LIST_SET"
disassembleInstr (IObjectCreate name) = "OBJECT_CREATE " ++ name
disassembleInstr (IMemberGet name) = "MEMBER_GET " ++ name
disassembleInstr (IMemberSet name) = "MEMBER_SET " ++ name
disassembleInstr (IAssign slot) = "ASSIGN " ++ show slot
disassembleInstr (IAssignGlobal name) = "ASSIGN_GLOBAL " ++ name
disassembleInstr IRangeCreate = "RANGE_CREATE"

showConstant :: Constant -> String
showConstant (CInt n) = "INT " ++ show n
showConstant (CFloat n) = "FLOAT " ++ show n
showConstant (CBool b) = "BOOL " ++ show b
showConstant (CString s) = "STRING " ++ show s
showConstant (CFuncRef name) = "FUNCREF " ++ name

dumpCodeObject :: CodeObject -> IO ()
dumpCodeObject co = putStrLn (disassemble co)

padLeft :: Int -> String -> String
padLeft n s
  | length s >= n = s
  | otherwise = replicate (n - length s) ' ' ++ s

{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- app/Main.hs
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import qualified Data.Map as Map

import AST
import Compiler
import VM
import Disasm
import SExprParser
import Desugar
import qualified Data.Vector as Vector

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    ["--help"] -> printHelp
    ["--disasm", file] -> disasmFile file
    ["--ast", file] -> showAst file
    ["--compiled", file] -> showCompiled file
    [file] -> runFile file
    _ -> do
      hPutStrLn stderr "Invalid arguments. Use --help for usage."
      exitFailure

printHelp :: IO ()
printHelp = do
  putStrLn "GLaDOS - A LISP compiler and VM"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  glados-exe [file]             Compile and run a LISP file"
  putStrLn "  glados-exe --disasm [file]    Disassemble a LISP file"
  putStrLn "  glados-exe --ast [file]       Show AST of a LISP file"
  putStrLn "  glados-exe --compiled [file]  Show compiled code of a LISP file"
  putStrLn "  glados-exe                    Start REPL (interactive mode)"
  putStrLn "  glados-exe --help             Show this help message"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  glados-exe program.lisp"
  putStrLn "  glados-exe --disasm program.lisp"
  putStrLn "  glados-exe --ast program.lisp"
  putStrLn "  glados-exe --compiled program.lisp"

-- Run a file
runFile :: FilePath -> IO ()
runFile file = do
  source <- readFile file
  case compileWithDefs defaultConfig source of
    Left err -> do
      hPutStrLn stderr $ "Compilation error: " ++ show err
      exitFailure
    Right (code, defs) -> do
      let allCodeObjects = Map.insert "main" code defs
      let vmState = initVMState { vCodeObjects = allCodeObjects }
      result <- execVM vmState code
      case result of
        Left err -> do
          hPutStrLn stderr $ "Runtime error: " ++ show err
          exitFailure
        Right val -> do
          -- Only print result if it's not nil (False from format)
          case val of
            VBool False -> return ()
            _ -> putStrLn $ show val
          exitSuccess

-- Disassemble a file
disasmFile :: FilePath -> IO ()
disasmFile file = do
  source <- readFile file
  case compileWithDefs defaultConfig source of
    Left err -> do
      hPutStrLn stderr $ "Compilation error: " ++ show err
      exitFailure
    Right (code, defs) -> do
      putStrLn "=== Main Code ==="
      dumpCodeObject code
      putStrLn "\n=== Nested Definitions ==="
      mapM_ (\(name, obj) -> do
        putStrLn $ "\n--- " ++ name ++ " ---"
        dumpCodeObject obj) (Map.toList defs)
      exitSuccess

-- Show AST of a file
showAst :: FilePath -> IO ()
showAst file = do
  source <- readFile file
  case parseFromString source of
    Left err -> do
      hPutStrLn stderr $ "Parse error: " ++ show err
      exitFailure
    Right sexprs -> do
      putStrLn "=== S-Expression ==="
      mapM_ (putStrLn . show) sexprs
      putStrLn "\n=== AST ==="
      case mapM sexprToExpr sexprs of
        Left err -> do
          hPutStrLn stderr $ "Desugar error: " ++ show err
          exitFailure
        Right exprs -> do
          mapM_ (putStrLn . show) exprs
          exitSuccess

-- Show compiled code of a file
showCompiled :: FilePath -> IO ()
showCompiled file = do
  source <- readFile file
  case compileWithDefs defaultConfig source of
    Left err -> do
      hPutStrLn stderr $ "Compilation error: " ++ show err
      exitFailure
    Right (code, defs) -> do
      putStrLn "=== Main Code ==="
      dumpCodeInfo code
      putStrLn "\n=== Nested Definitions ==="
      mapM_ (\(name, obj) -> do
        putStrLn $ "\n--- " ++ name ++ " ---"
        dumpCodeInfo obj) (Map.toList defs)
      exitSuccess

dumpCodeInfo :: CodeObject -> IO ()
dumpCodeInfo co = do
  putStrLn $ "Name: " ++ coName co
  putStrLn $ "Arity: " ++ show (coArity co)
  putStrLn $ "Max Locals: " ++ show (coMaxLocals co)
  putStrLn "\nConstants:"
  Vector.imapM_ (\i c -> putStrLn $ "  " ++ show i ++ ": " ++ show c) (coConsts co)
  putStrLn "\nInstructions:"
  Vector.imapM_ (\i instr -> putStrLn $ "  " ++ show i ++ ": " ++ show instr) (coInstrs co)

-- Simple REPL
repl :: IO ()
repl = do
  putStrLn "GLaDOS REPL - Enter expressions (Ctrl+D to exit)"
  replLoop

replLoop :: IO ()
replLoop = do
  putStr "> "
  input <- getLine
  case compile defaultConfig input of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      replLoop
    Right code -> do
      let vmState = initVMState { vCodeObjects = Map.singleton "main" code }
      result <- execVM vmState code
      case result of
        Left err -> putStrLn $ "Runtime error: " ++ show err
        Right val -> putStrLn $ show val
      replLoop

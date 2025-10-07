{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Main
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    ["--help"] -> printHelp
    ["--disasm", file] -> disasmFile file
    [file] -> runFile file
    _ -> do
      hPutStrLn stderr "Invalid arguments. Use --help for usage."
      exitFailure

printHelp :: IO ()
printHelp = do
  putStrLn "GLaDOS - A LISP compiler and VM"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  glados-exe [file]           Compile and run a LISP file"
  putStrLn "  glados-exe --disasm [file]  Disassemble a LISP file"
  putStrLn "  glados-exe                  Start REPL (interactive mode)"
  putStrLn "  glados-exe --help           Show this help message"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  glados-exe program.lisp"
  putStrLn "  glados-exe --disasm program.lisp"

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
          putStrLn $ "Result: " ++ show val
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
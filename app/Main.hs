{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- app/Main.hs
-}

module Main (main) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, isEOF, stderr, stdin, stdout)

import AST
import Compiler
import Desugar
import Disasm
import SExprParser
import VM

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
      exitWithError "Invalid arguments. Use --help for usage."

exitWithError :: String -> IO a
exitWithError msg = do
  hPutStrLn stderr $ "*** ERROR : " ++ msg
  exitWith (ExitFailure 84)

formatCompileError :: CompileError -> String
formatCompileError (ParseError err _) = "ParseError " ++ err
formatCompileError (SyntaxError err _) = "Compilation error: " ++ err

renderValue :: Value -> String
renderValue (VInt i) = show i
renderValue (VBool True) = "#t"
renderValue (VBool False) = "#f"
renderValue (VString s) = s
renderValue (VClosure _ _) = "#<procedure>"
renderValue (VBuiltin name _) = "#<builtin:" ++ name ++ ">"

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
  sourceOrErr <- try (readFile file) :: IO (Either IOException String)
  source <- case sourceOrErr of
    Left _ -> exitWithError $ "Cannot open file: " ++ file
    Right src -> return src
  case compileWithDefs defaultConfig source of
    Left err -> exitWithError (formatCompileError err)
    Right (code, defs) -> do
      let allCodeObjects = Map.insert "main" code defs
      let vmState = initVMState { vCodeObjects = allCodeObjects }
      execResult <- try (execVM vmState code) :: IO (Either SomeException (Either VMError Value))
      case execResult of
        Left ex -> exitWithError $ "Runtime error: " ++ show ex
        Right (Left err) -> exitWithError $ "Runtime error: " ++ show err
        Right (Right val) -> do
          case val of
            VBool False -> return ()
            _ -> putStrLn $ renderValue val
          exitSuccess

-- Disassemble a file
disasmFile :: FilePath -> IO ()
disasmFile file = do
  sourceOrErr <- try (readFile file) :: IO (Either IOException String)
  source <- case sourceOrErr of
    Left _ -> exitWithError $ "Cannot open file: " ++ file
    Right src -> return src
  case compileWithDefs defaultConfig source of
    Left err -> exitWithError (formatCompileError err)
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
  sourceOrErr <- try (readFile file) :: IO (Either IOException String)
  source <- case sourceOrErr of
    Left _ -> exitWithError $ "Cannot open file: " ++ file
    Right src -> return src
  case parseFromString source of
    Left err -> exitWithError (formatCompileError err)
    Right sexprs -> do
      putStrLn "=== S-Expression ==="
      mapM_ (putStrLn . show) sexprs
      putStrLn "\n=== AST ==="
      case mapM sexprToExpr sexprs of
        Left err -> exitWithError ("Desugar error: " ++ show err)
        Right exprs -> do
          mapM_ (putStrLn . show) exprs
          exitSuccess

-- Show compiled code of a file
showCompiled :: FilePath -> IO ()
showCompiled file = do
  sourceOrErr <- try (readFile file) :: IO (Either IOException String)
  source <- case sourceOrErr of
    Left _ -> exitWithError $ "Cannot open file: " ++ file
    Right src -> return src
  case compileWithDefs defaultConfig source of
    Left err -> exitWithError (formatCompileError err)
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
  interactive <- hIsTerminalDevice stdin
  when interactive $
    putStrLn "GLaDOS REPL - Enter expressions (Ctrl+D to exit)"
  replLoop interactive

replLoop :: Bool -> IO ()
replLoop interactive = do
  when interactive $ do
    putStr "> "
    hFlush stdout
  eof <- isEOF
  if eof
    then when interactive (putStrLn "")
    else do
      input <- getLine
      case compile defaultConfig input of
        Left err -> do
          putStrLn $ "*** ERROR : " ++ formatCompileError err
          replLoop interactive
        Right code -> do
          let vmState = initVMState { vCodeObjects = Map.singleton "main" code }
          execResult <- try (execVM vmState code) :: IO (Either SomeException (Either VMError Value))
          case execResult of
            Left ex -> putStrLn $ "*** ERROR : Runtime error: " ++ show ex
            Right (Left err) -> putStrLn $ "*** ERROR : Runtime error: " ++ show err
            Right (Right val) -> putStrLn $ renderValue val
          replLoop interactive

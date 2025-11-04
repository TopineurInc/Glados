{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Main
-}

module Main (main) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad (foldM, unless)
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitSuccess, exitWith)
import System.FilePath (takeExtension)
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, isEOF, stderr, stdin, stdout)

import AST
import Compiler
import Desugar (sexprToExpr)
import Disasm (dumpCodeObject)
import SExprParser (parseFromString)
import TopineurParser (parseTopineurSource)
import VM

main :: IO ()
main = do
  args <- getArgs
  let (noCache, restArgs) = parseFlags args
      config = defaultConfig { cfgUseCache = not noCache }
  case restArgs of
    [] -> repl
    ["--help"] -> printHelp
    ["--disasm", file] -> disasmFile config file
    ["--ast", file] -> showAst file
    ["--compiled", file] -> showCompiled config file
    ["--bytecode", file] -> showBytecode config file
    ["--parse-top", file] -> parseTopineurFile file
    [file] -> runFile config file
    _ ->
      exitWithError "Invalid arguments. Use --help for usage."
  where
    parseFlags :: [String] -> (Bool, [String])
    parseFlags xs = ("--no-cache" `elem` xs, filter (/= "--no-cache") xs)

exitWithError :: String -> IO a
exitWithError msg =
  hPutStrLn stderr ("*** ERROR : " ++ msg) >> exitWith (ExitFailure 84)

-- | Read a file or exit with an error message
readFileOrExit :: FilePath -> IO String
readFileOrExit file = do
  result <- try (readFile file) :: IO (Either IOException String)
  case result of
    Left _ -> exitWithError $ "Cannot open file: " ++ file
    Right src -> return src

-- | Compile source code from either Lisp or Topineur
compileSource :: CompilerConfig -> String -> FilePath -> IO (Either CompileError (CodeObject, Map.Map Name CodeObject))
compileSource config source file
  | takeExtension file == ".top" = compileTopineurFile config file
  | otherwise = return $ compileWithDefs config source

formatCompileError :: CompileError -> String
formatCompileError (ParseError err _) = "ParseError " ++ err
formatCompileError (SyntaxError err _) = "Compilation error: " ++ err

renderValue :: Value -> String
renderValue (VInt i) = show i
renderValue (VFloat f) = show f
renderValue (VBool True) = "#t"
renderValue (VBool False) = "#f"
renderValue (VString s) = s
renderValue (VClosure _ _) = "#<procedure>"
renderValue (VBuiltin name _) = "#<builtin:" ++ name ++ ">"
renderValue VUnit = "#<void>"
renderValue (VList values) = "(" ++ unwords (map renderValue values) ++ ")"
renderValue (VTuple values) = "#(" ++ unwords (map renderValue values) ++ ")"
renderValue (VObject name fields) =
  "#<object:" ++ name ++ " {" ++
  intercalate ", " [k ++ ": " ++ renderValue v | (k, v) <- fields] ++
  "}>"

runProgram :: String -> IO (Either String Value)
runProgram source =
  case compileWithDefs defaultConfig source of
    Left err -> return $ Left (formatCompileError err)
    Right (code, defs) -> do
      let allCodeObjects = Map.insert "main" code defs
          initialVmState = initVMState { vCodeObjects = allCodeObjects }
      vmState <- initializeGlobals initialVmState defs
      execResult <- try (execVM vmState code) :: IO (Either SomeException (Either VMError Value))
      case execResult of
        Left ex -> return $ Left $ "Runtime error: " ++ show ex
        Right (Left err) -> return $ Left $ "Runtime error: " ++ show err
        Right (Right val) -> return $ Right val

initializeGlobals :: VMState -> Map.Map Name CodeObject -> IO VMState
initializeGlobals vmState defs =
  foldM
    (\state (name, codeObj) ->
      if coArity codeObj == 0
        then do
          result <- execVM state codeObj
          case result of
            Right val ->
              return $ state { vGlobals = Map.insert name val (vGlobals state) }
            Left _err -> return state
        else return state)
    vmState
    (Map.toList defs)

runProgramWithSource :: CompilerConfig -> String -> String -> IO (Either String Value)
runProgramWithSource config source filePath =
  let isTopineur = takeExtension filePath == ".top"
  in if isTopineur
      then do
        compileResult <- compileTopineurFile config filePath
        case compileResult of
          Left err -> return $ Left (formatCompileError err)
          Right (code, defs) -> execute code defs
      else case compileWithDefs config source of
        Left err -> return $ Left (formatCompileError err)
        Right (code, defs) -> execute code defs
  where
    execute code defs = do
      let allCodeObjects = Map.insert "main" code defs
          initialVmState = initVMState { vCodeObjects = allCodeObjects }
      vmState <- initializeGlobals initialVmState defs
      execResult <- try (execVM vmState code) :: IO (Either SomeException (Either VMError Value))
      case execResult of
        Left ex -> return $ Left $ "Runtime error: " ++ show ex
        Right (Left err) -> return $ Left $ "Runtime error: " ++ show err
        Right (Right val) -> return $ Right val

printHelp :: IO ()
printHelp =
  mapM_
    putStrLn
    [ "GLaDOS - A LISP and Topineur compiler and VM"
    , ""
    , "Usage:"
    , "  ./glados [file]                 Compile and run a LISP file"
    , "  ./glados --disasm [file]        Disassemble a LISP file"
    , "  ./glados --ast [file]           Show AST of a LISP file"
    , "  ./glados --compiled [file]      Show compiled code of a LISP file"
    , "  ./glados --bytecode [file]      Show bytecode instructions of a LISP file"
    , "  ./glados --parse-top [file]     Show parsed structure of a Topineur file"
    , "  ./glados                        Start REPL (interactive mode)"
    , "  ./glados --help                 Show this help message"
    , ""
    , "Options:"
    , "  --no-cache                      Disable bytecode cache (.topo files)"
    , ""
    , "Examples:"
    , "  ./glados program.lisp"
    , "  ./glados program.top"
    , "  ./glados --no-cache program.top"
    , "  ./glados --disasm program.lisp"
    , "  ./glados --ast program.lisp"
    , "  ./glados --compiled program.lisp"
    , "  ./glados --parse-topineur program.top"
    ]

runFile :: CompilerConfig -> FilePath -> IO ()
runFile config file = do
  source <- readFileOrExit file
  result <- runProgramWithSource config source file
  case result of
    Left err -> exitWithError err
    Right val ->
      if takeExtension file == ".top"
        then exitWith (valueToExitCode val)
        else case val of
          VUnit -> exitSuccess
          _ -> putStrLn (renderValue val) >> exitSuccess

valueToExitCode :: Value -> ExitCode
valueToExitCode (VInt n)
  | n == 0 = ExitSuccess
  | otherwise = ExitFailure (fromInteger n)
valueToExitCode VUnit = ExitSuccess
valueToExitCode _ = ExitSuccess  -- Default to success for other types

disasmFile :: CompilerConfig -> FilePath -> IO ()
disasmFile config file = do
  source <- readFileOrExit file
  compileResult <- compileSource config source file
  case compileResult of
    Left err -> exitWithError (formatCompileError err)
    Right (code, defs) -> do
      putStrLn "=== Main Code ==="
      dumpCodeObject code
      putStrLn "\n=== Nested Definitions ==="
      mapM_ (\(name, obj) -> putStrLn ("\n--- " ++ name ++ " ---") >> dumpCodeObject obj) (Map.toList defs)
      exitSuccess

showAst :: FilePath -> IO ()
showAst file = do
  source <- readFileOrExit file
  case parseFromString source of
    Left err -> exitWithError (formatCompileError err)
    Right sexprs -> do
      putStrLn "=== S-Expression ==="
      mapM_ print sexprs
      putStrLn "\n=== AST ==="
      case mapM sexprToExpr sexprs of
        Left err -> exitWithError ("Desugar error: " ++ show err)
        Right exprs -> mapM_ print exprs >> exitSuccess

showCompiled :: CompilerConfig -> FilePath -> IO ()
showCompiled config file = do
  source <- readFileOrExit file
  compileResult <- compileSource config source file
  case compileResult of
    Left err -> exitWithError (formatCompileError err)
    Right (code, defs) -> do
      putStrLn "=== Main Code ==="
      dumpCodeInfo code
      putStrLn "\n=== Nested Definitions ==="
      mapM_ (\(name, obj) -> putStrLn ("\n--- " ++ name ++ " ---") >> dumpCodeInfo obj) (Map.toList defs)
      exitSuccess

showBytecode :: CompilerConfig -> FilePath -> IO ()
showBytecode config file = do
  source <- readFileOrExit file
  compileResult <- compileSource config source file
  case compileResult of
    Left err -> exitWithError (formatCompileError err)
    Right (code, defs) -> do
      putStrLn "=== Main Bytecode ==="
      dumpBytecode code
      putStrLn "\n=== Nested Definitions ==="
      mapM_ (\(name, obj) -> putStrLn ("\n--- " ++ name ++ " ---") >> dumpBytecode obj) (Map.toList defs)
      exitSuccess

parseTopineurFile :: FilePath -> IO ()
parseTopineurFile file = do
  source <- readFileOrExit file
  case parseTopineurSource source of
    Left err -> exitWithError (formatCompileError err)
    Right topineur -> do
      putStrLn "=== Topineur AST ==="
      print topineur
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

dumpBytecode :: CodeObject -> IO ()
dumpBytecode co = do
  putStrLn "Instructions:"
  Vector.imapM_ (\i instr -> putStrLn $ "  " ++ show i ++ ": " ++ show instr) (coInstrs co)

repl :: IO ()
repl = do
  interactive <- hIsTerminalDevice stdin
  if interactive
    then replInteractive
    else replNonInteractive

replLoop :: IO ()
replLoop = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn ""
    else do
      input <- getLine
      unless (all isSpace input) $ do
        result <- runProgram input
        case result of
          Left err -> putStrLn ("*** ERROR : " ++ err)
          Right VUnit -> return ()
          Right val -> putStrLn (renderValue val)
      replLoop

replInteractive :: IO ()
replInteractive = do
  putStrLn "GLaDOS REPL - Enter expressions (Ctrl+D to exit)"
  replLoop

replNonInteractive :: IO ()
replNonInteractive = do
  contents <- getContents
  if all isSpace contents
    then exitSuccess
    else runNonInteractive contents

runNonInteractive :: String -> IO ()
runNonInteractive contents = do
  result <- runProgram contents
  case result of
    Left err -> exitWithError err
    Right VUnit -> exitSuccess
    Right val -> putStrLn (renderValue val) >> exitSuccess

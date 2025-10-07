{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- app/Main.hs
-}

module Main (main) where

import Control.Monad (unless)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import AST
import Compiler
import VM
import Disasm
import SExprParser
import Desugar

main :: IO ()
main =
  getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [] = repl
handleArgs ["--help"] = printHelp
handleArgs ["--disasm", file] = disasmFile file
handleArgs ["--ast", file] = showAst file
handleArgs ["--compiled", file] = showCompiled file
handleArgs [file] = runFile file
handleArgs _ = invalidArguments

invalidArguments :: IO a
invalidArguments =
  hPutStrLn stderr "Invalid arguments. Use --help for usage." >> exitFailure

printHelp :: IO ()
printHelp = mapM_ putStrLn helpLines

helpLines :: [String]
helpLines = headerLines ++ usageLines ++ exampleLines

headerLines :: [String]
headerLines =
  [ "GLaDOS - A LISP compiler and VM"
  , ""
  ]

usageLines :: [String]
usageLines =
  [ "Usage:"
  , "  glados-exe [file]             Compile and run a LISP file"
  , "  glados-exe --disasm [file]    Disassemble a LISP file"
  , "  glados-exe --ast [file]       Show AST of a LISP file"
  , "  glados-exe --compiled [file]  Show compiled code of a LISP file"
  , "  glados-exe                    Start REPL (interactive mode)"
  , "  glados-exe --help             Show this help message"
  , ""
  ]

exampleLines :: [String]
exampleLines =
  [ "Examples:"
  , "  glados-exe program.lisp"
  , "  glados-exe --disasm program.lisp"
  , "  glados-exe --ast program.lisp"
  , "  glados-exe --compiled program.lisp"
  ]

runFile :: FilePath -> IO ()
runFile file =
  readFile file >>= \source ->
    either (dieWithError "Compilation error: ") runCompiled
      (compileWithDefs defaultConfig source)

disasmFile :: FilePath -> IO ()
disasmFile file =
  readFile file >>= \source ->
    either (dieWithError "Compilation error: ") renderDisasm
      (compileWithDefs defaultConfig source)

showAst :: FilePath -> IO ()
showAst file =
  readFile file >>= \source ->
    either (dieWithError "Parse error: ") displaySexprs
      (parseFromString source)

showCompiled :: FilePath -> IO ()
showCompiled file =
  readFile file >>= \source ->
    either (dieWithError "Compilation error: ") renderCompiledInfo
      (compileWithDefs defaultConfig source)

dumpCodeInfo :: CodeObject -> IO ()
dumpCodeInfo co =
  sequence_
    [ putStrLn ("Name: " ++ coName co)
    , putStrLn ("Arity: " ++ show (coArity co))
    , putStrLn ("Max Locals: " ++ show (coMaxLocals co))
    , putStrLn ""
    ]
    >> dumpVector "Constants:" printConst (coConsts co)
    >> putStrLn ""
    >> dumpVector "Instructions:" printInstr (coInstrs co)

repl :: IO ()
repl =
  putStrLn "GLaDOS REPL - Enter expressions (Ctrl+D to exit)" >> replLoop

replLoop :: IO ()
replLoop =
  putStr "> " >> getLine >>= processInput

processInput :: String -> IO ()
processInput input =
  either handleCompileError runReplCode (compile defaultConfig input)

runCompiled :: (CodeObject, Map.Map Name CodeObject) -> IO ()
runCompiled (code, defs) =
  execVM (mkVMState code defs) code
    >>= either handleRuntimeError completeExecution

completeExecution :: Value -> IO ()
completeExecution val =
  printValueUnlessFalse val >> exitSuccess

renderDisasm :: (CodeObject, Map.Map Name CodeObject) -> IO ()
renderDisasm (code, defs) =
  sequence_
    [ putStrLn "=== Main Code ==="
    , dumpCodeObject code
    , putStrLn ""
    , putStrLn "=== Nested Definitions ==="
    ]
    >> mapM_ (dumpWithHeading dumpCodeObject) (Map.toList defs)
    >> exitSuccess

displaySexprs :: [SExpr] -> IO ()
displaySexprs sexprs =
  sequence_
    [ putStrLn "=== S-Expression ==="
    , mapM_ (putStrLn . show) sexprs
    , putStrLn ""
    , putStrLn "=== AST ==="
    ]
    >> case mapM sexprToExpr sexprs of
        Left err -> dieWithError "Desugar error: " err
        Right exprs -> displayExprs exprs

displayExprs :: [Expr] -> IO ()
displayExprs exprs = mapM_ (putStrLn . show) exprs >> exitSuccess

renderCompiledInfo :: (CodeObject, Map.Map Name CodeObject) -> IO ()
renderCompiledInfo (code, defs) =
  sequence_
    [ putStrLn "=== Main Code ==="
    , dumpCodeInfo code
    , putStrLn ""
    , putStrLn "=== Nested Definitions ==="
    ]
    >> mapM_ (dumpWithHeading dumpCodeInfo) (Map.toList defs)
    >> exitSuccess

dumpWithHeading :: (CodeObject -> IO ()) -> (Name, CodeObject) -> IO ()
dumpWithHeading action (name, obj) =
  sequence_
    [ putStrLn ""
    , putStrLn ("--- " ++ name ++ " ---")
    , action obj
    ]

dumpVector :: String -> (Int -> a -> IO ()) -> Vector.Vector a -> IO ()
dumpVector title printer vec =
  putStrLn title >> Vector.imapM_ printer vec

printConst :: Int -> Constant -> IO ()
printConst i c = putStrLn ("  " ++ show i ++ ": " ++ show c)

printInstr :: Int -> Instr -> IO ()
printInstr i instr = putStrLn ("  " ++ show i ++ ": " ++ show instr)

dieWithError :: Show e => String -> e -> IO a
dieWithError label err = hPutStrLn stderr (label ++ show err) >> exitFailure

printValueUnlessFalse :: Value -> IO ()
printValueUnlessFalse val =
  unless (val == VBool False) (putStrLn (show val))

mkVMState :: CodeObject -> Map.Map Name CodeObject -> VMState
mkVMState code defs =
  let defsWithMain = Map.insert "main" code defs
  in initVMState { vCodeObjects = defsWithMain }

handleRuntimeError :: VMError -> IO a
handleRuntimeError = dieWithError "Runtime error: "

handleCompileError :: CompileError -> IO ()
handleCompileError err = putStrLn ("Error: " ++ show err) >> replLoop

runReplCode :: CodeObject -> IO ()
runReplCode code =
  execVM (mkReplState code) code
    >>= either handleRuntime displayValue
  where
    handleRuntime err =
      putStrLn ("Runtime error: " ++ show err) >> replLoop
    displayValue val =
      putStrLn (show val) >> replLoop

mkReplState :: CodeObject -> VMState
mkReplState code =
  let mainEntry = Map.singleton "main" code
  in initVMState { vCodeObjects = mainEntry }

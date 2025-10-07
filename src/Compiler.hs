{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Compiler
-}

module Compiler
  ( compile
  , compileWithDefs
  , compileProgram
  , CompilerConfig(..)
  , defaultConfig
  ) where

import AST
import SExprParser
import MacroExpander
import Desugar
import AlphaRename
import ClosureConversion
import CodeGen
import qualified Data.Map as Map
import Data.Map (Map)

data CompilerConfig = CompilerConfig
  { cfgTCO :: Bool  -- Tail call optimization enabled
  , cfgDebug :: Bool
  , cfgMacroEnv :: MacroEnv
  }

defaultConfig :: CompilerConfig
defaultConfig = CompilerConfig
  { cfgTCO = True
  , cfgDebug = False
  , cfgMacroEnv = defaultMacroEnv
  }

-- Compile a single program from source text
compile :: CompilerConfig -> String -> Either CompileError CodeObject
compile config source = do
  -- 1. Parse to S-expression
  exprs <- parseFromString source
  case exprs of
    [] -> Left $ SyntaxError "Empty program" Nothing
    sexprs -> do
      -- Transform program to wrap in letrec for mutually recursive definitions
      let transformed = transformProgram sexprs

      -- 2. Expand macros
      expanded <- expandMacros (cfgMacroEnv config) transformed

      -- 3. Desugar to AST
      expr <- sexprToExpr expanded

      -- 4. Alpha-rename
      renamed <- alphaRename expr

      -- 5. Closure conversion
      converted <- closureConvert renamed

      -- 6. Generate code
      let (mainCodeE, _) = generateCodeWithDefs "main" converted
      mainCodeE

-- Compile and return all code objects (main + nested definitions)
compileWithDefs :: CompilerConfig -> String -> Either CompileError (CodeObject, Map Name CodeObject)
compileWithDefs config source = do
  -- 1. Parse to S-expression
  exprs <- parseFromString source
  case exprs of
    [] -> Left $ SyntaxError "Empty program" Nothing
    sexprs -> do
      -- Transform program to wrap in letrec for mutually recursive definitions
      let transformed = transformProgram sexprs

      -- 2. Expand macros
      expanded <- expandMacros (cfgMacroEnv config) transformed

      -- 3. Desugar to AST
      expr <- sexprToExpr expanded

      -- 4. Alpha-rename
      renamed <- alphaRename expr

      -- 5. Closure conversion
      converted <- closureConvert renamed

      -- 6. Generate code with all nested definitions
      let (mainCodeE, defs) = generateCodeWithDefs "main" converted
      mainCode <- mainCodeE
      return (mainCode, defs)

-- Transform a program with defines into a letrec
transformProgram :: [SExpr] -> SExpr
transformProgram sexprs =
  let (defines, others) = span isDefine sexprs
      bindings = map extractBinding defines
      body = case others of
               [] -> SAtom (AInteger 0) Nothing
               [e] -> e
               es -> SList (SAtom (ASymbol "begin") Nothing : es) Nothing
  in if null bindings
     then body
     else SList [SAtom (ASymbol "letrec") Nothing,
                 SList (map toBinding bindings) Nothing,
                 body] Nothing
  where
    isDefine (SList (SAtom (ASymbol "define") _ : _) _) = True
    isDefine _ = False

    extractBinding (SList (SAtom (ASymbol "define") _ : SAtom (ASymbol name) _ : [expr]) _) =
      (name, expr)
    extractBinding (SList (SAtom (ASymbol "define") _ : SList (SAtom (ASymbol name) _ : params) _ : body : _) loc) =
      (name, SList [SAtom (ASymbol "lambda") Nothing, SList params Nothing, body] loc)
    extractBinding _ = error "Invalid define"

    toBinding (name, expr) =
      SList [SAtom (ASymbol name) Nothing, expr] Nothing

-- Compile a complete program (multiple top-level definitions)
compileProgram :: CompilerConfig -> String -> Either CompileError (Map.Map Name CodeObject)
compileProgram config source = do
  -- Parse all S-expressions
  sexprs <- parseMultipleSExprs source

  -- Process each definition
  codeObjects <- mapM (compileTopLevel config) sexprs

  -- Build a map of function names to code objects
  Right $ Map.fromList codeObjects

-- Compile a top-level definition
compileTopLevel :: CompilerConfig -> SExpr -> Either CompileError (Name, CodeObject)
compileTopLevel config sexpr = do
  -- Expand macros
  expanded <- expandMacros (cfgMacroEnv config) sexpr

  -- Desugar to AST
  expr <- sexprToExpr expanded

  -- Extract name from define
  case expr of
    EDefine name body -> do
      -- Alpha-rename
      renamed <- alphaRename body

      -- Closure conversion
      converted <- closureConvert renamed

      -- Generate code
      code <- generateCode name converted
      Right (name, code)

    _ -> Left $ SyntaxError "Top-level must be define" Nothing

-- Parse multiple S-expressions from source
parseMultipleSExprs :: String -> Either CompileError [SExpr]
parseMultipleSExprs = parseFromString

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

compile :: CompilerConfig -> String -> Either CompileError CodeObject
compile config source = do
  exprs <- parseFromString source
  case exprs of
    [] -> Left $ SyntaxError "Empty program" Nothing
    sexprs -> do
      let transformed = transformProgram sexprs

      expanded <- expandMacros (cfgMacroEnv config) transformed

      expr <- sexprToExpr expanded

      renamed <- alphaRename expr

      converted <- closureConvert renamed

      let (mainCodeE, _) = generateCodeWithDefs "main" converted
      mainCodeE

compileWithDefs :: CompilerConfig -> String -> Either CompileError (CodeObject, Map Name CodeObject)
compileWithDefs config source = do
  exprs <- parseFromString source
  case exprs of
    [] -> Left $ SyntaxError "Empty program" Nothing
    sexprs -> do
      let transformed = transformProgram sexprs

      expanded <- expandMacros (cfgMacroEnv config) transformed

      expr <- sexprToExpr expanded

      renamed <- alphaRename expr

      converted <- closureConvert renamed

      let (mainCodeE, defs) = generateCodeWithDefs "main" converted
      mainCode <- mainCodeE
      return (mainCode, defs)

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
    isDefine (SList [SAtom (ASymbol "define") _, SAtom (ASymbol _) _, _] _) = True
    isDefine (SList (SAtom (ASymbol "define") _ : SList (SAtom (ASymbol _) _ : _) _ : _ : _) _) = True
    isDefine _ = False

    extractBinding (SList (SAtom (ASymbol "define") _ : SAtom (ASymbol name) _ : [expr]) _) =
      (name, expr)
    extractBinding (SList (SAtom (ASymbol "define") _ : SList (SAtom (ASymbol name) _ : params) _ : body : _) loc) =
      (name, SList [SAtom (ASymbol "lambda") Nothing, SList params Nothing, body] loc)
    extractBinding _ = error "Invalid define (unreachable)"

    toBinding (name, expr) =
      SList [SAtom (ASymbol name) Nothing, expr] Nothing

compileProgram :: CompilerConfig -> String -> Either CompileError (Map.Map Name CodeObject)
compileProgram config source = do
  sexprs <- parseMultipleSExprs source

  codeObjects <- mapM (compileTopLevel config) sexprs

  Right $ Map.fromList codeObjects

compileTopLevel :: CompilerConfig -> SExpr -> Either CompileError (Name, CodeObject)
compileTopLevel config sexpr = do
  expanded <- expandMacros (cfgMacroEnv config) sexpr

  expr <- sexprToExpr expanded

  case expr of
    EDefine name body -> do
      renamed <- alphaRename body

      converted <- closureConvert renamed

      code <- generateCode name converted
      Right (name, code)

    _ -> Left $ SyntaxError "Top-level must be define" Nothing

parseMultipleSExprs :: String -> Either CompileError [SExpr]
parseMultipleSExprs = parseFromString

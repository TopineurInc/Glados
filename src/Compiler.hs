{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Compiler - Topineur compilation pipeline
-}

module Compiler
  ( compile
  , compileWithDefs
  , compileProgram
  , CompilerConfig(..)
  , defaultConfig
  ) where

import AST
import TopineurParser (parseTopineur)
import Text.Parsec (ParseError)
import ObjectDesugar
import Desugar
import AlphaRename
import ClosureConversion
import CodeGen
import qualified Data.Map as Map
import Data.Map (Map)

data CompilerConfig = CompilerConfig
  { cfgTCO :: Bool  -- Tail call optimization enabled
  , cfgDebug :: Bool
  , cfgTypeCheck :: Bool  -- Enable type checking (disabled for now)
  , cfgEffectCheck :: Bool  -- Enable effect checking (disabled for now)
  , cfgLinearityCheck :: Bool  -- Enable linearity checking (disabled for now)
  }

defaultConfig :: CompilerConfig
defaultConfig = CompilerConfig
  { cfgTCO = True
  , cfgDebug = False
  , cfgTypeCheck = False  -- Disabled for now
  , cfgEffectCheck = False  -- Disabled for now
  , cfgLinearityCheck = False  -- Disabled for now
  }

-- Convert ParseError to CompileError
convertParseError :: ParseError -> CompileError
convertParseError err = ParseError (show err) Nothing

-- Compile Topineur source code to bytecode
compile :: CompilerConfig -> String -> Either CompileError CodeObject
compile config source = do
  -- Phase 1: Parse Topineur source
  topModule <- case parseTopineur source of
    Left err -> Left (convertParseError err)
    Right exprs -> Right exprs

  -- Phase 5: Object desugaring
  desugared <- desugarObjects topModule

  -- Phase 6: Convert to single expression and auto-call main if present
  let expr = case desugared of
        [] -> EInt 0
        [EDefine "main" body] ->
          -- Single main function: define and call it
          case body of
            ELambda [] bodyExpr ->
              ELet "main" Nothing bodyExpr (EVar "main")
            _ ->
              ELet "main" Nothing body (EApp (EVar "main") [])
        [e] -> e
        es ->
          -- Multiple definitions: chain them with let bindings and call main
          let hasMain = any isMainDef es
              isMainDef (EDefine "main" _) = True
              isMainDef _ = False
              buildLetChain [] =
                -- Look for main and call it, or return 0
                if hasMain then EApp (EVar "main") [] else EInt 0
              buildLetChain (EDefine name body : rest) =
                ELet name Nothing body (buildLetChain rest)
              buildLetChain (expr : rest) =
                -- Non-define expression: execute and continue
                ELet "_tmp" Nothing expr (buildLetChain rest)
          in buildLetChain es

  -- Phase 7: Desugar standard constructs
  coreExpr <- desugarToCore expr

  -- Phase 8: Alpha renaming
  renamed <- alphaRename coreExpr

  -- Phase 9: Closure conversion
  converted <- closureConvert renamed

  -- Phase 10: Code generation
  let (mainCodeE, _) = generateCodeWithDefs "main" converted
  mainCodeE

-- Compile with nested definitions
compileWithDefs :: CompilerConfig -> String -> Either CompileError (CodeObject, Map Name CodeObject)
compileWithDefs config source = do
  -- Phase 1: Parse
  topModule <- case parseTopineur source of
    Left err -> Left (convertParseError err)
    Right exprs -> Right exprs

  -- Phase 5: Object desugaring
  desugared <- desugarObjects topModule

  -- Phase 6: Convert to single expression and auto-call main if present
  let expr = case desugared of
        [] -> EInt 0
        [EDefine "main" body] ->
          -- Single main function: define and call it
          case body of
            ELambda [] bodyExpr ->
              ELet "main" Nothing bodyExpr (EVar "main")
            _ ->
              ELet "main" Nothing body (EApp (EVar "main") [])
        [e] -> e
        es ->
          -- Multiple definitions: chain them with let bindings and call main
          let hasMain = any isMainDef es
              isMainDef (EDefine "main" _) = True
              isMainDef _ = False
              buildLetChain [] =
                -- Look for main and call it, or return 0
                if hasMain then EApp (EVar "main") [] else EInt 0
              buildLetChain (EDefine name body : rest) =
                ELet name Nothing body (buildLetChain rest)
              buildLetChain (expr : rest) =
                -- Non-define expression: execute and continue
                ELet "_tmp" Nothing expr (buildLetChain rest)
          in buildLetChain es

  -- Phase 7-10: Desugar, rename, convert, generate
  coreExpr <- desugarToCore expr
  renamed <- alphaRename coreExpr
  converted <- closureConvert renamed

  let (mainCodeE, defs) = generateCodeWithDefs "main" converted
  mainCode <- mainCodeE
  return (mainCode, defs)

-- Compile a program with multiple top-level definitions
compileProgram :: CompilerConfig -> String -> Either CompileError (Map.Map Name CodeObject)
compileProgram config source = do
  topModule <- case parseTopineur source of
    Left err -> Left (convertParseError err)
    Right exprs -> Right exprs

  -- Desugar and compile each top-level definition
  desugared <- desugarObjects topModule
  codeObjects <- mapM (compileTopLevel config) desugared

  Right $ Map.fromList codeObjects

-- Compile a single top-level definition
compileTopLevel :: CompilerConfig -> Expr -> Either CompileError (Name, CodeObject)
compileTopLevel config expr = do
  coreExpr <- desugarToCore expr

  case coreExpr of
    EDefine name body -> do
      renamed <- alphaRename body
      converted <- closureConvert renamed
      code <- generateCode name converted
      Right (name, code)
    _ -> Left $ SyntaxError "Top-level must be define" Nothing

-- Desugar standard constructs (from the old Desugar module)
-- This handles remaining constructs after object desugaring
desugarToCore :: Expr -> Either CompileError Expr
desugarToCore = sexprToExpr . exprToSExpr

-- Temporary conversion functions (simplified)
exprToSExpr :: Expr -> SExpr
exprToSExpr (EInt i) = SAtom (AInteger i) Nothing
exprToSExpr (EFloat f) = SAtom (AFloat f) Nothing
exprToSExpr (EBool b) = SAtom (ABool b) Nothing
exprToSExpr (EString s) = SAtom (AString s) Nothing
exprToSExpr (EVar n) = SAtom (ASymbol n) Nothing
exprToSExpr (EList exprs) = SList (map exprToSExpr exprs) Nothing
exprToSExpr (ELambda params body) =
  SList [SAtom (ASymbol "lambda") Nothing,
         SList (map (\p -> SAtom (ASymbol p) Nothing) params) Nothing,
         exprToSExpr body] Nothing
exprToSExpr (EDefine name expr) =
  SList [SAtom (ASymbol "define") Nothing,
         SAtom (ASymbol name) Nothing,
         exprToSExpr expr] Nothing
exprToSExpr (EIf cond thenE elseE) =
  SList [SAtom (ASymbol "if") Nothing,
         exprToSExpr cond,
         exprToSExpr thenE,
         exprToSExpr elseE] Nothing
exprToSExpr (EApp func args) =
  SList (exprToSExpr func : map exprToSExpr args) Nothing
exprToSExpr (EQuote s) = SList [SAtom (ASymbol "quote") Nothing, s] Nothing
exprToSExpr (ELet name _ valExpr body) =
  SList [SAtom (ASymbol "let") Nothing,
         SList [SList [SAtom (ASymbol name) Nothing, exprToSExpr valExpr] Nothing] Nothing,
         exprToSExpr body] Nothing
exprToSExpr (EBlock exprs) =
  case exprs of
    [] -> exprToSExpr (EInt 0)
    [e] -> exprToSExpr e
    es -> SList (SAtom (ASymbol "begin") Nothing : map exprToSExpr es) Nothing
-- Topineur constructs should be desugared already
exprToSExpr e = error $ "Unexpected Topineur construct in desugarToCore: " ++ show e

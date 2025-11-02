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
  , compileTopineur
  , compileTopineurWithDefs
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
import TopineurParser
import TopineurToAst
import qualified Data.Map as Map
import Data.Map (Map)

data CompilerConfig = CompilerConfig
  { cfgTCO :: Bool
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
    EDefine name body _ -> do
      renamed <- alphaRename body

      converted <- closureConvert renamed

      code <- generateCode name converted
      Right (name, code)

    _ -> Left $ SyntaxError "Top-level must be define" Nothing

parseMultipleSExprs :: String -> Either CompileError [SExpr]
parseMultipleSExprs = parseFromString

compileTopineur :: CompilerConfig -> String -> Either CompileError CodeObject
compileTopineur config source = do
  topineur <- parseTopineurSource source
  expr <- topineurToAst topineur

  (defs, mainExpr) <- extractTopineurProgram expr

  renamed <- alphaRename mainExpr

  converted <- closureConvert renamed

  let (mainCodeE, defsCode) = generateCodeWithDefs "main" converted
  mainCode <- mainCodeE

  defsCodeObjects <- mapM (compileDefinition config) defs

  let allDefs = Map.union (Map.fromList defsCodeObjects) defsCode
  Right mainCode

compileTopineurWithDefs :: CompilerConfig -> String -> Either CompileError (CodeObject, Map Name CodeObject)
compileTopineurWithDefs config source = do
  topineur <- parseTopineurSource source
  expr <- topineurToAst topineur

  (defs, mainExpr) <- extractTopineurProgram expr

  renamed <- alphaRename mainExpr

  converted <- closureConvert renamed

  let (mainCodeE, defsCode) = generateCodeWithDefs "main" converted
  mainCode <- mainCodeE

  defsCodeObjects <- mapM (compileDefinition config) defs

  let allDefs = Map.union (Map.fromList defsCodeObjects) defsCode
  Right (mainCode, allDefs)

extractTopineurProgram :: Expr -> Either CompileError ([Expr], Expr)
extractTopineurProgram (EList exprs) = do
  case exprs of
    [] -> Left $ SyntaxError "Empty Topineur program" Nothing
    _ ->
      let (packageAndImports, defs) = span isPackageOrImport exprs
          mainDef = findMainDef defs
          otherDefs = filter (not . isMainDef) defs
          mainExpr = case mainDef of
            Just (EDefine _ body _) ->
              case body of
                ELambda _ _ lambdaBody _ -> lambdaBody
                _ -> body
            Nothing -> EUnit
       in Right (otherDefs, mainExpr)
  where
    isPackageOrImport EPackage{} = True
    isPackageOrImport EImport{} = True
    isPackageOrImport _ = False
    isDefineExpr EDefine{} = True
    isDefineExpr _ = False
    findMainDef = foldr findOne Nothing
      where
        findOne (EDefine "main" body anns) _ = Just (EDefine "main" body anns)
        findOne _ acc = acc
    isMainDef (EDefine "main" _ _) = True
    isMainDef _ = False
extractTopineurProgram expr = Right ([], expr)

compileDefinition :: CompilerConfig -> Expr -> Either CompileError (Name, CodeObject)
compileDefinition config (EDefine name body _) = do
  renamed <- alphaRename body
  converted <- closureConvert renamed
  code <- generateCode name converted
  Right (name, code)
compileDefinition _ expr = Left $ SyntaxError "Expected definition" Nothing

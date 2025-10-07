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
compile config source =
  lowerProgram config source >>= fst . generateCodeWithDefs "main"

compileWithDefs :: CompilerConfig -> String -> Either CompileError (CodeObject, Map Name CodeObject)
compileWithDefs config source =
  lowerProgram config source >>= \converted ->
    let (mainCodeE, defs) = generateCodeWithDefs "main" converted
    in mainCodeE >>= \mainCode -> pure (mainCode, defs)

-- Transform a program with defines into a letrec
transformProgram :: [SExpr] -> SExpr
transformProgram sexprs =
  let (defineForms, rest) = span isDefineForm sexprs
      bodyExpr = buildBody rest
  in if null defineForms
       then bodyExpr
       else buildLetRec defineForms bodyExpr

isDefineForm :: SExpr -> Bool
isDefineForm (SList (SAtom (ASymbol "define") _ : _) _) = True
isDefineForm _ = False

buildBody :: [SExpr] -> SExpr
buildBody [] = SAtom (AInteger 0) Nothing
buildBody [expr] = expr
buildBody exprs =
  SList (SAtom (ASymbol "begin") Nothing : exprs) Nothing

buildLetRec :: [SExpr] -> SExpr -> SExpr
buildLetRec defineForms body =
  let bindings = map bindingFromDefine defineForms
  in SList
       [ SAtom (ASymbol "letrec") Nothing
       , SList (map bindingToSExpr bindings) Nothing
       , body
       ]

bindingFromDefine :: SExpr -> (Name, SExpr)
bindingFromDefine form =
  case form of
    SList (defineKeyword : rest) loc ->
      extractNamedBinding defineKeyword rest loc
    _ -> error "Invalid define"

extractNamedBinding :: SExpr -> [SExpr] -> Loc -> (Name, SExpr)
extractNamedBinding keyword rest loc =
  case keyword of
    SAtom (ASymbol "define") _ -> interpretDefineParts rest loc
    _ -> error "Invalid define"

interpretDefineParts :: [SExpr] -> Loc -> (Name, SExpr)
interpretDefineParts parts loc =
  case parts of
    [SAtom (ASymbol name) _, expr] -> (name, expr)
    SList (SAtom (ASymbol name) _ : params) _ : body : _ ->
      (name, lambdaFromParts params body loc)
    _ -> error "Invalid define"

lambdaFromParts :: [SExpr] -> SExpr -> Loc -> SExpr
lambdaFromParts params body loc =
  SList
    [ SAtom (ASymbol "lambda") Nothing
    , SList params Nothing
    , body
    ] loc

bindingToSExpr :: (Name, SExpr) -> SExpr
bindingToSExpr (name, expr) =
  SList
    [ SAtom (ASymbol name) Nothing
    , expr
    ] Nothing

compileProgram :: CompilerConfig -> String -> Either CompileError (Map.Map Name CodeObject)
compileProgram config source =
  parseMultipleSExprs source
    >>= mapM (compileTopLevel config)
    >>= pure . Map.fromList

compileTopLevel :: CompilerConfig -> SExpr -> Either CompileError (Name, CodeObject)
compileTopLevel config sexpr =
  expandMacros (cfgMacroEnv config) sexpr
    >>= sexprToExpr
    >>= compileDefine

compileDefine :: Expr -> Either CompileError (Name, CodeObject)
compileDefine (EDefine name body) =
  compileDefineBody name body
compileDefine _ =
  Left $ SyntaxError "Top-level must be define" Nothing

compileDefineBody :: Name -> Expr -> Either CompileError (Name, CodeObject)
compileDefineBody name body =
  alphaRename body
    >>= closureConvert
    >>= generateCode name
    >>= attachName
  where
    attachName code = Right (name, code)

parseMultipleSExprs :: String -> Either CompileError [SExpr]
parseMultipleSExprs = parseFromString

lowerProgram :: CompilerConfig -> String -> Either CompileError Expr
lowerProgram config source =
  parseFromString source
    >>= ensureNonEmpty
    >>= expandAndConvert config

ensureNonEmpty :: [SExpr] -> Either CompileError [SExpr]
ensureNonEmpty [] = Left $ SyntaxError "Empty program" Nothing
ensureNonEmpty sexprs = Right sexprs

expandAndConvert :: CompilerConfig -> [SExpr] -> Either CompileError Expr
expandAndConvert config sexprs =
  let transformed = transformProgram sexprs
  in expandMacros (cfgMacroEnv config) transformed
       >>= convertExpr

convertExpr :: SExpr -> Either CompileError Expr
convertExpr expr =
  sexprToExpr expr >>= alphaRename >>= closureConvert

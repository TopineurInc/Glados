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
  , compileTopineurFile
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
import TypeChecker
import TopineurParser
import TopineurToAst
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as Vector
import Data.Maybe (mapMaybe)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldl')
import qualified Data.Set as Set
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (takeDirectory, (</>), joinPath)

data CompilerConfig = CompilerConfig
  { cfgTCO :: Bool
  , cfgDebug :: Bool
  , cfgTypeCheck :: Bool
  , cfgMacroEnv :: MacroEnv
  , cfgModulePaths :: [FilePath]
  }

defaultConfig :: CompilerConfig
defaultConfig = CompilerConfig
  { cfgTCO = True
  , cfgDebug = False
  , cfgTypeCheck = False  -- Disabled by default for backward compatibility
  , cfgMacroEnv = defaultMacroEnv
  , cfgModulePaths = ["stdlib"]
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

      -- Optional type checking
      if cfgTypeCheck config
        then case typeCheck emptyTypeEnv converted of
          Left typeErr -> Left $ SyntaxError ("Type error: " ++ show typeErr) Nothing
          Right _ -> return ()
        else return ()

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

  (_imports, defs, mainExpr) <- extractTopineurProgram expr

  renamed <- alphaRename mainExpr

  converted <- closureConvert renamed

  if cfgTypeCheck config
    then case typeCheck emptyTypeEnv converted of
      Left typeErr -> Left $ SyntaxError ("Type error: " ++ show typeErr) Nothing
      Right _ -> return ()
    else return ()

  let (mainCodeE, _defsCode) = generateCodeWithDefs "main" converted
  mainCode <- mainCodeE

  _defsCodeObjects <- mapM (compileDefinition config) defs

  Right mainCode

compileTopineurWithDefs :: CompilerConfig -> String -> Either CompileError (CodeObject, Map Name CodeObject)
compileTopineurWithDefs config source = do
  topineur <- parseTopineurSource source
  expr <- topineurToAst topineur

  (_imports, defs, mainExpr) <- extractTopineurProgram expr

  renamed <- alphaRename mainExpr

  converted <- closureConvert renamed

  -- Optional type checking
  if cfgTypeCheck config
    then case typeCheck emptyTypeEnv converted of
      Left typeErr -> Left $ SyntaxError ("Type error: " ++ show typeErr) Nothing
      Right _ -> return ()
    else return ()

  let (mainCodeE, defsCode) = generateCodeWithDefs "main" converted
  mainCode <- mainCodeE

  defsCodeObjects <- mapM (compileDefinition config) defs

  -- Extract method code objects from object declarations
  let methodObjs = concatMap extractMethods defs
        where
          extractMethods (EObjectDecl typeName fields methods) =
            let expr = EObjectDecl typeName fields methods
                (_mainCodeE, methodMap) = generateCodeWithDefs typeName expr
            in Map.toList methodMap
          extractMethods _ = []

  let allDefs = Map.unions [Map.fromList defsCodeObjects, Map.fromList methodObjs, defsCode]
  Right (mainCode, allDefs)

extractTopineurProgram :: Expr -> Either CompileError ([Name], [Expr], Expr)
extractTopineurProgram (EList exprs) = do
  case exprs of
    [] -> Left $ SyntaxError "Empty Topineur program" Nothing
    _ ->
      let (pkgAndImports, defs) = span isPackageOrImport exprs
          imports = mapMaybe extractImport pkgAndImports
          mainDef = findMainDef defs
          otherDefs = filter (not . isMainDef) defs
          mainExpr = case mainDef of
            Just (EDefine _ body _) ->
              case body of
                ELambda _ _ lambdaBody _ -> lambdaBody
                _ -> body
            Just _ -> EUnit
            Nothing -> EUnit
       in Right (imports, otherDefs, mainExpr)
  where
    isPackageOrImport EPackage{} = True
    isPackageOrImport EImport{} = True
    isPackageOrImport _ = False
    extractImport (EImport name) = Just name
    extractImport _ = Nothing
    findMainDef = foldr findOne Nothing
      where
        findOne (EDefine "main" body anns) _ = Just (EDefine "main" body anns)
        findOne _ acc = acc
    isMainDef (EDefine "main" _ _) = True
    isMainDef _ = False
extractTopineurProgram expr = Right ([], [], expr)

compileDefinition :: CompilerConfig -> Expr -> Either CompileError (Name, CodeObject)
compileDefinition _config (EDefine name body _) = do
  case body of
    ELambda params _retType lambdaBody _ann -> do
      renamed <- alphaRename lambdaBody
      converted <- closureConvert renamed
      -- Use compileLambda to properly compile the function with parameters
      let paramNames = map fst params
          initialState = emptyCodeGenState
          (codeObj, _finalState) = runCodeGen (compileLambda name paramNames converted) initialState
      Right (name, codeObj)
    _ -> do
      renamed <- alphaRename body
      converted <- closureConvert renamed
      code <- generateCode name converted
      Right (name, code)

compileDefinition _config (EObjectDecl typeName fields methods) = do
  -- Compile the object declaration to generate method code objects
  -- We'll compile each method as a separate function
  let expr = EObjectDecl typeName fields methods
      (_mainCodeE, methodObjs) = generateCodeWithDefs typeName expr

  -- For now, just return the type's empty code object
  -- The methods will be handled separately
  Right (typeName, CodeObject
    { coName = typeName
    , coArity = 0
    , coMaxLocals = 0
    , coConsts = Vector.empty
    , coInstrs = Vector.empty
    , coLabelMap = Map.empty
    })

compileDefinition _ _expr = Left $ SyntaxError "Expected definition" Nothing

-- Module loading ----------------------------------------------------------------

type ModuleCache = Map FilePath (Map Name CodeObject)

data ModuleState = ModuleState ModuleCache (Set.Set FilePath)

emptyModuleState :: ModuleState
emptyModuleState = ModuleState Map.empty Set.empty

compileTopineurFile :: CompilerConfig -> FilePath -> IO (Either CompileError (CodeObject, Map Name CodeObject))
compileTopineurFile config entryPath =
  runExceptT $ evalStateT (compileEntry entryPath) emptyModuleState
  where
    compileEntry :: FilePath -> StateT ModuleState (ExceptT CompileError IO) (CodeObject, Map Name CodeObject)
    compileEntry path = do
      source <- liftIO (readFile path)
      imports <- liftEitherState (collectTopineurImports source)
      let searchPaths = takeDirectory path : cfgModulePaths config
      importDefs <- loadImports searchPaths imports
      (mainCode, defs) <- liftEitherState (compileTopineurWithDefs config source)
      let combinedDefs = Map.union defs importDefs
      pure (mainCode, combinedDefs)

    loadImports :: [FilePath] -> [Name] -> StateT ModuleState (ExceptT CompileError IO) (Map Name CodeObject)
    loadImports searchPaths = fmap (foldl' (flip Map.union) Map.empty) . mapM (loadModule searchPaths)

    loadModule :: [FilePath] -> Name -> StateT ModuleState (ExceptT CompileError IO) (Map Name CodeObject)
    loadModule searchPaths moduleName = do
      resolvedPath <- resolveModulePath searchPaths moduleName
      ModuleState cache inProgress <- get
      let alreadyCompiled = Map.lookup resolvedPath cache
      case alreadyCompiled of
        Just defs -> pure defs
        Nothing -> do
          when (Set.member resolvedPath inProgress) $
            lift $ throwError $ SyntaxError ("Cyclic import detected for module: " ++ moduleName) Nothing
          put $ ModuleState cache (Set.insert resolvedPath inProgress)
          source <- liftIO (readFile resolvedPath)
          imports <- liftEitherState (collectTopineurImports source)
          let nextSearchPaths = takeDirectory resolvedPath : cfgModulePaths config
          depsDefs <- loadImports nextSearchPaths imports
          (_, moduleDefs) <- liftEitherState (compileTopineurWithDefs config source)
          let finalDefs = Map.union moduleDefs depsDefs
          ModuleState cache' inProgress' <- get
          let newCache = Map.insert resolvedPath finalDefs cache'
              newInProgress = Set.delete resolvedPath inProgress'
          put $ ModuleState newCache newInProgress
          pure finalDefs

    resolveModulePath :: [FilePath] -> Name -> StateT ModuleState (ExceptT CompileError IO) FilePath
    resolveModulePath [] moduleName =
      lift $ throwError $ SyntaxError ("Unable to resolve import: " ++ moduleName) Nothing
    resolveModulePath (p:ps) moduleName = do
      let relativePath = joinPath (splitModuleName moduleName) ++ ".top"
          candidate = p </> relativePath
      exists <- liftIO (doesFileExist candidate)
      if exists
        then liftIO (canonicalizePath candidate)
        else resolveModulePath ps moduleName

    liftEitherState :: Either CompileError a -> StateT ModuleState (ExceptT CompileError IO) a
    liftEitherState = either (lift . throwError) pure

collectTopineurImports :: String -> Either CompileError [Name]
collectTopineurImports source = do
  topineur <- parseTopineurSource source
  expr <- topineurToAst topineur
  (imports, _, _) <- extractTopineurProgram expr
  pure imports

splitModuleName :: String -> [String]
splitModuleName [] = []
splitModuleName name =
  let (chunk, rest) = break (=='.') name
  in case rest of
       [] -> [chunk | not (null chunk)]
       (_:xs) -> chunk : splitModuleName xs

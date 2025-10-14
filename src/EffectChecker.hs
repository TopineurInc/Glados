{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- EffectChecker - Effect system verification for Topineur
-}

module EffectChecker
  ( checkEffects
  , inferEffects
  , EffectEnv
  , EffectError(..)
  , emptyEffectEnv
  ) where

import qualified AST
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Effect environment mapping functions to their effects
type EffectEnv = Map.Map AST.Name AST.EffectRow

-- Effect checking errors
data EffectError
  = EffectNotDeclared AST.Effect AST.Name
  | EffectMismatch AST.EffectRow AST.EffectRow
  | PureContextViolation AST.Name AST.EffectRow
  | UnknownEffect AST.Name
  deriving (Eq, Show)

emptyEffectEnv :: EffectEnv
emptyEffectEnv = Map.empty

-- Check if effect row is a subset of another (allows more effects)
isSubsetOf :: AST.EffectRow -> AST.EffectRow -> Bool
isSubsetOf (AST.EffectRow actual) (AST.EffectRow declared) =
  all (`elem` declared) actual

-- Merge two effect rows
mergeEffects :: AST.EffectRow -> AST.EffectRow -> AST.EffectRow
mergeEffects (AST.EffectRow e1) (AST.EffectRow e2) =
  AST.EffectRow $ Set.toList $ Set.fromList (e1 ++ e2)

-- Check if a function call respects effect constraints
checkCall :: EffectEnv -> AST.Name -> AST.EffectRow -> Either EffectError ()
checkCall env funcName declaredEffects =
  case Map.lookup funcName env of
    Nothing -> Right ()  -- Unknown function, assume pure
    Just funcEffects ->
      if funcEffects `isSubsetOf` declaredEffects
      then Right ()
      else Left $ EffectMismatch funcEffects declaredEffects

-- Infer effects from expression
inferEffects :: EffectEnv -> AST.Expr -> AST.EffectRow
inferEffects env expr = case expr of
  -- Pure expressions
  AST.EInt _ -> AST.EffectRow []
  AST.EFloat _ -> AST.EffectRow []
  AST.EBool _ -> AST.EffectRow []
  AST.EString _ -> AST.EffectRow []

  -- Variable lookup (pure)
  AST.EVar name ->
    case Map.lookup name env of
      Just effects -> effects
      Nothing -> AST.EffectRow []

  -- Lambda (effects determined by body)
  AST.ELambda params body ->
    inferEffects env body

  -- Function application
  AST.EApp func args ->
    let funcEffects = inferEffects env func
        argEffects = map (inferEffects env) args
    in foldr mergeEffects funcEffects argEffects

  -- If expression
  AST.EIf cond thenE elseE ->
    let condEffects = inferEffects env cond
        thenEffects = inferEffects env thenE
        elseEffects = inferEffects env elseE
    in mergeEffects condEffects (mergeEffects thenEffects elseEffects)

  -- Let binding
  AST.ELet name _ value body ->
    let valueEffects = inferEffects env value
        bodyEffects = inferEffects env body
    in mergeEffects valueEffects bodyEffects

  -- List
  AST.EList exprs ->
    foldr mergeEffects (AST.EffectRow []) (map (inferEffects env) exprs)

  -- Object literal (pure)
  AST.EObjectLit _ fields ->
    foldr mergeEffects (AST.EffectRow []) (map (inferEffects env . snd) fields)

  -- Method call
  AST.EMethodCall obj _ args ->
    let objEffects = inferEffects env obj
        argEffects = map (inferEffects env) args
    in foldr mergeEffects objEffects argEffects

  -- Field access (pure)
  AST.EFieldAccess obj _ ->
    inferEffects env obj

  -- Type annotation (transparent to effects)
  AST.ETyped expr _ ->
    inferEffects env expr

  -- Linear binding
  AST.ELinearBind _ value body ->
    mergeEffects (inferEffects env value) (inferEffects env body)

  -- Match expression
  AST.EMatch scrutinee cases ->
    let scrutineeEffects = inferEffects env scrutinee
        caseEffects = map (\(_, e) -> inferEffects env e) cases
    in foldr mergeEffects scrutineeEffects caseEffects

  -- Block expression
  AST.EBlock exprs ->
    foldr mergeEffects (AST.EffectRow []) (map (inferEffects env) exprs)

  -- Definitions (pure at definition time)
  AST.EObjectDef _ -> AST.EffectRow []
  AST.ETraitDef _ -> AST.EffectRow []
  AST.ETraitImpl _ -> AST.EffectRow []
  AST.EDefine _ expr -> inferEffects env expr

  -- Quote (pure)
  AST.EQuote _ -> AST.EffectRow []

-- Check that function respects its declared effects
checkEffects :: EffectEnv -> AST.MethodDef -> Either EffectError ()
checkEffects env methodDef =
  let declaredEffects = AST.methEffects methodDef
      actualEffects = inferEffects env (AST.methBody methodDef)
  in if actualEffects `isSubsetOf` declaredEffects
     then Right ()
     else Left $ EffectMismatch actualEffects declaredEffects

-- Builtin effects environment
builtinEffects :: EffectEnv
builtinEffects = Map.fromList
  [ ("println", AST.EffectRow [AST.EffIO])
  , ("print", AST.EffectRow [AST.EffIO])
  , ("readLine", AST.EffectRow [AST.EffIO])
  , ("readFile", AST.EffectRow [AST.EffIO])
  , ("writeFile", AST.EffectRow [AST.EffIO])
  , ("+", AST.EffectRow [])
  , ("-", AST.EffectRow [])
  , ("*", AST.EffectRow [])
  , ("div", AST.EffectRow [])
  , ("mod", AST.EffectRow [])
  , ("<", AST.EffectRow [])
  , (">", AST.EffectRow [])
  , ("eq?", AST.EffectRow [])
  , ("and", AST.EffectRow [])
  , ("or", AST.EffectRow [])
  , ("not", AST.EffectRow [])
  , ("string-append", AST.EffectRow [])
  ]

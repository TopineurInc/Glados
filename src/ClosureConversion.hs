{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ClosureConversion
-}

module ClosureConversion
  ( closureConvert
  , ClosureInfo(..)
  ) where

import AST
import qualified Data.Set as Set

data ClosureInfo = ClosureInfo
  { ciFreeVars :: [Name]
  , ciCode :: Expr
  } deriving (Eq, Show)

type Env = Set.Set Name

closureConvert :: Expr -> Either CompileError Expr
closureConvert expr = Right $ convertExpr Set.empty expr

convertExpr :: Env -> Expr -> Expr
convertExpr _ e@(EInt _) = e
convertExpr _ e@(EFloat _) = e
convertExpr _ e@(EBool _) = e
convertExpr _ e@(EString _) = e
convertExpr _ e@EUnit = e
convertExpr _ e@(EVar _) = e
convertExpr _ e@(EQuote _) = e

convertExpr env (EList exprs) =
  EList (map (convertExpr env) exprs)

convertExpr env (ELambda params retType body ann) =
  let paramNames = map fst params
      env' = env `Set.union` Set.fromList paramNames
      body' = convertExpr env' body
      -- Note: free variables analysis done but not yet used in current implementation
      _free = getFreeVars (Set.fromList paramNames) body'
  in ELambda params retType body' ann

convertExpr env (EDefine name expr ann) =
  let env' = Set.insert name env
  in EDefine name (convertExpr env' expr) ann

convertExpr env (EIf cond thenE elseE) =
  EIf (convertExpr env cond) (convertExpr env thenE) (convertExpr env elseE)

convertExpr env (EApp func args) =
  EApp (convertExpr env func) (map (convertExpr env) args)

convertExpr env (EWhile cond body) =
  EWhile (convertExpr env cond) (convertExpr env body)

convertExpr env (EFor var start end body) =
  let env' = Set.insert var env
  in EFor var (convertExpr env start) (convertExpr env end) (convertExpr env' body)

convertExpr env (ERange start end) =
  ERange (convertExpr env start) (convertExpr env end)

convertExpr env (EReturn expr) =
  EReturn (convertExpr env expr)

convertExpr env (EBinOp op left right) =
  EBinOp op (convertExpr env left) (convertExpr env right)

convertExpr env (EUnOp op expr) =
  EUnOp op (convertExpr env expr)

convertExpr env (ETuple exprs) =
  ETuple (map (convertExpr env) exprs)

convertExpr env (ETupleDestruct names bindExpr body) =
  let env' = env `Set.union` Set.fromList names
  in ETupleDestruct names (convertExpr env bindExpr) (convertExpr env' body)

convertExpr env (EListLiteral exprs mType) =
  EListLiteral (map (convertExpr env) exprs) mType

convertExpr env (EIndex expr idx) =
  EIndex (convertExpr env expr) (convertExpr env idx)

convertExpr env (EIndexSet container idx value) =
  EIndexSet (convertExpr env container) (convertExpr env idx) (convertExpr env value)

convertExpr env (EAssign name expr) =
  EAssign name (convertExpr env expr)

convertExpr env (EObjectDecl name fields methods) =
  let fields' = map (convertField env) fields
      methods' = map (convertMethod env) methods
  in EObjectDecl name fields' methods'

convertExpr env (EObjectInst name fieldInits) =
  EObjectInst name (map (\(n, e) -> (n, convertExpr env e)) fieldInits)

convertExpr env (EMemberAccess expr member) =
  EMemberAccess (convertExpr env expr) member

convertExpr _ e@(EPackage _) = e

convertExpr _ e@(EImport _) = e

convertField :: Env -> Field -> Field
convertField env (Field name ftype mExpr) =
  Field name ftype (fmap (convertExpr env) mExpr)

convertMethod :: Env -> Method -> Method
convertMethod env (Method name params retType body) =
  let paramNames = map fst params
      env' = env `Set.union` Set.fromList paramNames
      body' = convertExpr env' body
  in Method name params retType body'

getFreeVars :: Set.Set Name -> Expr -> Set.Set Name
getFreeVars bound (EVar name)
  | Set.member name bound = Set.empty
  | otherwise = Set.singleton name
getFreeVars _ (EInt _) = Set.empty
getFreeVars _ (EFloat _) = Set.empty
getFreeVars _ (EBool _) = Set.empty
getFreeVars _ (EString _) = Set.empty
getFreeVars _ EUnit = Set.empty
getFreeVars _ (EQuote _) = Set.empty

getFreeVars bound (EList exprs) =
  Set.unions (map (getFreeVars bound) exprs)

getFreeVars bound (ELambda params _retType body _ann) =
  let paramNames = map fst params
      bound' = bound `Set.union` Set.fromList paramNames
  in getFreeVars bound' body

getFreeVars bound (EDefine name expr _ann) =
  let bound' = Set.insert name bound
  in getFreeVars bound' expr

getFreeVars bound (EIf cond thenE elseE) =
  getFreeVars bound cond `Set.union`
  getFreeVars bound thenE `Set.union`
  getFreeVars bound elseE

getFreeVars bound (EApp func args) =
  getFreeVars bound func `Set.union`
  Set.unions (map (getFreeVars bound) args)

getFreeVars bound (EWhile cond body) =
  getFreeVars bound cond `Set.union` getFreeVars bound body

getFreeVars bound (EFor var start end body) =
  let bound' = Set.insert var bound
  in getFreeVars bound start `Set.union`
     getFreeVars bound end `Set.union`
     getFreeVars bound' body

getFreeVars bound (ERange start end) =
  getFreeVars bound start `Set.union` getFreeVars bound end

getFreeVars bound (EReturn expr) =
  getFreeVars bound expr

getFreeVars bound (EBinOp _op left right) =
  getFreeVars bound left `Set.union` getFreeVars bound right

getFreeVars bound (EUnOp _op expr) =
  getFreeVars bound expr

getFreeVars bound (ETuple exprs) =
  Set.unions (map (getFreeVars bound) exprs)

getFreeVars bound (ETupleDestruct names bindExpr body) =
  let bound' = bound `Set.union` Set.fromList names
  in getFreeVars bound bindExpr `Set.union` getFreeVars bound' body

getFreeVars bound (EListLiteral exprs _mType) =
  Set.unions (map (getFreeVars bound) exprs)

getFreeVars bound (EIndex expr idx) =
  getFreeVars bound expr `Set.union` getFreeVars bound idx

getFreeVars bound (EIndexSet container idx value) =
  getFreeVars bound container `Set.union` getFreeVars bound idx `Set.union` getFreeVars bound value

getFreeVars bound (EAssign _name expr) =
  getFreeVars bound expr

getFreeVars bound (EObjectDecl _name fields methods) =
  let fieldFreeVars = Set.unions (map (getFreeVarsField bound) fields)
      methodFreeVars = Set.unions (map (getFreeVarsMethod bound) methods)
  in fieldFreeVars `Set.union` methodFreeVars

getFreeVars bound (EObjectInst _name fieldInits) =
  Set.unions (map (\(_n, e) -> getFreeVars bound e) fieldInits)

getFreeVars bound (EMemberAccess expr _member) =
  getFreeVars bound expr

getFreeVars _ (EPackage _) = Set.empty

getFreeVars _ (EImport _) = Set.empty

getFreeVarsField :: Set.Set Name -> Field -> Set.Set Name
getFreeVarsField bound (Field _name _ftype mExpr) =
  case mExpr of
    Nothing -> Set.empty
    Just expr -> getFreeVars bound expr

getFreeVarsMethod :: Set.Set Name -> Method -> Set.Set Name
getFreeVarsMethod bound (Method _name params _retType body) =
  let paramNames = map fst params
      bound' = bound `Set.union` Set.fromList paramNames
  in getFreeVars bound' body

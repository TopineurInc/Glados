{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/topineur/TopineurToAst.hs
-}

{-# LANGUAGE LambdaCase #-}

module TopineurToAst
  ( topineurToAst
  ) where

import AST
import Control.Monad ((>=>))
import qualified TopineurParserTypes as T

topineurToAst :: T.Topineur -> Either CompileError Expr
topineurToAst (T.Topineur pkg imps decls) = do
  pkgExpr <- Right $ EPackage pkg
  impExprs <- mapM (\imp -> Right $ EImport imp) imps
  declExprs <- mapM declToExpr decls
  Right $ EList (pkgExpr : impExprs ++ declExprs)

declToExpr :: T.Decl -> Either CompileError Expr
declToExpr = \case
  T.DLet _loc pat expr -> do
    name <- patternToName pat
    expr' <- expressionToExpr expr
    Right $ EDefine name expr' []

  T.DFunc _loc decorators name params retType block -> do
    params' <- mapM paramToTuple params
    retType' <- mapM typeAnnToType retType
    body <- blockToExpr block
    Right $ EDefine name (ELambda params' retType' body decorators') []
    where
      decorators' = map decoratorToAnnotation decorators

  T.DObjectType _loc name members -> do
    (fields, methods) <- partitionMembers members
    fields' <- mapM memberFieldToField fields
    methods' <- mapM memberMethodToMethod methods
    Right $ EObjectDecl name fields' methods'

decoratorToAnnotation :: T.Decorator -> Annotation
decoratorToAnnotation (T.Decorator _ name _args) =
  case name of
    "cache" -> Cache
    other -> Custom other

partitionMembers :: [T.ObjMember] -> Either CompileError ([T.ObjMember], [T.ObjMember])
partitionMembers members =
  let (fields, methods) = foldr partitionOne ([], []) members
   in Right (fields, methods)
  where
    partitionOne (T.OMField loc name typeAnn initVal) (fs, ms) = (T.OMField loc name typeAnn initVal : fs, ms)
    partitionOne (T.OMFunc func) (fs, ms) = (fs, T.OMFunc func : ms)

memberFieldToField :: T.ObjMember -> Either CompileError Field
memberFieldToField (T.OMField _ name typeAnn initVal) = do
  type' <- typeAnnToType typeAnn
  initVal' <- mapM expressionToExpr initVal
  Right $ Field name type' initVal'
memberFieldToField _ = Left $ SyntaxError "Expected field member" Nothing

memberMethodToMethod :: T.ObjMember -> Either CompileError Method
memberMethodToMethod (T.OMFunc (T.DFunc _ _ name params retType block)) = do
  params' <- mapM paramToTuple params
  retType' <- mapM typeAnnToType retType
  body <- blockToExpr block
  Right $ Method name params' (maybe TUnit id retType') body
memberMethodToMethod _ = Left $ SyntaxError "Expected method member" Nothing

paramToTuple :: T.Param -> Either CompileError (Name, Maybe Type)
paramToTuple = \case
  T.PVar _ name typeAnn -> do
    type' <- typeAnnToType typeAnn
    Right (name, Just type')
  T.PTuple _ params -> do
    types <- mapM (paramToTypeAnn >=> typeAnnToType) params
    Right ("", Just $ TTuple types)

paramToTypeAnn :: T.Param -> Either CompileError T.TypeAnn
paramToTypeAnn (T.PVar _ _ typeAnn) = Right typeAnn
paramToTypeAnn (T.PTuple loc params) = do
  typeAnns <- mapM paramToTypeAnn params
  Right $ T.TTuple loc typeAnns

patternToName :: T.Pattern -> Either CompileError Name
patternToName = \case
  T.PVarPat _ name _ -> Right name
  T.PTuplePat _ _pats ->
    Left $ SyntaxError "Tuple pattern destructuring not yet fully supported" Nothing

blockToExpr :: T.Block -> Either CompileError Expr
blockToExpr (T.Block _ stmts) = do
  exprs <- mapM stmtToExpr stmts
  case exprs of
    [] -> Right EUnit
    [e] -> Right e
    es -> Right $ EList es

stmtToExpr :: T.Stmt -> Either CompileError Expr
stmtToExpr = \case
  T.STop _ expr -> expressionToExpr expr

  T.SLet _loc pat expr -> do
    name <- patternToName pat
    expr' <- expressionToExpr expr
    Right $ EDefine name expr' []

  T.SIf _loc cond thenStmt elseStmt -> do
    cond' <- expressionToExpr cond
    then' <- stmtToExpr thenStmt
    else' <- case elseStmt of
      Just s -> stmtToExpr s
      Nothing -> Right EUnit
    Right $ EIf cond' then' else'

  T.SWhile _loc cond stmts -> do
    cond' <- expressionToExpr cond
    body <- blockToExpr (T.Block _loc stmts)
    Right $ EWhile cond' body

  T.SFor _loc var range stmts -> do
    range' <- rangeToExpr range
    body <- blockToExpr (T.Block _loc stmts)
    case range' of
      ERange start end -> Right $ EFor var start end body
      _ -> Left $ SyntaxError "Invalid range expression" Nothing

  T.SAssign _loc lval expr -> do
    expr' <- expressionToExpr expr
    name <- lvalueToName lval
    Right $ EAssign name expr'

  T.SExpression _loc expr -> expressionToExpr expr

rangeToExpr :: T.Range -> Either CompileError Expr
rangeToExpr (T.Range start end) = do
  start' <- expressionToExpr start
  end' <- expressionToExpr end
  Right $ ERange start' end'

lvalueToName :: T.LValue -> Either CompileError Name
lvalueToName = \case
  T.LVar _ name -> Right name
  T.LMember _ _expr name -> Right name

expressionToExpr :: T.Expression -> Either CompileError Expr
expressionToExpr = \case
  T.EVar _ name -> Right $ EVar name

  T.ESelf _ -> Left $ SyntaxError "self keyword not yet supported" Nothing

  T.EInt _ n -> Right $ EInt n

  T.EFloat _ f -> Right $ EFloat f

  T.EBool _ b -> Right $ EBool b

  T.EString _ s -> Right $ EString s

  T.ETuple _ exprs -> do
    exprs' <- mapM expressionToExpr exprs
    Right $ ETuple exprs'

  T.EArray _ exprs -> do
    exprs' <- mapM expressionToExpr exprs
    Right $ EListLiteral exprs' Nothing

  T.EObject _ name fields -> do
    fields' <- mapM fieldAssignToPair fields
    Right $ EObjectInst name fields'

  T.ECall _ func args -> do
    func' <- expressionToExpr func
    args' <- mapM expressionToExpr args
    Right $ EApp func' args'

  T.EMethodCall _ obj methodName args -> do
    obj' <- expressionToExpr obj
    args' <- mapM expressionToExpr args
    Right $ EApp (EMemberAccess obj' methodName) args'

  T.EMember _ obj name -> do
    obj' <- expressionToExpr obj
    Right $ EMemberAccess obj' name

  T.EIf _ cond thenExpr elseExpr -> do
    cond' <- expressionToExpr cond
    then' <- expressionToExpr thenExpr
    else' <- case elseExpr of
      Just e -> expressionToExpr e
      Nothing -> Right EUnit
    Right $ EIf cond' then' else'

  T.ELet _ name initExpr body -> do
    init' <- expressionToExpr initExpr
    body' <- expressionToExpr body
    Right $ EApp (ELambda [(name, Nothing)] Nothing body' []) [init']

  T.ELambda _ params retType body -> do
    params' <- mapM paramToTuple params
    retType' <- mapM typeAnnToType retType
    body' <- expressionToExpr body
    Right $ ELambda params' retType' body' []

  T.EBinOp _ op left right -> do
    left' <- expressionToExpr left
    right' <- expressionToExpr right
    binOp <- stringToBinOp op
    Right $ EBinOp binOp left' right'

  T.EParens _ expr -> expressionToExpr expr

fieldAssignToPair :: T.FieldAssign -> Either CompileError (Name, Expr)
fieldAssignToPair (T.FieldAssign _ (Left (_expr, _name)) _) =
  Left $ SyntaxError "Bitfield assignment not yet supported" Nothing
fieldAssignToPair (T.FieldAssign _ (Right name) expr) = do
  expr' <- expressionToExpr expr
  Right (name, expr')

stringToBinOp :: String -> Either CompileError BinOp
stringToBinOp = \case
  "+" -> Right Add
  "-" -> Right Sub
  "*" -> Right Mul
  "/" -> Right Div
  "%" -> Right Mod
  "&&" -> Right And
  "||" -> Right Or
  "==" -> Right Eq
  "!=" -> Right Neq
  "<" -> Right Lt
  "<=" -> Right Lte
  ">" -> Right Gt
  ">=" -> Right Gte
  "++" -> Right Concat
  op -> Left $ SyntaxError ("Unknown binary operator: " ++ op) Nothing

typeAnnToType :: T.TypeAnn -> Either CompileError Type
typeAnnToType = \case
  T.TIdent _loc name -> typeNameToType name
  T.TUpperIdent _loc name -> typeNameToType name  -- Also check upper idents for primitives
  T.TGeneric _loc name args -> do
    args' <- mapM typeAnnToType args
    case name of
      "List" -> case args' of
        [t] -> Right $ TList t
        _ -> Left $ SyntaxError "List type requires one type argument" Nothing
      "Tuple" -> Right $ TTuple args'
      _ -> Right $ TObject name
  T.TTuple _loc types -> do
    types' <- mapM typeAnnToType types
    Right $ TTuple types'

typeNameToType :: Name -> Either CompileError Type
typeNameToType = \case
  "Int" -> Right TInt
  "Float" -> Right TFloat
  "Bool" -> Right TBool
  "String" -> Right TString
  "Unit" -> Right TUnit
  name -> Right $ TObject name

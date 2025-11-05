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

  T.DObjectType _loc name _typeParams members -> do
    -- Note: type params are currently ignored in AST
    -- They could be used for type checking but not for codegen
    (fields, methods) <- partitionMembers members
    fields' <- mapM memberFieldToField fields
    methods' <- mapM memberMethodToMethod methods
    Right $ EObjectDecl name fields' methods'

  T.DExpr _loc expr -> do
    -- Top-level expression (e.g., println(...))
    expressionToExpr expr

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

-- Extract names from a pattern (for tuple destructuring)
patternToNames :: T.Pattern -> Either CompileError [Name]
patternToNames = \case
  T.PVarPat _ name _ -> Right [name]
  T.PTuplePat _ pats -> concat <$> mapM patternToNames pats

blockToExpr :: T.Block -> Either CompileError Expr
blockToExpr (T.Block _ stmts) = blockStmtsToExpr stmts
  where
    blockStmtsToExpr :: [T.Stmt] -> Either CompileError Expr
    blockStmtsToExpr [] = Right EUnit
    blockStmtsToExpr [stmt] = stmtToExpr stmt
    blockStmtsToExpr (T.SLet _loc (T.PTuplePat _ pats) expr : rest) = do
      expr' <- expressionToExpr expr
      names <- patternToNames (T.PTuplePat _loc pats)
      body <- blockStmtsToExpr rest
      Right $ ETupleDestruct names expr' body
    blockStmtsToExpr (allStmts) = do
      exprs <- mapM stmtToExpr allStmts
      case exprs of
        [] -> Right EUnit
        [e] -> Right e
        es -> Right $ EList es

stmtToExpr :: T.Stmt -> Either CompileError Expr
stmtToExpr = \case
  T.STop _ expr -> expressionToExpr expr

  T.SLet _loc pat expr -> do
    expr' <- expressionToExpr expr
    case pat of
      T.PVarPat _ name _ ->
        Right $ EDefine name expr' []
      T.PTuplePat _ _pats -> do
        -- Standalone tuple destructuring let is not yet supported
        Left $ SyntaxError "Tuple destructuring in standalone let not yet supported. Use it inside a block with a continuation." Nothing

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
    case lval of
      T.LIndex _ container idx -> do
        container' <- expressionToExpr container
        idx' <- expressionToExpr idx
        Right $ EIndexSet container' idx' expr'
      _ -> do
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
  T.LIndex _ _expr _idx -> Left $ SyntaxError "Index assignment should be handled in stmtToExpr" Nothing

expressionToExpr :: T.Expression -> Either CompileError Expr
expressionToExpr = \case
  T.EVar _ name -> Right $ EVar name

  T.ESelf _ -> Right $ EVar "self"

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

  T.EIndex _ obj idx -> do
    obj' <- expressionToExpr obj
    idx' <- expressionToExpr idx
    Right $ EIndex obj' idx'

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

  T.EUnOp _ op expr -> do
    expr' <- expressionToExpr expr
    unOp <- stringToUnOp op
    Right $ EUnOp unOp expr'

  T.EParens _ expr -> expressionToExpr expr

fieldAssignToPair :: T.FieldAssign -> Either CompileError (Name, Expr)
fieldAssignToPair (T.FieldAssign _ (Left (_expr, name)) expr) = do
  expr' <- expressionToExpr expr
  Right (name, expr')
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
  "and" -> Right And
  "or" -> Right Or
  "==" -> Right Eq
  "!=" -> Right Neq
  "<" -> Right Lt
  "<=" -> Right Lte
  ">" -> Right Gt
  ">=" -> Right Gte
  "++" -> Right Concat
  op -> Left $ SyntaxError ("Unknown binary operator: " ++ op) Nothing

stringToUnOp :: String -> Either CompileError UnOp
stringToUnOp = \case
  "not" -> Right Not
  "-" -> Right Neg
  op -> Left $ SyntaxError ("Unknown unary operator: " ++ op) Nothing

typeAnnToType :: T.TypeAnn -> Either CompileError Type
typeAnnToType = \case
  T.TIdent _loc name -> typeNameToType name
  T.TUpperIdent _loc name -> Right $ TObject name
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

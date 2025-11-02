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

-- Convert Topineur AST to AST Expr
topineurToAst :: T.Topineur -> Either CompileError Expr
topineurToAst (T.Topineur pkg imps decls) = do
  pkgExpr <- Right $ EPackage pkg
  impExprs <- mapM (\imp -> Right $ EImport imp) imps
  declExprs <- mapM declToExpr decls
  Right $ EList (pkgExpr : impExprs ++ declExprs)

-- Convert a declaration to an Expr
declToExpr :: T.Decl -> Either CompileError Expr
declToExpr = \case
  T.DLet loc pat expr -> do
    name <- patternToName pat
    expr' <- expressionToExpr expr
    Right $ EDefine name expr' []
  
  T.DFunc loc decorators name params retType block -> do
    params' <- mapM paramToTuple params
    retType' <- mapM typeAnnToType retType
    body <- blockToExpr block
    Right $ EDefine name (ELambda params' retType' body decorators') []
    where
      decorators' = map decoratorToAnnotation decorators
  
  T.DObjectType loc name members -> do
    (fields, methods) <- partitionMembers members
    fields' <- mapM memberFieldToField fields
    methods' <- mapM memberMethodToMethod methods
    Right $ EObjectDecl name fields' methods'

-- Convert a decorator to an annotation
decoratorToAnnotation :: T.Decorator -> Annotation
decoratorToAnnotation (T.Decorator _ name args) =
  case name of
    "cache" -> Cache
    other -> Custom other

-- Partition object members into fields and methods
partitionMembers :: [T.ObjMember] -> Either CompileError ([T.ObjMember], [T.ObjMember])
partitionMembers members =
  let (fields, methods) = foldr partitionOne ([], []) members
   in Right (fields, methods)
  where
    partitionOne (T.OMField loc name typeAnn initVal) (fs, ms) = (T.OMField loc name typeAnn initVal : fs, ms)
    partitionOne (T.OMFunc func) (fs, ms) = (fs, T.OMFunc func : ms)

-- Convert object member field to AST Field
memberFieldToField :: T.ObjMember -> Either CompileError Field
memberFieldToField (T.OMField _ name typeAnn initVal) = do
  type' <- typeAnnToType typeAnn
  initVal' <- mapM expressionToExpr initVal
  Right $ Field name type' initVal'
memberFieldToField _ = Left $ SyntaxError "Expected field member" Nothing

-- Convert object member method to AST Method
memberMethodToMethod :: T.ObjMember -> Either CompileError Method
memberMethodToMethod (T.OMFunc (T.DFunc _ _ name params retType block)) = do
  params' <- mapM paramToTuple params
  retType' <- mapM typeAnnToType retType
  body <- blockToExpr block
  Right $ Method name params' (maybe TUnit id retType') body
memberMethodToMethod _ = Left $ SyntaxError "Expected method member" Nothing

-- Convert a parameter to (Name, Maybe Type)
paramToTuple :: T.Param -> Either CompileError (Name, Maybe Type)
paramToTuple = \case
  T.PVar _ name typeAnn -> do
    type' <- typeAnnToType typeAnn
    Right (name, Just type')
  T.PTuple _ params -> do
    -- For tuple parameters, we need to extract them - this is complex
    -- For now, create a tuple type
    types <- mapM (paramToTypeAnn >=> typeAnnToType) params
    Right ("", Just $ TTuple types)

-- Extract TypeAnn from Param
paramToTypeAnn :: T.Param -> Either CompileError T.TypeAnn
paramToTypeAnn (T.PVar _ _ typeAnn) = Right typeAnn
paramToTypeAnn (T.PTuple loc params) = do
  typeAnns <- mapM paramToTypeAnn params
  Right $ T.TTuple loc typeAnns

-- Convert a pattern to a variable name
patternToName :: T.Pattern -> Either CompileError Name
patternToName = \case
  T.PVarPat _ name _ -> Right name
  T.PTuplePat _ pats -> 
    -- For tuple patterns, we'll use a synthetic name for now
    -- A more complete implementation would handle tuple destructuring
    Left $ SyntaxError "Tuple pattern destructuring not yet fully supported" Nothing

-- Convert a block to an Expr
blockToExpr :: T.Block -> Either CompileError Expr
blockToExpr (T.Block _ stmts) = do
  exprs <- mapM stmtToExpr stmts
  case exprs of
    [] -> Right EUnit
    [e] -> Right e
    es -> Right $ EList es

-- Convert a statement to an Expr
stmtToExpr :: T.Stmt -> Either CompileError Expr
stmtToExpr = \case
  T.STop _ expr -> expressionToExpr expr
  
  T.SLet loc pat expr -> do
    name <- patternToName pat
    expr' <- expressionToExpr expr
    -- Let bindings in Topineur are more like assignments, but we'll treat as define
    Right $ EDefine name expr' []
  
  T.SIf loc cond thenStmt elseStmt -> do
    cond' <- expressionToExpr cond
    then' <- stmtToExpr thenStmt
    else' <- case elseStmt of
      Just s -> stmtToExpr s
      Nothing -> Right EUnit
    Right $ EIf cond' then' else'
  
  T.SWhile loc cond stmts -> do
    cond' <- expressionToExpr cond
    body <- blockToExpr (T.Block loc stmts)
    Right $ EWhile cond' body
  
  T.SFor loc var range stmts -> do
    range' <- rangeToExpr range
    body <- blockToExpr (T.Block loc stmts)
    -- EFor takes: Name Expr Expr Expr (var, start, end, body)
    -- Range is Expression Expression (start, end)
    case range' of
      ERange start end -> Right $ EFor var start end body
      _ -> Left $ SyntaxError "Invalid range expression" Nothing
  
  T.SAssign loc lval expr -> do
    expr' <- expressionToExpr expr
    name <- lvalueToName lval
    Right $ EAssign name expr'
  
  T.SExpression loc expr -> expressionToExpr expr

-- Convert a range to an Expr
rangeToExpr :: T.Range -> Either CompileError Expr
rangeToExpr (T.Range start end) = do
  start' <- expressionToExpr start
  end' <- expressionToExpr end
  Right $ ERange start' end'

-- Convert an LValue to a name
lvalueToName :: T.LValue -> Either CompileError Name
lvalueToName = \case
  T.LVar _ name -> Right name
  T.LMember _ expr name -> Right name  -- Member access assignment - simplified

-- Convert an expression to an Expr
expressionToExpr :: T.Expression -> Either CompileError Expr
expressionToExpr = \case
  T.EVar _ name -> Right $ EVar name
  
  T.ESelf _ -> Left $ SyntaxError "self keyword not yet supported" Nothing
  
  T.EInt _ n -> Right $ EInt n
  
  T.EFloat _ f -> Right $ EFloat f
  
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
    -- Method call is obj.method(args) -> EApp (EMemberAccess obj method) args
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
  
  T.ELet _ name init body -> do
    init' <- expressionToExpr init
    body' <- expressionToExpr body
    -- Let is like a lambda application
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

-- Convert a field assignment to a (Name, Expr) pair
fieldAssignToPair :: T.FieldAssign -> Either CompileError (Name, Expr)
fieldAssignToPair (T.FieldAssign _ (Left (expr, name)) _) =
  -- This is a bitfield-style assignment: expr.name = value
  Left $ SyntaxError "Bitfield assignment not yet supported" Nothing
fieldAssignToPair (T.FieldAssign _ (Right name) expr) = do
  expr' <- expressionToExpr expr
  Right (name, expr')

-- Convert a binary operator string to BinOp
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

-- Convert a TypeAnn to a Type
typeAnnToType :: T.TypeAnn -> Either CompileError Type
typeAnnToType = \case
  T.TIdent _ name -> typeNameToType name
  T.TUpperIdent _ name -> Right $ TObject name
  T.TGeneric _ name args -> do
    args' <- mapM typeAnnToType args
    -- Generic types - for now treat as function type if it makes sense
    case name of
      "List" -> case args' of
        [t] -> Right $ TList t
        _ -> Left $ SyntaxError "List type requires one type argument" Nothing
      "Tuple" -> Right $ TTuple args'
      _ -> Right $ TObject name  -- Treat as object type
  T.TTuple _ types -> do
    types' <- mapM typeAnnToType types
    Right $ TTuple types'

-- Convert a type name string to Type
typeNameToType :: Name -> Either CompileError Type
typeNameToType = \case
  "Int" -> Right TInt
  "Float" -> Right TFloat
  "Bool" -> Right TBool
  "String" -> Right TString
  "Unit" -> Right TUnit
  name -> Right $ TObject name  -- Assume it's an object type


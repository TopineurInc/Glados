{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ObjectDesugar - Desugar Topineur objects/traits to core IR
-}

module ObjectDesugar
  ( desugarObjects
  , desugarExpr
  ) where

import AST
import qualified Data.Map as Map

-- Desugar a list of top-level declarations
desugarObjects :: [Expr] -> Either CompileError [Expr]
desugarObjects = mapM desugarExpr

-- Desugar a single expression
desugarExpr :: Expr -> Either CompileError Expr

-- Object definition: Transform to record-like structure
-- object type Point { x: Float, y: Float, def distance(): Float = ... }
-- becomes a constructor function that returns a lambda-based object
desugarExpr (EObjectDef objDef) = do
  let name = objName objDef
      fields = objFields objDef
      methods = objMethods objDef
      fieldNames = map fst fields

  -- Create constructor that takes field values and returns an object
  -- Point(x, y) becomes:
  -- lambda (x, y) ->
  --   let self = record { x, y } in
  --   let vtable = { distance = lambda () -> sqrt(x*x + y*y), ... } in
  --   object(name, fields, vtable)

  desugaredMethods <- mapM desugarMethod methods

  -- For simplicity, we'll represent this as a lambda that creates the object
  let constructorBody =
        EApp (EVar "__make_object")
             [EString name,
              EList (map EVar fieldNames),
              EList desugaredMethods]

  Right $ EDefine name (ELambda fieldNames constructorBody)

-- Trait definition: Store as metadata (handled by type system)
desugarExpr (ETraitDef traitDef) =
  -- Traits are primarily a type-system construct
  -- At runtime, we just need to ensure implementations exist
  Right $ EDefine (traitName traitDef) (EString ("__trait_" ++ traitName traitDef))

-- Trait implementation: Create a trait dictionary
desugarExpr (ETraitImpl impl) = do
  let traitName' = implTrait impl
      typeName = case implForType impl of
        TObject n -> n
        _ -> "__unknown_type"
      methods = implMethods impl

  desugaredMethods <- mapM desugarMethod methods

  -- Create a dictionary of methods for this trait implementation
  let dictName = "__trait_impl_" ++ traitName' ++ "_for_" ++ typeName
  Right $ EDefine dictName (EList desugaredMethods)

-- Object literal: Point { x = 1.0, y = 2.0 }
-- becomes: Point(1.0, 2.0)
desugarExpr (EObjectLit name fieldExprs) = do
  desugaredFields <- mapM (\(_, e) -> desugarExpr e) fieldExprs
  Right $ EApp (EVar name) desugaredFields

-- Method call: obj.method(args)
-- becomes: __call_method(obj, "method", [args])
desugarExpr (EMethodCall objExpr methodName args) = do
  desugaredObj <- desugarExpr objExpr
  desugaredArgs <- mapM desugarExpr args
  Right $ EApp (EVar "__call_method")
               (desugaredObj : EString methodName : desugaredArgs)

-- Field access: obj.field
-- becomes: __get_field(obj, "field")
desugarExpr (EFieldAccess objExpr fieldName) = do
  desugaredObj <- desugarExpr objExpr
  Right $ EApp (EVar "__get_field") [desugaredObj, EString fieldName]

-- Typed expression: Remove type annotation (types are checked separately)
desugarExpr (ETyped expr _) = desugarExpr expr

-- Linear binding: let !lin x = expr in body
-- becomes: let x = expr in body (linearity checked separately)
desugarExpr (ELinearBind name expr body) = do
  desugaredExpr <- desugarExpr expr
  desugaredBody <- desugarExpr body
  Right $ EApp (ELambda [name] desugaredBody) [desugaredExpr]

-- Match expression: Transform to nested if-then-else
-- match x { case 0 => a; case n => b }
-- becomes: if (x == 0) then a else let n = x in b
desugarExpr (EMatch expr patterns) = do
  desugaredExpr <- desugarExpr expr
  desugarMatch desugaredExpr patterns

-- Block: { stmt1; stmt2; expr }
-- becomes: let _ = stmt1 in let _ = stmt2 in expr
desugarExpr (EBlock exprs) = desugarBlock exprs

-- Let binding: let x : Type = expr in body
-- becomes: (lambda (x) body)(expr)
desugarExpr (ELet name _ expr body) = do
  desugaredExpr <- desugarExpr expr
  desugaredBody <- desugarExpr body
  Right $ EApp (ELambda [name] desugaredBody) [desugaredExpr]

-- Standard expressions: Recursively desugar
desugarExpr (EInt i) = Right $ EInt i
desugarExpr (EFloat f) = Right $ EFloat f
desugarExpr (EBool b) = Right $ EBool b
desugarExpr (EString s) = Right $ EString s
desugarExpr (EVar n) = Right $ EVar n

desugarExpr (EList exprs) = do
  desugared <- mapM desugarExpr exprs
  Right $ EList desugared

desugarExpr (ELambda params body) = do
  desugaredBody <- desugarExpr body
  Right $ ELambda params desugaredBody

desugarExpr (EDefine name expr) = do
  desugaredExpr <- desugarExpr expr
  Right $ EDefine name desugaredExpr

desugarExpr (EIf cond thenE elseE) = do
  desugaredCond <- desugarExpr cond
  desugaredThen <- desugarExpr thenE
  desugaredElse <- desugarExpr elseE
  Right $ EIf desugaredCond desugaredThen desugaredElse

desugarExpr (EApp func args) = do
  desugaredFunc <- desugarExpr func
  desugaredArgs <- mapM desugarExpr args
  Right $ EApp desugaredFunc desugaredArgs

desugarExpr (EQuote s) = Right $ EQuote s

-- Helper: Desugar a method definition to a lambda
desugarMethod :: MethodDef -> Either CompileError Expr
desugarMethod method = do
  desugaredBody <- desugarExpr (methBody method)
  let paramNames = map fst (methParams method)
  Right $ EDefine (methName method) (ELambda paramNames desugaredBody)

-- Helper: Desugar match expression to nested conditionals
desugarMatch :: Expr -> [(Pattern, Expr)] -> Either CompileError Expr
desugarMatch _ [] = Left $ SyntaxError "Empty match patterns" Nothing
desugarMatch scrutinee [(pattern, body)] = do
  -- Last pattern - no else branch needed
  desugarSinglePattern scrutinee pattern body (EInt 0) -- fallback

desugarMatch scrutinee ((pattern, body):rest) = do
  -- Create: if (matches pattern) then body else (rest of matches)
  restExpr <- desugarMatch scrutinee rest
  desugarSinglePattern scrutinee pattern body restExpr

-- Helper: Desugar a single pattern match
desugarSinglePattern :: Expr -> Pattern -> Expr -> Expr -> Either CompileError Expr
-- Literal pattern: if scrutinee == literal then body else elseExpr
desugarSinglePattern scrutinee (PLit (CInt n)) body elseExpr = do
  desugaredBody <- desugarExpr body
  Right $ EIf (EApp (EVar "==") [scrutinee, EInt n]) desugaredBody elseExpr

desugarSinglePattern scrutinee (PLit (CBool b)) body elseExpr = do
  desugaredBody <- desugarExpr body
  Right $ EIf (EApp (EVar "==") [scrutinee, EBool b]) desugaredBody elseExpr

desugarSinglePattern scrutinee (PLit (CString s)) body elseExpr = do
  desugaredBody <- desugarExpr body
  Right $ EIf (EApp (EVar "==") [scrutinee, EString s]) desugaredBody elseExpr

desugarSinglePattern scrutinee (PLit (CFloat f)) body elseExpr = do
  desugaredBody <- desugarExpr body
  Right $ EIf (EApp (EVar "==") [scrutinee, EFloat f]) desugaredBody elseExpr

-- Variable pattern: let var = scrutinee in body
desugarSinglePattern scrutinee (PVar name) body _ = do
  desugaredBody <- desugarExpr body
  Right $ EApp (ELambda [name] desugaredBody) [scrutinee]

-- Wildcard pattern: always matches, just evaluate body
desugarSinglePattern _ PWildcard body _ = desugarExpr body

-- Constructor pattern: More complex, simplified for now
desugarSinglePattern scrutinee (PConstructor name patterns) body elseExpr = do
  -- For simplicity, check constructor name and destructure
  -- This is a simplified version - full implementation would need type info
  desugaredBody <- desugarExpr body
  let check = EApp (EVar "__is_constructor") [scrutinee, EString name]
  Right $ EIf check desugaredBody elseExpr

-- Tuple pattern: Destructure tuple
desugarSinglePattern scrutinee (PTuple patterns) body elseExpr = do
  -- For simplicity, just bind to variables
  -- Full implementation would destructure properly
  desugaredBody <- desugarExpr body
  Right $ EIf (EApp (EVar "__is_tuple") [scrutinee]) desugaredBody elseExpr

-- Helper: Desugar block expression
desugarBlock :: [Expr] -> Either CompileError Expr
desugarBlock [] = Right $ EInt 0  -- Empty block returns unit (represented as 0)
desugarBlock [e] = desugarExpr e  -- Single expression
desugarBlock (e:rest) = do
  desugaredExpr <- desugarExpr e
  desugaredRest <- desugarBlock rest
  -- Create: let _ = e in rest
  Right $ EApp (ELambda ["_"] desugaredRest) [desugaredExpr]

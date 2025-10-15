{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Desugar - Topineur frontend scaffolding
-}

module Desugar
  ( TopineurDesugarContext(..)
  , emptyTopineurContext
  , desugarTopModule
  ) where

import AST
import Control.Monad (foldM)
import qualified ObjectDesugar as Obj
import qualified TraitResolver as Traits

data TopineurDesugarContext = TopineurDesugarContext
  { tdcObjectEnv :: Obj.ObjectDesugarEnv
  , tdcTraitEnv  :: Traits.TraitEnv
  } deriving (Eq, Show)

emptyTopineurContext :: Name -> TopineurDesugarContext
emptyTopineurContext moduleName = TopineurDesugarContext
  { tdcObjectEnv = Obj.emptyEnv moduleName
  , tdcTraitEnv = Traits.emptyTraitEnv
  }

desugarTopModule
  :: TopineurDesugarContext
  -> [Expr]
  -> Either CompileError ([Expr], TopineurDesugarContext)
desugarTopModule ctx exprs = foldM step ([], ctx) exprs
  where
    step (acc, curCtx) expr = do
      (expr', nextCtx) <- desugarTopExpr curCtx expr
      let acc' = acc ++ [expr']
      Right (acc', nextCtx)

desugarTopExpr
  :: TopineurDesugarContext
  -> Expr
  -> Either CompileError (Expr, TopineurDesugarContext)
desugarTopExpr ctx expr = case expr of
  EObjectDef def -> do
    desugaredDef <- Obj.desugarObjectDef (tdcObjectEnv ctx) def
    Right (EObjectDef desugaredDef, ctx)

  EObjectLit {} -> do
    desugared <- Obj.desugarObjectLiteral (tdcObjectEnv ctx) expr
    Right (desugared, ctx)

  ETraitDef def ->
    let traitEnv' = Traits.registerTrait (tdcTraitEnv ctx) def
        ctx' = ctx { tdcTraitEnv = traitEnv' }
    in Right (ETraitDef def, ctx')

  ETraitImpl impl -> do
    traitEnv' <- Traits.resolveTraitImpl (tdcTraitEnv ctx) impl
    let ctx' = ctx { tdcTraitEnv = traitEnv' }
    Right (ETraitImpl impl, ctx')

  _ ->
    Right (expr, ctx)

{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ObjectDesugar - scaffolding for Topineur object lowering
-}

module ObjectDesugar
  ( ObjectDesugarEnv(..)
  , ObjectLoweringPlan(..)
  , emptyEnv
  , planObjectLowering
  , desugarObjectDef
  , desugarObjectLiteral
  ) where

import qualified AST
import qualified Data.Map as Map
import Data.Map (Map)

-- | Environment carried while planning/desugaring objects.
data ObjectDesugarEnv = ObjectDesugarEnv
  { odeModuleName :: AST.Name
  , odeTypeEnv    :: Map AST.Name AST.Type
  } deriving (Eq, Show)

-- | Captures the intermediate structure that will later feed code generation.
data ObjectLoweringPlan = ObjectLoweringPlan
  { olpObjects :: [AST.ObjectDef]
  , olpTraits  :: [AST.TraitImpl]
  } deriving (Eq, Show)

-- | Helper used by future callers to start from a blank environment.
emptyEnv :: AST.Name -> ObjectDesugarEnv
emptyEnv moduleName = ObjectDesugarEnv
  { odeModuleName = moduleName
  , odeTypeEnv = Map.empty
  }

-- | Currently a thin wrapper that records objects/trait impls for later passes.
planObjectLowering
  :: ObjectDesugarEnv
  -> [AST.ObjectDef]
  -> [AST.TraitImpl]
  -> ObjectLoweringPlan
planObjectLowering _env objects traitImpls =
  ObjectLoweringPlan
    { olpObjects = objects
    , olpTraits = traitImpls
    }

-- | Placeholder implementation: simply wraps the high-level definition.
--   Future work will translate fields/methods into core records and lambdas.
desugarObjectDef
  :: ObjectDesugarEnv
  -> AST.ObjectDef
  -> Either AST.CompileError AST.ObjectDef
desugarObjectDef _env objDef = Right objDef

-- | Placeholder for object literal lowering.
--   For now the literal is returned untouched so the existing pipeline compiles.
desugarObjectLiteral
  :: ObjectDesugarEnv
  -> AST.Expr
  -> Either AST.CompileError AST.Expr
desugarObjectLiteral _env expr = Right expr

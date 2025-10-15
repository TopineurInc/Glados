{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TraitResolver - scaffolding for Topineur trait dictionary handling
-}

module TraitResolver
  ( TraitEnv(..)
  , TraitDictionary
  , emptyTraitEnv
  , registerTrait
  , resolveTraitImpl
  , lookupTrait
  ) where

import qualified AST
import qualified Data.Map as Map
import Data.Map (Map)

-- | Trait dictionaries are represented as lists of method definitions for now.
type TraitDictionary = [AST.MethodDef]

data TraitEnv = TraitEnv
  { teTraits :: Map AST.Name AST.TraitDef
  , teImpls  :: Map AST.Name TraitDictionary
  } deriving (Eq, Show)

emptyTraitEnv :: TraitEnv
emptyTraitEnv = TraitEnv
  { teTraits = Map.empty
  , teImpls = Map.empty
  }

registerTrait :: TraitEnv -> AST.TraitDef -> TraitEnv
registerTrait env traitDef =
  env { teTraits = Map.insert (AST.traitName traitDef) traitDef (teTraits env) }

resolveTraitImpl :: TraitEnv -> AST.TraitImpl -> Either AST.CompileError TraitEnv
resolveTraitImpl env impl =
  case Map.lookup (AST.implTrait impl) (teTraits env) of
    Nothing ->
      Left $ AST.SyntaxError ("Unknown trait: " ++ AST.implTrait impl) Nothing
    Just _knownTrait ->
      let key = traitImplKey impl
          dictionary = map pureMethod (AST.implMethods impl)
          updatedDicts = Map.insertWith (++) key dictionary (teImpls env)
      in Right env { teImpls = updatedDicts }
  where
    traitImplKey :: AST.TraitImpl -> AST.Name
    traitImplKey ti = AST.implTrait ti ++ ":" ++ show (AST.implForType ti)

    -- Placeholder: wrap each method definition so code generation can inspect it later.
    pureMethod :: AST.MethodDef -> AST.MethodDef
    pureMethod = id

lookupTrait :: TraitEnv -> AST.Name -> Maybe AST.TraitDef
lookupTrait env traitName = Map.lookup traitName (teTraits env)

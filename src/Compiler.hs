{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Compiler - Topineur frontend driver
-}

module Compiler
  ( CompilerConfig(..)
  , defaultConfig
  , CompilerError(..)
  , compileModule
  ) where

import qualified TopineurPipeline as Topineur

data CompilerConfig = CompilerConfig
  { cfgEmitCore :: Bool
  } deriving (Eq, Show)

defaultConfig :: CompilerConfig
defaultConfig = CompilerConfig
  { cfgEmitCore = True
  }

data CompilerError
  = PipelineError Topineur.TopineurError
  deriving (Eq, Show)

compileModule
  :: CompilerConfig
  -> String          -- ^ Module name
  -> String          -- ^ Source contents
  -> Either CompilerError Topineur.TopineurPipelineResult
compileModule _ moduleName source =
  mapLeft PipelineError (Topineur.compileTopineurModule moduleName source)

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = either (Left . f) Right

{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TopineurPipeline - frontend orchestration for Topineur sources
-}

module TopineurPipeline
  ( TopineurError(..)
  , TopineurPipelineResult(..)
  , compileTopineurModule
  ) where

import qualified AST
import qualified Desugar
import qualified EffectChecker
import qualified LinearityChecker
import qualified TopineurParser
import qualified TypeChecker
import Text.Parsec (ParseError)

-- | Aggregated error type for the Topineur pipeline.
data TopineurError
  = ParserError ParseError
  | TypeSystemError TypeChecker.TypeError
  | EffectSystemError EffectChecker.EffectError
  | LinearitySystemError LinearityChecker.LinearityError
  | DesugaringError AST.CompileError
  deriving (Eq, Show)

-- | Successful result of the frontend passes.
data TopineurPipelineResult = TopineurPipelineResult
  { tprParsed      :: [AST.Expr]
  , tprDesugared   :: [AST.Expr]
  , tprTypes       :: [AST.Type]
  , tprEffects     :: [AST.EffectRow]
  , tprContext     :: Desugar.TopineurDesugarContext
  } deriving (Eq, Show)

-- | Run parsing, type/effect/linearity checks, and desugaring for a module.
compileTopineurModule
  :: AST.Name        -- ^ Module name
  -> String          -- ^ Source contents
  -> Either TopineurError TopineurPipelineResult
compileTopineurModule moduleName source = do
  parsed <- mapLeft ParserError $ TopineurParser.parseTopineur source

  -- Type checking
  let typeEnv = TypeChecker.defaultTypeEnv
  types <- traverse (mapLeft TypeSystemError . TypeChecker.typeCheck typeEnv) parsed

  -- Effect inference (placeholder check for now)
  let effectEnv = EffectChecker.defaultEffectEnv
      effects   = map (EffectChecker.inferEffects effectEnv) parsed

  -- Linearity verification
  mapM_ (mapLeft LinearitySystemError . LinearityChecker.checkLinearity) parsed

  -- Desugar into the core-friendly representation
  let initialCtx = Desugar.emptyTopineurContext moduleName
  (desugared, finalCtx) <- mapLeft DesugaringError $
    Desugar.desugarTopModule initialCtx parsed

  pure TopineurPipelineResult
    { tprParsed = parsed
    , tprDesugared = desugared
    , tprTypes = types
    , tprEffects = effects
    , tprContext = finalCtx
    }

-- | Helper to map the `Left` constructor.
mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = either (Left . f) Right

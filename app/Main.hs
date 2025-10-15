{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Main - Topineur CLI entry point
-}

module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.List (intercalate, zip4)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)

import AST
import qualified TopineurPipeline as Topineur

data Command
  = CmdCheck FilePath
  | CmdPipeline FilePath
  | CmdHelp
  deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    CmdHelp -> printHelp
    CmdCheck file -> runCheck file
    CmdPipeline file -> runPipeline file

parseArgs :: [String] -> Command
parseArgs args = case args of
  ["check", file] -> CmdCheck file
  ["pipeline", file] -> CmdPipeline file
  ["help"] -> CmdHelp
  ["--help"] -> CmdHelp
  ["-h"] -> CmdHelp
  [file] -> CmdPipeline file
  _ -> CmdHelp

printHelp :: IO ()
printHelp = do
  putStrLn "Topineur CLI"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  glados check <file.top>      Parse + type/effect/linearity check"
  putStrLn "  glados pipeline <file.top>   Display frontend pipeline summary"
  putStrLn "  glados help                  Show this message"

runCheck :: FilePath -> IO ()
runCheck file = do
  outcome <- runPipelineCore file
  case outcome of
    Left err -> exitWithError ("Typecheck failed: " ++ renderTopineurError err)
    Right _  -> putStrLn "Typecheck succeeded." >> exitSuccess

runPipeline :: FilePath -> IO ()
runPipeline file = do
  outcome <- runPipelineCore file
  case outcome of
    Left err -> exitWithError ("Pipeline failed: " ++ renderTopineurError err)
    Right result -> do
      putStrLn "=== Topineur Pipeline Summary ==="
      let parsedExprs     = Topineur.tprParsed result
          desugaredExprs  = Topineur.tprDesugared result
          inferredTypes   = Topineur.tprTypes result
          inferredEffects = Topineur.tprEffects result
      forM_ (zip4 [0 :: Int ..] parsedExprs desugaredExprs (zip inferredTypes inferredEffects)) $
        \(idx, parsedExpr, desugaredExpr, (ty, effRow)) -> do
          putStrLn $ "Expr #" ++ show idx
          putStrLn $ "  Parsed    : " ++ show parsedExpr
          putStrLn $ "  Desugared : " ++ show desugaredExpr
          putStrLn $ "  Type      : " ++ renderType ty
          putStrLn $ "  Effects   : " ++ renderEffectRow effRow
      putStrLn "\n=== Desugar Context ==="
      print (Topineur.tprContext result)
      exitSuccess

runPipelineCore :: FilePath -> IO (Either Topineur.TopineurError Topineur.TopineurPipelineResult)
runPipelineCore file = do
  sourceOrErr <- try (readFile file) :: IO (Either IOException String)
  case sourceOrErr of
    Left _ -> exitWithError $ "Cannot open file: " ++ file
    Right source ->
      let moduleName = moduleNameFromPath file
      in pure $ Topineur.compileTopineurModule moduleName source

moduleNameFromPath :: FilePath -> Name
moduleNameFromPath path =
  let base = takeBaseName path
  in if null base then "Main" else base

renderTopineurError :: Topineur.TopineurError -> String
renderTopineurError err = case err of
  Topineur.ParserError parseErr -> show parseErr
  Topineur.TypeSystemError typeErr -> show typeErr
  Topineur.EffectSystemError effectErr -> show effectErr
  Topineur.LinearitySystemError linErr -> show linErr
  Topineur.DesugaringError compileErr -> show compileErr

renderType :: Type -> String
renderType = show

renderEffectRow :: EffectRow -> String
renderEffectRow (EffectRow effects) =
  case effects of
    [] -> "{}"
    _  -> "{ " ++ intercalate ", " (map renderEffect effects) ++ " }"

renderEffect :: Effect -> String
renderEffect effect = case effect of
  EffIO -> "IO"
  EffState -> "State"
  EffNetwork -> "Network"
  EffException -> "Exception"
  EffAsync -> "Async"
  EffCustom name -> "Custom(" ++ name ++ ")"

exitWithError :: String -> IO a
exitWithError msg = do
  hPutStrLn stderr ("*** ERROR : " ++ msg)
  exitWith (ExitFailure 84)

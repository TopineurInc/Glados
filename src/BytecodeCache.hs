{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BytecodeCache
  ( saveBytecodeCache
  , loadBytecodeCache
  , isCacheValid
  , getCachePath
  , CacheData(..)
  ) where

import AST
import Control.Exception (catch, SomeException)
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import System.Directory (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.FilePath (replaceExtension, takeDirectory)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)

-- | Data structure to be cached in .topo files
data CacheData = CacheData
  { cdMainCode :: CodeObject
  , cdDefs :: Map.Map Name CodeObject
  , cdTimestamp :: UTCTime
  } deriving (Eq, Show, Generic)

-- Manual Binary instance for CacheData since UTCTime doesn't have one
instance Binary CacheData where
  put (CacheData mainCode defs timestamp) = do
    put mainCode
    put (Map.toList defs)
    -- Encode UTCTime as POSIX seconds (Double)
    put (realToFrac (utcTimeToPOSIXSeconds timestamp) :: Double)

  get = do
    mainCode <- get
    defsList <- get
    timeDouble <- get :: Get Double
    let timestamp = posixSecondsToUTCTime (realToFrac timeDouble)
    return $ CacheData mainCode (Map.fromList defsList) timestamp

-- Binary instances for AST types
instance Binary Constant where
  put (CInt i) = putWord8 0 >> put i
  put (CFloat d) = putWord8 1 >> put d
  put (CBool b) = putWord8 2 >> put b
  put (CString s) = putWord8 3 >> put s
  put (CFuncRef n) = putWord8 4 >> put n

  get = do
    tag <- getWord8
    case tag of
      0 -> CInt <$> get
      1 -> CFloat <$> get
      2 -> CBool <$> get
      3 -> CString <$> get
      4 -> CFuncRef <$> get
      _ -> fail "Invalid Constant tag"

instance Binary Instr where
  put (IConst i) = putWord8 0 >> put i
  put (ILoad i) = putWord8 1 >> put i
  put (IStore i) = putWord8 2 >> put i
  put (IPrim s) = putWord8 3 >> put s
  put (ICall i n) = putWord8 4 >> put i >> put n
  put (ITailCall i n) = putWord8 5 >> put i >> put n
  put IReturn = putWord8 6
  put (IJump i) = putWord8 7 >> put i
  put (IJumpIfFalse i) = putWord8 8 >> put i
  put IPop = putWord8 9
  put INop = putWord8 10
  put (IMakeClosure n is) = putWord8 11 >> put n >> put is
  put (ILoadClosure i) = putWord8 12 >> put i
  put (IStoreClosure i) = putWord8 13 >> put i
  put IWhile = putWord8 14
  put IFor = putWord8 15
  put IBreak = putWord8 16
  put IContinue = putWord8 17
  put (ITupleCreate i) = putWord8 18 >> put i
  put (ITupleGet i) = putWord8 19 >> put i
  put (IListCreate i) = putWord8 20 >> put i
  put IListGet = putWord8 21
  put IListSet = putWord8 22
  put (IObjectCreate n ns) = putWord8 23 >> put n >> put ns
  put (IMemberGet n) = putWord8 24 >> put n
  put (IMemberSet n) = putWord8 25 >> put n
  put (IAssign i) = putWord8 26 >> put i
  put (IAssignGlobal n) = putWord8 27 >> put n
  put IRangeCreate = putWord8 28

  get = do
    tag <- getWord8
    case tag of
      0 -> IConst <$> get
      1 -> ILoad <$> get
      2 -> IStore <$> get
      3 -> IPrim <$> get
      4 -> ICall <$> get <*> get
      5 -> ITailCall <$> get <*> get
      6 -> return IReturn
      7 -> IJump <$> get
      8 -> IJumpIfFalse <$> get
      9 -> return IPop
      10 -> return INop
      11 -> IMakeClosure <$> get <*> get
      12 -> ILoadClosure <$> get
      13 -> IStoreClosure <$> get
      14 -> return IWhile
      15 -> return IFor
      16 -> return IBreak
      17 -> return IContinue
      18 -> ITupleCreate <$> get
      19 -> ITupleGet <$> get
      20 -> IListCreate <$> get
      21 -> return IListGet
      22 -> return IListSet
      23 -> IObjectCreate <$> get <*> get
      24 -> IMemberGet <$> get
      25 -> IMemberSet <$> get
      26 -> IAssign <$> get
      27 -> IAssignGlobal <$> get
      28 -> return IRangeCreate
      _ -> fail "Invalid Instr tag"

instance Binary CodeObject where
  put (CodeObject name arity maxLocals consts instrs labelMap) = do
    put name
    put arity
    put maxLocals
    put (Vector.toList consts)
    put (Vector.toList instrs)
    put (Map.toList labelMap)

  get = do
    name <- get
    arity <- get
    maxLocals <- get
    consts <- Vector.fromList <$> get
    instrs <- Vector.fromList <$> get
    labelMap <- Map.fromList <$> get
    return $ CodeObject name arity maxLocals consts instrs labelMap

-- | Get the cache file path for a source file
-- e.g., "foo.top" -> "foo.topo"
getCachePath :: FilePath -> FilePath
getCachePath = replaceExtension <*> const ".topo"

-- | Save compiled bytecode to cache file
saveBytecodeCache :: FilePath -> CodeObject -> Map.Map Name CodeObject -> IO (Either String ())
saveBytecodeCache sourcePath mainCode defs = do
  catch (do
    -- Get source file modification time
    sourceTime <- getModificationTime sourcePath
    let cachePath = getCachePath sourcePath
        cacheData = CacheData mainCode defs sourceTime
        cacheDir = takeDirectory cachePath

    -- Create directory if it doesn't exist
    createDirectoryIfMissing True cacheDir

    -- Write binary data to .topo file
    encodeFile cachePath cacheData
    return $ Right ()
    ) $ \(e :: SomeException) -> return $ Left $ "Failed to save cache: " ++ show e

-- | Load bytecode from cache file if it exists and is valid
loadBytecodeCache :: FilePath -> IO (Maybe (CodeObject, Map.Map Name CodeObject))
loadBytecodeCache sourcePath = do
  catch (do
    let cachePath = getCachePath sourcePath
    cacheExists <- doesFileExist cachePath

    if not cacheExists
      then return Nothing
      else do
        -- Check if cache is still valid
        valid <- isCacheValid sourcePath cachePath
        if valid
          then do
            cacheData <- decodeFile cachePath
            return $ Just (cdMainCode cacheData, cdDefs cacheData)
          else return Nothing
    ) $ \(_ :: SomeException) -> return Nothing

-- | Check if cache file is valid (source hasn't been modified since cache was created)
isCacheValid :: FilePath -> FilePath -> IO Bool
isCacheValid sourcePath cachePath = do
  catch (do
    sourceExists <- doesFileExist sourcePath
    cacheExists <- doesFileExist cachePath

    if not (sourceExists && cacheExists)
      then return False
      else do
        sourceTime <- getModificationTime sourcePath
        cacheData <- decodeFile cachePath :: IO CacheData
        let cachedSourceTime = cdTimestamp cacheData

        -- Cache is valid if source hasn't been modified since cache was created
        return $ sourceTime <= cachedSourceTime
    ) $ \(_ :: SomeException) -> return False

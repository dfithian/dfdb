module DFDB.Persist where

import ClassyPrelude
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import System.Directory (createDirectoryIfMissing, doesFileExist)

import qualified DFDB.Types

dbDir :: FilePath
dbDir = ".dfdb"

dbFile :: FilePath
dbFile = "db"

saveDatabase :: MonadIO m => FilePath -> DFDB.Types.Database -> m ()
saveDatabase dir db = do
  liftIO $ createDirectoryIfMissing True dir
  liftIO $ encodeFile (dir </> dbFile) db

loadDatabase :: MonadIO m => FilePath -> m (Maybe (Either String DFDB.Types.Database))
loadDatabase dir = do
  let fp = dir </> dbFile
  liftIO (doesFileExist fp) >>= \ case
    True -> liftIO $ Just <$> eitherDecodeFileStrict' fp
    False -> pure Nothing

module Agda.Build.Files where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import System.Directory
import System.FilePath

import Agda.Interaction.FindFile (sourceFileExts)

findAgdaFiles :: MonadIO m => FilePath -> m [FilePath]
findAgdaFiles = findFiles' sourceFileExts

-- | Returns the files with the given file name suffixes. Returns file path
-- relevant to given search directory.
findFiles' :: MonadIO m => [String] -> FilePath -> m [FilePath]
findFiles' suffs dir = liftIO $ go "."
  where
    go rel = do
      chlds <- filter (not . flip elem [".", ".."]) <$> getDirectoryContents (dir </> rel)
      rs <- forM chlds $ \chld -> do
        isDir <- doesDirectoryExist (dir </> rel </> chld)
        if | isDir -> go (rel </> chld)
           | any (`isSuffixOf` chld) suffs -> return [rel </> chld]
           | otherwise -> return []
      return $ concat rs

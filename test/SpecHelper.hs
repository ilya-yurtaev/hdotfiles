module SpecHelper
  ( withTmpDir
  , exists
  ) where

import System.Directory (doesFileExist, doesDirectoryExist)
import System.IO.Temp (withSystemTempDirectory)


exists :: FilePath -> IO Bool
exists path = or <$> sequence [doesFileExist path, doesDirectoryExist path]

withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = withSystemTempDirectory "hdotfiles_test"

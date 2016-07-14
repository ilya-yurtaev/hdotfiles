module SpecHelper where

import           Control.Monad ((>=>))
import           System.Directory (doesFileExist, doesDirectoryExist)
import           System.IO.Temp (withSystemTempDirectory)
  
import           Dotfiles
import           Dotfiles.Config
import           Dotfiles.Commands


exists :: FilePath -> IO Bool
exists path = or `fmap` sequence [doesFileExist path, doesDirectoryExist path]
  

mockEnv :: FilePath -> IO Env
mockEnv tmpDir = do
  env <- readEnv tmpDir
  runCommand env install []
  return env


withEnv :: (Env -> IO ()) -> IO ()
withEnv action = withSystemTempDirectory "hdotfiles_test" (mockEnv >=> action)

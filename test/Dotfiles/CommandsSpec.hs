module Dotfiles.CommandsSpec where

import System.Directory
import Dotfiles
import Dotfiles.Commands
import Control.Monad.Trans.Reader
import Test.Hspec
  
runCommand' :: Command -> Args -> IO ()
runCommand' cmd args = do
  env <- mkEnv `fmap` getTemporaryDirectory
  runReaderT cmd (env, args)


spec :: Spec
spec = do
     describe "Commands" $ do
       it "conf creating" $ do
          tmpdir <- getTemporaryDirectory
          runCommand' install []

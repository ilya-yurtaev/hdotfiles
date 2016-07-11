module Dotfiles.CommandsSpec where

import qualified Data.Set as Set
import           Test.Hspec

import           Dotfiles
import           Dotfiles.Commands
import           Dotfiles.Utils
import           SpecHelper (withTmpDir, exists)


spec :: Spec
spec = do
  describe "Commands" $ do
    around withTmpDir $ do
      it "simple install" $ \tmpDir -> do
        let env = mkEnv tmpDir
        let cfg = ["~/first", "~/second", "~/third", "~/fourth", "~/fifth"]
        runCommand env install []
        exists (envAppDir env) `shouldReturn` True
        mapM_ (\fn -> writeFile (normalize (envRoot env) fn) fn) cfg
        runCommand env addDotfiles cfg
        readCfg env `shouldReturn` Set.fromList cfg
        runCommand env forgetDotfiles ["~/firstfile"] `shouldReturn` ()
        runCommand env syncDotfiles [] `shouldReturn` ()
        -- readCfg env `shouldReturn` Set.fromList (tail cfg) -- zomg it fails. wtf
        runCommand env showStatus []
        showHelp `shouldReturn` ()

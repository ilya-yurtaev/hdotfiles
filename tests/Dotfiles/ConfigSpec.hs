module Dotfiles.ConfigSpec where

import System.Directory (getHomeDirectory, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Dotfiles
import Dotfiles.Config
import SpecHelper


spec :: Spec
spec = do
  describe "Dotfiles.Config" $ do
    around withEnv $ do
      it "covers default env" $ \_ -> do
        env' <- defaultEnv
        home <- getHomeDirectory
        tmp  <- getTemporaryDirectory
        envRoot env' `shouldBe` home
        envTmp env' `shouldBe` tmp
        envAppDir env' `shouldBe` home </> ".dotfiles"

      it "reads user config" $ \env -> do
        let appdir = envRoot env </> "appdir"
        let cfg    = Config {appDir = Just appdir, dfNames = []}
        writeConfig (envCfgPath env) cfg
        env' <- readEnv (envRoot env)
        envAppDir env' `shouldBe` appdir

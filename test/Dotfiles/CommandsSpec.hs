module Dotfiles.CommandsSpec where

import Control.Monad ((>=>))
import Data.List (sort)
import Test.Hspec

import Dotfiles
import Dotfiles.Commands
import Dotfiles.Config
import Dotfiles.Utils
import SpecHelper


-- re-read config
cmd :: Env -> Command -> Args -> IO ()
cmd env command args = readEnv (envRoot env) >>= \env' -> runCommand env' command args


-- read names only
readCfg :: Env -> IO [String]
readCfg env = (sort . dfNames) `fmap` readConfig (envCfgPath env)


cfg :: [String]
cfg = sort ["~/first", "~/second", "~/third", "~/fourth", "~/fifth"]


withPopulatedEnv :: (Env -> IO ()) -> IO ()
withPopulatedEnv action = withEnv ((createFiles >> return) >=> action)
  where createFiles env = mapM_ (\fn -> writeFile (normalize (envRoot env) fn) fn) cfg


spec :: Spec
spec = do
  describe "Commands" $ do
    around withPopulatedEnv $ do
      it "env init" $ \env -> do
        readCfg env `shouldReturn` []
        exists (envAppDir env) `shouldReturn` True
        exists (envCfgPath env) `shouldReturn` True
        exists (envStorage env) `shouldReturn` True

      it "runs `add` command" $ \env -> do
        cmd env addDotfiles [head cfg]
        readCfg env `shouldReturn` [head cfg]

      it "runs `forget` command" $ \env -> do
        cmd env addDotfiles cfg
        readCfg env `shouldReturn` cfg
        cmd env forgetDotfiles [head cfg]
        cmd env syncDotfiles []
        readCfg env `shouldReturn` tail cfg

      it "runs `sync` command" $ \env -> do
        writeConfig (envCfgPath env) Config
          { appDir = Just $ denormalize (envRoot env) (envAppDir env)
          , dfNames = cfg
          }
        cmd env syncDotfiles []
        readCfg env `shouldReturn` cfg

      -- it "runs `install` with git repo" $ \env -> do
      --   rm (envAppDir env)
      --   rm (envCfgPath env)
      --   runCommand env install ["https://github.com/ilya-yurtaev/dotfiles"]
      --   exists (envCfgPath env) `shouldReturn` True


module DotfilesSpec where

import Dotfiles
import System.Directory (getTemporaryDirectory)
import System.FilePath
import Test.Hspec

defaultCfg =
  unlines [ "~/.wtfrc"
          , "~/.broken_link"
          , "~/.alien_link"
          , "~/.conflict"
          ]

env = mkEnv "/tmp"


spec :: Spec
spec = describe "Dotfiles" $ do
    it "env init" $ do
        envHome env `shouldBe` "/tmp"

    it "path denormalisation" $ do
        denormalize env (envHome env) `shouldBe` "~"
        denormalize env "~/.wtfrc" `shouldBe` "~/.wtfrc"
        denormalize env "" `shouldBe` ""

    it "path normalisation" $ do
        normalize env "~" `shouldBe` envHome env
        normalize env "~/.wtfrc" `shouldBe` joinPath [envHome env, ".wtfrc"]
        normalize env "" `shouldBe` ""

    it "determines dotfiles status" $ do
        getDotfileStatus "/tmp/doesnotexist" "/tmp/samething" `shouldReturn` Invalid

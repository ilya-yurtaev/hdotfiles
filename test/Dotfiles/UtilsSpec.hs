module Dotfiles.UtilsSpec where

import System.FilePath (joinPath, (</>))
import Test.Hspec

import Dotfiles
import Dotfiles.Utils
import Dotfiles.Commands
import SpecHelper


spec :: Spec
spec = do
  describe "Utils" $ do
    around withTmpDir $ do
      it "env init" $ \tmpDir -> do
        let env = mkEnv tmpDir
        envAppDir env `shouldBe` tmpDir </> ".dotfiles"

      it "create\\remove dirs\\files" $ \tmpDir -> do
        let env = mkEnv tmpDir
        let testdir = joinPath [envAppDir env, "somedir", "subdir"]
        let anotherdir = joinPath [envAppDir env, "anotherdir"]
        let testfile = joinPath [envAppDir env, "somefile"]
        mkdir testdir
        writeFile testfile ""
        cp testfile (joinPath [testdir, "somefile"])
        cp testdir anotherdir
        mv (joinPath [testdir, "somefile"]) testfile
        rm anotherdir
        mv testdir anotherdir
        rm testfile
        rm testdir
        rm (joinPath [anotherdir, "somefile"])
        rm (joinPath [envAppDir env, "_"])

      it "path denormalization" $ \tmpDir -> do
        let env = mkEnv tmpDir
        denormalize (envAppDir env) (envAppDir env) `shouldBe` "~"
        denormalize (envAppDir env) "~/.wtfrc" `shouldBe` "~/.wtfrc"
        denormalize (envAppDir env) "" `shouldBe` ""

      it "path normalization" $ \tmpDir -> do
        let env = mkEnv tmpDir
        normalize (envAppDir env) "~" `shouldBe` envAppDir env
        normalize (envAppDir env) "~/.wtfrc" `shouldBe` joinPath [envAppDir env, ".wtfrc"]
        normalize (envAppDir env) "" `shouldBe` ""
        normalize (envAppDir env) "/absolute/path" `shouldBe` "/absolute/path"
        normalize (envAppDir env) "~/glob/*" `shouldBe` joinPath [envAppDir env, "glob"]
        normalize (envAppDir env) "~/dir/" `shouldBe` joinPath [envAppDir env, "dir"]

      it "files comparison" $ \tmpDir -> do
        let env = mkEnv tmpDir
        runCommand env install []
        let f1 = joinPath [envAppDir env, "f1"]
        let f2 = joinPath [envAppDir env, "f2"]
        let f3 = joinPath [envAppDir env, "f3"]
        writeFile f1 "some file content"
        writeFile f2 "some file content"
        writeFile f3 "third file content"
        cmp f1 f2 `shouldReturn` True
        cmp f1 f3 `shouldReturn` False

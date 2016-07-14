module Dotfiles.UtilsSpec where

import System.FilePath ((</>))
import Test.Hspec

import Dotfiles
import Dotfiles.Utils
import SpecHelper


mkdir' :: FilePath -> IO ()
mkdir' p = do
  exists p `shouldReturn` False
  mkdir p
  exists p `shouldReturn` True
  

touch :: FilePath -> IO ()
touch p = do
  exists p `shouldReturn` False
  writeFile p ""
  exists p `shouldReturn` True
  

spec :: Spec
spec = do
  describe "Utils" $ do
    around withEnv $ do
      it "creates directory with parents" $ \env -> do
        let testdir = envAppDir env </> "somedir" </> "subdir"
        mkdir' testdir

      it "copies file" $ \env -> do
        let testfile = envAppDir env </> "somefile"
        let testdir = envAppDir env </> "somedir" </> "subdir"
        let anotherdir = envAppDir env </> "anotherdir"
        let copiedfile = anotherdir </> "copiedfile"
        touch testfile
        mkdir' testdir
        mkdir' anotherdir
        exists copiedfile `shouldReturn` False
        cp testfile copiedfile
        exists testfile `shouldReturn` True
        exists copiedfile `shouldReturn` True

      it "copies dir recursive" $ \env -> do
        let dir1 = envRoot env </> "dir1"
        let dir2 = envRoot env </> "dir2"
        let testfile = dir1 </> "somefile"
        mkdir' dir1
        mkdir' dir2
        touch testfile
        cp dir1 dir2
        exists (dir2 </> "somefile") `shouldReturn` True
        exists dir1 `shouldReturn` True
        exists testfile `shouldReturn` True
  
      it "renames file" $ \env -> do
        let testfile = envRoot env </> "testfile"
        let target = envRoot env </> "target"
        exists target `shouldReturn` False
        touch testfile
        mv testfile target
        exists target `shouldReturn` True
        exists testfile `shouldReturn` False

      it "renames dir" $ \env -> do
        let testdir = envRoot env </> "dir1"
        let targetdir = envRoot env </> "dir2"
        let testfile = testdir </> "testfile"
        let targetfile = targetdir </> "testfile"
        mkdir' testdir
        touch testfile
        mv testdir targetdir
        exists testdir `shouldReturn` False
        exists testfile `shouldReturn` False
        exists targetdir `shouldReturn` True
        exists targetfile `shouldReturn` True

      it "removes file" $ \env -> do
        let testfile = envAppDir env </> "testfile"
        touch testfile
        rm testfile
        exists testfile `shouldReturn` False

      it "removes dir" $ \env -> do
        let testdir = envAppDir env </> "testdir"
        let testfile = testdir </> "testfile"
        mkdir' testdir
        touch testfile
        rm testdir
        exists testdir `shouldReturn` False
        exists testfile `shouldReturn` False

      it "denormalizes normlized path" $ \env -> do
        denormalize (envAppDir env) (envAppDir env) `shouldBe` "~"

      it "denormalizes already denormalized path" $ \env -> do
        denormalize (envAppDir env) "~/.wtfrc" `shouldBe` "~/.wtfrc"

      it "denormalizes empty string" $ \env -> do
        denormalize (envAppDir env) "" `shouldBe` ""

      it "normalizes root" $ \env -> do
        normalize (envRoot env) "~" `shouldBe` envRoot env

      it "normalizes first level path" $ \env -> do
        normalize (envRoot env) "~/.wtfrc" `shouldBe` envRoot env </> ".wtfrc"

      it "normalizes empty path" $ \env -> do
        normalize (envAppDir env) "" `shouldBe` ""

      it "normalizes absolute path" $ \env -> do
        normalize (envAppDir env) "/absolute/path" `shouldBe` "/absolute/path"

      it "normalizes glob" $ \env -> do
        normalize (envAppDir env) "~/glob/*" `shouldBe` envAppDir env </> "glob"

      it "normalizes dir" $ \env -> do
        normalize (envAppDir env) "~/dir/" `shouldBe` envAppDir env </> "dir"

      it "compares equal files" $ \env -> do
        let f1 = envAppDir env </> "f1"
        let f2 = envAppDir env </> "f2"
        writeFile f1 "some file content"
        writeFile f2 "some file content"
        cmp f1 f2 `shouldReturn` True

      it "compares different files" $ \env -> do
        let f3 = envAppDir env </> "f3"
        let f4 = envAppDir env </> "f4"
        writeFile f3 "some file content"
        writeFile f4 "another file content"
        cmp f3 f4 `shouldReturn` False

module DotfilesSpec where

import System.FilePath ((</>))
import System.Posix (createSymbolicLink, readSymbolicLink)
import Test.Hspec

import Dotfiles
import Dotfiles.Utils

import SpecHelper


spec :: Spec
spec = describe "Dotfiles" $ do
  around withEnv $ do
    it "determines invalid status" $ \env -> do
      let path = "~/.somepath"
      df <- mkDotfile env path
      dfName df `shouldBe` path
      dfStatus df `shouldBe` Invalid
      exists (dfSrc df) `shouldReturn` False
      exists (dfDst df) `shouldReturn` False

    it "determines tracked status" $ \env -> do
      let dst = envStorage env </> ".validpath"
      let src = envRoot env </> ".validpath"
      writeFile dst "some content"
      createSymbolicLink dst src
      df <- mkDotfile env "~/.validpath"
      dfSrc df `shouldBe` src
      dfDst df `shouldBe` dst
      exists (dfSrc df) `shouldReturn` True
      exists (dfDst df) `shouldReturn` True
      readSymbolicLink src `shouldReturn` dst
      dfStatus df `shouldBe` Tracked

    it "determines pending left status" $ \env -> do
      let src = envRoot env </> ".pending_left"
      let dst = envStorage env </> ".pending_left"
      writeFile src "some content"
      df <- mkDotfile env "~/.pending_left"
      dfSrc df `shouldBe` src
      dfDst df `shouldBe` dst
      exists (dfSrc df) `shouldReturn` True
      exists (dfDst df) `shouldReturn` False
      dfStatus df `shouldBe` PendingLeft

    it "determines pending right status" $ \env -> do
      let src = envRoot env </> ".pending_right"
      let dst = envStorage env </> ".pending_right"
      writeFile dst "some content"
      df <- mkDotfile env "~/.pending_right"
      dfSrc df `shouldBe` src
      dfDst df `shouldBe` dst
      exists (dfSrc df) `shouldReturn` False
      exists (dfDst df) `shouldReturn` True
      dfStatus df `shouldBe` PendingRight

    it "determines conflict status" $ \env -> do
      let src = envRoot env </> ".conflict"
      let dst = envStorage env </> ".conflict"
      writeFile src "local file content"
      writeFile dst "stored file content"
      df <- mkDotfile env "~/.conflict"
      exists (dfSrc df) `shouldReturn` True
      exists (dfDst df) `shouldReturn` True
      cmp src dst `shouldReturn` False
      dfStatus df `shouldBe` Conflicts

    it "determines alien (src link doesnt lead to dst)" $ \env -> do
      let src = envRoot env </> ".alien_link"
      let alien_target = envRoot env </> "alien_target"
      let dst = envStorage env </> ".alien_link"
      writeFile dst "stored file content"
      writeFile alien_target "alient file content"
      createSymbolicLink alien_target src
      exists src `shouldReturn` True
      readSymbolicLink src `shouldReturn` alien_target
      df <- mkDotfile env "~/.alien_link"
      exists (dfDst df) `shouldReturn` True
      dfStatus df `shouldBe` Alien

    it "covers two equal files (pending right)" $ \env -> do
      let src = envRoot env </> ".equal"
      let dst = envStorage env </> ".equal"
      writeFile src "same content"
      writeFile dst "same content"
      df <- mkDotfile env "~/.equal"
      exists (dfSrc df) `shouldReturn` True
      exists (dfDst df) `shouldReturn` True
      cmp src dst `shouldReturn` True
      dfStatus df `shouldBe` PendingRight

    it "covers when src file is a valid link and there is no dst (pending left)" $ \env -> do
      let src = envRoot env </> ".link"
      let target = envRoot env </> ".target"
      writeFile target "target content"
      createSymbolicLink target src
      df <- mkDotfile env "~/.link"
      exists (dfSrc df) `shouldReturn` True
      exists (dfDst df) `shouldReturn` False
      dfStatus df `shouldBe` PendingLeft

    it "covers broken link (pending right)" $ \env -> do
      let src = envRoot env </> ".broken_link"
      let doesnotexist = envRoot env </> "doesnotexist"
      let dst = envStorage env </> ".broken_link"
      writeFile dst "stored file content"
      writeFile doesnotexist ""
      createSymbolicLink doesnotexist src
      exists src `shouldReturn` True
      readSymbolicLink src `shouldReturn` doesnotexist
      rm doesnotexist
      exists doesnotexist `shouldReturn` False
      df <- mkDotfile env "~/.broken_link"
      exists (dfDst df) `shouldReturn` True
      dfStatus df `shouldBe` PendingRight

    it "covers broken link (invalid)" $ \env -> do
      let src = envRoot env </> ".broken_link"
      let doesnotexist = envRoot env </> "doesnotexist"
      let dst = envStorage env </> ".broken_link"
      writeFile dst "stored file content"
      writeFile doesnotexist ""
      createSymbolicLink doesnotexist src
      exists src `shouldReturn` True
      readSymbolicLink src `shouldReturn` doesnotexist
      rm doesnotexist
      rm dst
      exists dst `shouldReturn` False
      exists doesnotexist `shouldReturn` False
      df <- mkDotfile env "~/.broken_link"
      dfStatus df `shouldBe` Invalid

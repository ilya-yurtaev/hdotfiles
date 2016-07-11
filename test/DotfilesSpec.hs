module DotfilesSpec where

import System.Posix (createSymbolicLink)
import Test.Hspec

import Dotfiles
import Dotfiles.Commands
import Dotfiles.Utils

import SpecHelper (exists, withTmpDir)



spec :: Spec
spec = describe "Dotfiles" $ do
  around withTmpDir $ do
    it "env init" $ \tmpDir -> do
      let env = mkEnv tmpDir
      putStrLn $ show env -- covers show env
      exists (envAppDir env) `shouldReturn` False
      runCommand env install []
      exists (envAppDir env) `shouldReturn` True
      exists (envCfgPath env) `shouldReturn` True

    it "determines dotfiles status" $ \tmpDir -> do
      let env = mkEnv tmpDir
      runCommand env install []
      let valid = "~/.validrc"
      -- no src and dst - so it's invalid
      df <- mkDotfile env valid
      dfStatus df `shouldBe` Invalid
      dfName df `shouldBe` valid
      exists (dfSrc df) `shouldReturn` False
      writeFile (dfDst df) ""
      exists (dfDst df) `shouldReturn` True
      link df
      exists (dfSrc df) `shouldReturn` True
      -- this one must be tracked
      validDf <- mkDotfile env valid
      dfStatus validDf `shouldBe` Tracked
      -- make PendingLeft: src is solid file. and no dst. that's all
      unlink validDf
      exists (dfDst validDf) `shouldReturn` True -- keep dst
      pendingL <- mkDotfile env valid
      dfStatus pendingL `shouldBe` PendingRight -- src and dst are equal
      -- make PendingRight: there is no src and valid dst
      sync pendingL
      rm (dfSrc pendingL)
      pendingR <- mkDotfile env valid
      dfStatus pendingR `shouldBe` PendingRight
      -- make Conflict: there is some solid file instead of link. dst is present
      writeFile (dfSrc pendingR) "adfs"
      conflictDf <- mkDotfile env (dfSrc pendingR)
      dfStatus conflictDf `shouldBe` Conflicts
      -- PendingRight when src and dst are solid files and they are equal
      let same_file = "~/samefile"
      two_versions <- mkDotfile env same_file
      writeFile (dfSrc two_versions) "adfs"
      writeFile (dfDst two_versions) "adfs"
      resolveDf <- mkDotfile env (dfSrc two_versions)
      dfStatus resolveDf `shouldBe` PendingRight
      -- alien
      rm (dfSrc conflictDf)
      let alien_target = (normalize (envRoot env) "~/alien")
      let target = (normalize (envRoot env) "~/target")
      writeFile alien_target ""
      writeFile target ""
      alienDf <- mkDotfile env target
      sync alienDf
      rm (dfSrc alienDf)
      createSymbolicLink alien_target (dfSrc alienDf)
      alienDf' <- mkDotfile env target
      dfStatus alienDf' `shouldBe` Alien

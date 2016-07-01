module Dotfiles.UtilsSpec where

import Control.Monad.IO.Class (liftIO)
import Dotfiles.Utils
import System.Directory (getTemporaryDirectory)
import System.FilePath (joinPath)
import Test.Hspec
  

spec :: Spec
spec = do
     describe "Utils" $ do
       it "create\\remove dirs\\files" $ do
          tmp <- getTemporaryDirectory
          let testdir = joinPath [tmp, "somedir", "subdir"]
          let testfile = joinPath [tmp, "somefile"]
          mkdir testdir
          writeFile testfile ""
          cp testfile (joinPath [testdir, "somefile"])
          mv (joinPath [testdir, "somefile"]) testfile
          rm testfile
          rm testdir


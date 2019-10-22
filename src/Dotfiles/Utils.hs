module Dotfiles.Utils
    ( mkdir
    , rm
    , cp
    , mv
    , cmp
    , normalize
    , denormalize
    ) where

import Control.Monad (when)
import Data.String.Utils (strip, replace)
import System.Directory
  ( removeFile
  , renameFile
  , removeDirectoryRecursive
  , renameDirectory
  , doesDirectoryExist
  , doesFileExist
  , createDirectoryIfMissing
  , copyFile
  , getDirectoryContents
  )
import System.FilePath (joinPath, normalise, takeDirectory, (</>))


-- | alias to 'createDirectoryIfMissing True' -- creates missing parents too
mkdir :: FilePath -> IO ()
mkdir = createDirectoryIfMissing True

-- | Removes file or directory (recursive).
--   Doesn't rise exception if target doesn't exist
rm :: FilePath -> IO ()
rm p = do
  f <- doesFileExist p
  if f
    then removeFile p
    else do
      d <- doesDirectoryExist p
      when d $ removeDirectoryRecursive p


-- | Moves file or directory.
--   Doesn't rise exception if target doesn't exist
mv :: FilePath -> FilePath -> IO ()
mv from to = do
  f <- doesFileExist from
  if f
    then mvF from to
    else do
      d <- doesDirectoryExist from
      when d $ mvD from to
 where
  mvF f t = do
    mkdir (takeDirectory t)
    renameFile f t
  mvD f t = do
    mkdir (takeDirectory t)
    renameDirectory f t


-- | Copies file or directory.
--   Doesn't rise exception if target doesn't exist
cp :: FilePath -> FilePath -> IO ()
cp from to = do
  f <- doesFileExist from
  if f
    then cpF from to
    else do
      d <- doesDirectoryExist from
      when d $ cpD from to
 where
  cpF f t = do
    mkdir (takeDirectory t)
    copyFile f t
  cpD f t = do
    mkdir t
    entries <- filter (`notElem` [".", ".."]) `fmap` getDirectoryContents f
    mapM_ (\x -> cp (f </> x) (t </> x)) entries


-- | Compares two files by content
cmp :: FilePath -> FilePath -> IO Bool
cmp src dst = do
  f1 <- readFile src
  f2 <- readFile dst
  case compare f1 f2 of
    EQ -> return True
    _  -> return False


-- | converts \/'Dotfiles.envRoot'\/.vimrc to ~\/.vimrc
denormalize :: FilePath -> FilePath -> String
denormalize basePath = replace basePath "~"


-- | Converts ~\/.vimrc to \/'Dotfiles.envRoot'\/.vimrc
normalize :: FilePath -> String -> FilePath
normalize basePath fp = case normalise $ strip fp of
  "." -> []
  xs  -> strip' . expand $ xs
   where

    expand ('~':'/':fp') = joinPath [basePath, fp']
    expand "~"           = basePath
    expand fp'           = fp'

    strip' fp' = case reverse fp' of
      ('*':'/':fp'') -> reverse fp''
      ('/'    :fp'') -> reverse fp''
      fp''           -> reverse fp''

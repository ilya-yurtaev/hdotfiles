module Dotfiles.Utils where

import Control.Monad (when)
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
import System.FilePath (takeDirectory, (</>))


mkdir :: FilePath -> IO ()
mkdir = createDirectoryIfMissing True

rm :: FilePath -> IO ()
rm p = do
  f <- doesFileExist p
  if f then removeFile p else do
    d <- doesDirectoryExist p
    when d $ removeDirectoryRecursive p


mv :: FilePath -> FilePath -> IO ()
mv from to = do
  f <- doesFileExist from
  if f then mvF from to else do
    d <- doesDirectoryExist from
    when d $ mvD from to
    where
      mvF f t = do
        mkdir (takeDirectory t)
        renameFile f t
      mvD f t = do
        mkdir (takeDirectory t)
        renameDirectory f t


cp :: FilePath -> FilePath -> IO ()
cp from to = do
  f <- doesFileExist from
  if f then cpF from to else do
    d <- doesDirectoryExist from
    when d $ cpD from to
  where
    cpF f t = do
      mkdir (takeDirectory t)
      copyFile f t
    cpD f t = do
      mkdir t
      entries <- filter (`notElem` [".", ".."]) <$> getDirectoryContents f
      mapM_ (\x -> cp (f </> x) (t </> x)) entries


_first :: [IO Bool] -> IO Bool
_first = (or <$>) . sequence


_all :: [IO Bool] -> IO Bool
_all = (and <$>) . sequence


exists :: FilePath -> IO Bool
exists path = _first [doesFileExist path, doesDirectoryExist path]

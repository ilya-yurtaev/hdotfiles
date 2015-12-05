module Dotfiles where

import Control.Exception (catch, SomeException(..))
import Control.Monad (filterM, liftM)
import qualified Data.Set as Set
import Data.Set hiding (valid, filter)
import Data.String.Utils (strip, replace)
import System.Directory (getHomeDirectory, getTemporaryDirectory)
import System.FilePath (normalise, (</>), joinPath)
import System.Posix (readSymbolicLink, createSymbolicLink)


import Dotfiles.Utils


data Dotfile = Dotfile {
    dfName :: String
  , dfSrc :: FilePath
  , dfDst :: FilePath
} deriving (Show, Eq, Ord)

type Dotfiles = Set Dotfile


data Env = Env {
    envHome :: FilePath
  , envRoot :: FilePath
  , envFilesDir :: FilePath
  , envCfgPath :: FilePath
  , envTmpCfgPath :: FilePath
  , envBackupDir :: FilePath
} deriving (Show, Eq)


-- | It's actually .dotconfig content
type Names = Set String


defaultCfg :: String
defaultCfg = ""


getEnv :: IO Env
getEnv = do
    home <- getHomeDirectory
    tmp <- getTemporaryDirectory `catch` (\(SomeException _) -> return home)
    let envRoot' = home </> ".dotfiles"
        cfgPath = envRoot' </> ".dotconfig"
        tmpCfgPath = tmp </> ".dotconfig"
        in return Env {
              envHome=home
            , envRoot=envRoot'
            , envFilesDir=envRoot' </> "files"
            , envCfgPath=cfgPath
            , envTmpCfgPath=tmpCfgPath
            , envBackupDir=envRoot' </> "backups"
        }


readCfg :: Env -> IO Names
readCfg env = (Set.fromList . lines) <$> readFile (envCfgPath env)


mkDotfile :: Env -> String -> Dotfile
mkDotfile env name = Dotfile {
      dfName=name'
    , dfSrc=src
    , dfDst=dst
    } where src = normalize env name
            dst = replace (envHome env) (envFilesDir env) src
            name' = denormalize env src


mkDotfiles :: Env -> Names -> Dotfiles
mkDotfiles env = Set.map (mkDotfile env)


toString :: Env -> Dotfile -> String
toString env df = denormalize env (dfSrc df)


unpack :: Env -> Dotfiles -> [String]
unpack env = fmap (toString env) . Set.toList


denormalize :: Env -> FilePath -> String
denormalize env = replace (envHome env) "~"


normalize :: Env -> String -> FilePath
normalize env fp = case normalise $ strip fp of
  [] -> []
  xs -> strip' . expand $ xs where

    expand ('~':'/':fp') = joinPath [envHome env, fp']
    expand fp' = joinPath [envHome env, fp']

    strip' fp' = case reverse fp' of
        ('*':'/':fp'') -> reverse fp''
        ('/':fp'') -> reverse fp''
        fp'' -> reverse fp''


-- actions
sync :: Dotfile -> IO ()
sync df = do
    cp (dfSrc df) (dfDst df)
    rm (dfSrc df)
    link df


link :: Dotfile -> IO ()
link df = createSymbolicLink (dfDst df) (dfSrc df)


unlink :: Dotfile -> IO ()
unlink df = rm (dfSrc df) >> mv (dfDst df) (dfSrc df)


backup :: Env -> Dotfile -> IO ()
backup env df = cp (dfSrc df) (envBackupDir env) >> link df


-- filters
filter' :: (Dotfile -> IO Bool) -> Dotfiles -> IO Dotfiles
filter' p xs = filterM p (Set.toList xs) >>= \dfs -> return $ Set.fromList dfs


valid :: Dotfile -> IO Bool
valid df = _first [exists (dfSrc df), exists (dfDst df)]


invalid :: Dotfile -> IO Bool
invalid = (not <$>) `liftM` valid


linked :: Dotfile -> IO Bool
linked df = readSymbolicLink (dfSrc df)
            `catch` (\(SomeException _) -> return "") >>=
                \lnk -> return $ lnk == dfDst df


tracked :: Dotfile -> IO Bool
tracked df = _all [valid df, linked df]


pending :: Dotfile -> IO Bool
pending df = _all [valid df, not <$> tracked df]

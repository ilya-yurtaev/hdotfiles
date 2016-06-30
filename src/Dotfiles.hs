{-# LANGUAGE OverloadedStrings #-}

module Dotfiles where

import           Control.Exception (catch)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Utils (strip, replace)
import           Dotfiles.Utils
import           GHC.IO.Exception
import           System.Directory (doesFileExist)
import           System.FilePath (normalise, (</>), joinPath)
import           System.IO.Error
import           System.Posix (readSymbolicLink, createSymbolicLink)


data DotfileStatus =
    PendingRight -- ^ no src link is present or link is broken, dst exists
  | PendingLeft  -- ^ no dst file is present, src exists (it's a file or valid link to file)
  | Tracked      -- ^ src is link which leads to valid tracked dst
  | Conflicts    -- ^ src is solid file, dst is present and they are not equal
  | Alien        -- ^ src link leads to not tracked dst, dst is not present
  | Invalid      -- ^ src is not present or it's a broken link, dst is not present
  | Unknown      -- ^ not checked yet or something really weird is happening
  deriving (Show, Eq, Ord)


data Dotfile = Dotfile
  { dfName :: String          -- ^ it's a short path of df, e.g. ~/.vimrc
  , dfSrc :: FilePath         -- ^ expanded path to soft link: /home/user/.vimrc
  , dfDst :: FilePath         -- ^ expanded path to tracked file: /home/user/.dotfiles_path/.vimrc
  , dfStatus :: DotfileStatus -- ^ status
  } deriving (Show, Eq, Ord)


type Dotfiles = Set Dotfile


data Env = Env
  { envHome :: FilePath
  , envRoot :: FilePath
  , envFilesDir :: FilePath
  , envCfgPath :: FilePath
  , envBackupDir :: FilePath
  } deriving (Show, Eq)


-- | It's actually .dotconfig content
type Names = Set String


getDotfileStatus :: FilePath -> FilePath -> IO DotfileStatus
getDotfileStatus src dst = do
  lnk <- readSymbolicLink src `catch` handleLinkError
  copied <- doesFileExist dst
  lnk_src <- doesFileExist lnk
  return (determine lnk copied lnk_src)
  where determine "doesNotExist" True _  = PendingRight
        determine "doesNotExist" False _ = Invalid
        determine "isFile" True _        = Conflicts
        determine "isFile" False _       = PendingLeft
        determine "" _ _                 = Unknown
        determine somelink True True | somelink == dst = Tracked
                                     | otherwise       = Alien -- link leads to different dst
        determine _ False True  = PendingLeft -- src link is valid, dst is not present
        determine _ True False  = PendingRight -- src link is broken, dst is present
        determine _ False False = Invalid

        handleLinkError e | isDoesNotExistError e = return "doesNotExist"
                          | isInvalidLink e       = return "isFile"
                          | otherwise             = return ""


defaultCfg :: String
defaultCfg = ""


mkEnv :: FilePath -> Env
mkEnv home =
  let root    = home </> ".hdotfiles"
      cfgPath = root </> ".dotconfig"
    in Env
      { envHome      = home
      , envRoot      = root
      , envFilesDir  = root </> "files"
      , envCfgPath   = cfgPath
      , envBackupDir = root </> "backups"
      }


readCfg :: Env -> IO Names
readCfg env = (Set.fromList . lines) `fmap` readFile (envCfgPath env)


mkDotfile :: Env -> String -> IO Dotfile
mkDotfile env name = do
  status <- getDotfileStatus src dst
  return Dotfile
    { dfName   = name'
    , dfSrc    = src
    , dfDst    = dst
    , dfStatus = status
    }
  where src   = normalize env name
        dst   = replace (envHome env) (envFilesDir env) src
        name' = denormalize env src


mkDotfiles :: Env -> Names -> IO Dotfiles
mkDotfiles env = fmap Set.fromList . mapM (mkDotfile env) . Set.toList


denormalize :: Env -> FilePath -> String
denormalize env = replace (envHome env) "~"


normalize :: Env -> String -> FilePath
normalize env fp = case normalise $ strip fp of
  []  -> []
  "." -> []
  xs  -> strip' . expand $ xs where

    expand ('~':'/':fp') = joinPath [envHome env, fp']
    expand "~"           = envHome env
    expand fp'           = joinPath [envHome env, fp']

    strip' fp' = case reverse fp' of
        ('*':'/':fp'') -> reverse fp''
        ('/':fp'')     -> reverse fp''
        fp''           -> reverse fp''


-- actions
sync :: Dotfile -> IO ()
sync df =
  case dfStatus df of
    PendingRight -> do
      rm (dfSrc df) -- if there is a link -- it's 100% broken
      link df
    PendingLeft  -> do
      cp (dfSrc df) (dfDst df)
      rm (dfSrc df)
      link df
    _            -> return ()


link :: Dotfile -> IO ()
link df = createSymbolicLink (dfDst df) (dfSrc df)


unlink :: Dotfile -> IO ()
unlink df =
  case dfStatus df of
    Tracked -> do
      rm (dfSrc df)
      mv (dfDst df) (dfSrc df)
    _       -> return ()


backup :: Env -> Dotfile -> IO ()
backup env df = cp (dfSrc df) (envBackupDir env)


isInvalidLink :: IOError -> Bool
isInvalidLink = isInvalidArgument . ioeGetErrorType
  where isInvalidArgument InvalidArgument = True
        isInvalidArgument _               = False

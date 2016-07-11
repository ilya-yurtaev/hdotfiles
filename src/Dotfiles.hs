{-# LANGUAGE OverloadedStrings #-}

module Dotfiles where

import           Control.Exception (catch)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Utils (replace)
import           Dotfiles.Utils
import           GHC.IO.Exception
import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError, ioeGetErrorType)
import           System.Posix (readSymbolicLink, createSymbolicLink)


data DotfileStatus =
    PendingRight -- ^ no `dfSrc` link is present or link is broken, `dfDst` exists
  | PendingLeft  -- ^ no `dfDst` file is present, `dfSrc` exists (it's a file or valid link to file)
  | Tracked      -- ^ `dfSrc` is link which leads to valid tracked `dfDst`
  | Conflicts    -- ^ `dfSrc` is solid file, `dfDst` is present and they are not equal
  | Alien        -- ^ `dfSrc` link leads to not tracked destination, `dfDst` is not present
  | Invalid      -- ^ `dfSrc` is not present or it's a broken link, `dfDst` is not present
  | Unknown      -- ^ not checked yet or something really weird is happening
  deriving (Show, Eq, Ord)


data Dotfile = Dotfile
  { dfName :: String          -- ^ relative to `envRoot` path: ~\/.vimrc
  , dfSrc :: FilePath         -- ^ expanded path to soft link: \/`envRoot`\/.vimrc
  , dfDst :: FilePath         -- ^ expanded path to solid file: \/`envRoot`\/`envAppDir`\/.vimrc
  , dfStatus :: DotfileStatus
  } deriving (Show, Eq, Ord)


type Dotfiles = Set Dotfile


data Env = Env
  { envRoot :: FilePath      -- ^ root path for links, it should be %HOME%, so it is by default
  , envAppDir :: FilePath    -- ^ application directory, it is \/`envRoot`\/.hdotfiles/ by default
  , envTmp :: FilePath       -- ^ system %TEMP%
  , envStorage :: FilePath   -- ^ directory where target files stored, by default it is \/`envRoot`\/`envAppDir`\/.files
  , envCfgPath :: FilePath   -- ^ path to config, \/`envRoot`\/.dotconfig by default
  , envBackupDir :: FilePath -- ^ \/`envRoot`\/`envAppDir`\/backups
  } deriving (Show)


-- | It's actually .dotconfig content
type Names = Set String


-- | Main magic
getDotfileStatus :: FilePath -> FilePath -> IO DotfileStatus
getDotfileStatus src dst = do
  lnk <- readSymbolicLink src `catch` handleLinkError
  copied <- doesFileExist dst
  determine lnk copied
  where determine "doesNotExist" True  = return PendingRight
        determine "doesNotExist" False = return Invalid
        determine "isFile" True = do
          sameFile <- cmp src dst 
          return $ if sameFile
                      then PendingRight
                      else Conflicts
        determine "isFile" False       = return PendingLeft
        determine "" _                 = return Unknown
        determine somelink True = do
          lnk_src_exist <- doesFileExist somelink
          return $ if lnk_src_exist
                      then 
                         if somelink == dst
                           then Tracked
                           else Alien
                      else PendingRight -- broken link
        determine somelink False = do
          lnk_src_exist <- doesFileExist somelink
          return $ if lnk_src_exist
                      then PendingLeft
                      else Invalid -- link is broken, no dst

        handleLinkError e | isDoesNotExistError e = return "doesNotExist"
                          | isInvalidLink e       = return "isFile"
                          | otherwise             = return ""

        isInvalidLink :: IOError -> Bool
        isInvalidLink = isInvalidArgument . ioeGetErrorType
          where isInvalidArgument InvalidArgument = True
                isInvalidArgument _               = False


mkEnv :: FilePath -> Env
mkEnv root =
  let appDir = root </> ".dotfiles"
      in Env
        { envRoot      = root
        , envAppDir    = appDir
        , envTmp       = appDir
        , envStorage   = appDir </> "files"
        , envCfgPath   = root </> ".dotconfig"
        , envBackupDir = appDir </> "backups"
        }


readCfg :: Env -> IO Names
readCfg env = (Set.fromList . filter (not . null) . lines) `fmap` readFile (envCfgPath env)


mkDotfile :: Env -> String -> IO Dotfile
mkDotfile env name = do
  status <- getDotfileStatus src dst
  return Dotfile
    { dfName   = name'
    , dfSrc    = src
    , dfDst    = dst
    , dfStatus = status
    }
  where src   = normalize home name
        dst   = replace home (envStorage env) src
        name' = denormalize home src
        home = envRoot env


mkDotfiles :: Env -> Names -> IO Dotfiles
mkDotfiles env = fmap Set.fromList . mapM (mkDotfile env) . Set.toList


unpack :: Dotfiles -> [String]
unpack = fmap dfName . Set.toList

-- actions
sync :: Dotfile -> IO ()
sync df =
  case dfStatus df of
    PendingRight -> do
      rm (dfSrc df) -- if there is a link -- it's 100% broken
      link df
    PendingLeft  -> do
      cp (dfSrc df) (dfDst df) -- it will overwrite dfDst if it exist
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
      cp (dfDst df) (dfSrc df)
    _       -> return ()


backup :: Env -> Dotfile -> IO ()
backup env df = cp (dfSrc df) (envBackupDir env)

module Dotfiles where

import           Control.Exception (catch)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Utils (replace)
import           Dotfiles.Utils
import           GHC.IO.Exception
import           System.Directory (doesFileExist)

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
  { dfName :: String          -- ^ short path relative to `envRoot`: ~\/.vimrc
  , dfSrc :: FilePath         -- ^ absolute path to soft link: \/`envRoot`\/.vimrc
  , dfDst :: FilePath         -- ^ absolute path to solid file: \/`envRoot`\/`envAppDir`\/.vimrc
  , dfStatus :: DotfileStatus -- ^ status
  } deriving (Eq, Ord)


type Dotfiles = Set Dotfile


type Names = Set String


data MaybeLink l = Link l | DoesNotExist | InvalidLink | Other


data Env = Env
  { envRoot :: FilePath      -- ^ root path for links, it is %HOME% by default
  , envAppDir :: FilePath    -- ^ application directory, it is \/`envRoot`\/.hdotfiles/ by default
  , envTmp :: FilePath       -- ^ system %TEMP%
  , envStorage :: FilePath   -- ^ directory where target files stored, by default it is \/`envRoot`\/`envAppDir`\/.files
  , envCfgPath :: FilePath   -- ^ config path. it __must__ be \/`envRoot`\/.dotconfig.yml
  , envDotfiles :: Names     -- ^ Set of dotfiles names
  , envBackupDir :: FilePath -- ^ \/`envRoot`\/`envAppDir`\/backups
  }


-- | Main magic
getDotfileStatus :: FilePath -> FilePath -> IO DotfileStatus
getDotfileStatus src dst = do
  lnk    <- (Link <$> readSymbolicLink src) `catch` handleLinkError
  copied <- doesFileExist dst
  determine lnk copied
 where
  determine DoesNotExist True  = return PendingRight
  determine DoesNotExist False = return Invalid
  determine InvalidLink  True  = do
    sameFile <- cmp src dst
    return $ if sameFile then PendingRight else Conflicts
  determine InvalidLink     False = return PendingLeft
  determine Other           _     = return Unknown
  determine (Link somelink) True  = do
    lnk_src_exist <- doesFileExist somelink
    return $ if lnk_src_exist
      then if somelink == dst then Tracked else Alien
      else PendingRight -- broken link
  determine (Link somelink) False = do
    lnk_src_exist <- doesFileExist somelink
    return $ if lnk_src_exist then PendingLeft else Invalid -- link is broken, no dst

  handleLinkError e | isDoesNotExistError e = return DoesNotExist
                    | isInvalidLink e       = return InvalidLink
                    | otherwise             = return Other

  isInvalidLink :: IOError -> Bool
  isInvalidLink = isInvalidArgument . ioeGetErrorType
   where
    isInvalidArgument InvalidArgument = True
    isInvalidArgument _               = False


mkDotfile :: Env -> String -> IO Dotfile
mkDotfile env name = do
  status <- getDotfileStatus src dst
  return Dotfile {dfName = name', dfSrc = src, dfDst = dst, dfStatus = status}
 where
  src   = normalize home name
  dst   = replace home (envStorage env) src
  name' = denormalize home src
  home  = envRoot env


mkDotfiles :: Env -> Names -> IO Dotfiles
mkDotfiles env = fmap Set.fromList . mapM (mkDotfile env) . Set.toList


unpack :: Dotfiles -> [String]
unpack = fmap dfName . Set.toList

-- actions
sync :: Dotfile -> IO ()
sync df = case dfStatus df of
  PendingRight -> do
    rm (dfSrc df) -- if there is a link -- it's 100% broken
    link df
  PendingLeft -> do
    rm (dfDst df)
    cp (dfSrc df) (dfDst df)
    rm (dfSrc df)
    link df
  _ -> return ()


link :: Dotfile -> IO ()
link df = createSymbolicLink (dfDst df) (dfSrc df)


unlink :: Dotfile -> IO ()
unlink df = case dfStatus df of
  Tracked -> do
    rm (dfSrc df)
    cp (dfDst df) (dfSrc df)
  _ -> return ()


backup :: Env -> Dotfile -> IO ()
backup env df = mv (dfSrc df) (envBackupDir env)

module Dotfiles.Commands where


import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String.Utils (replace)
import qualified Data.Version as Version (showVersion)
import           System.Directory (setCurrentDirectory)
import           System.Environment (getArgs)

import           System.Posix (createSymbolicLink)
import           System.Process (system)

import           Dotfiles
import           Dotfiles.Config
import           Dotfiles.Utils

import           Paths_hdotfiles (version)


type Args = [String]

type Action = ReaderT (Env, Args) IO

type Command = Action ()


fromConfig :: Action Dotfiles
fromConfig = do
  (env, _) <- ask
  liftIO $ mkDotfiles env (envDotfiles env)


fromArgs :: Action Dotfiles
fromArgs = do
  (env, args) <- ask
  liftIO $ mkDotfiles env (Set.fromList args)


runCommand :: Env -> Command -> Args -> IO ()
runCommand env cmd args = runReaderT cmd (env, args)


install :: Command
install = do
  (env, args) <- ask
  case args of
    []          -> doSimpleInstall
    -- TODO (x:y:_) -> gitClone x y
    (repoURL:_) -> gitClone repoURL (envAppDir env)
 where
  doSimpleInstall = do
    (env, _) <- ask
    liftIO $ mapM_ mkdir [envAppDir env, envStorage env]
    dotfiles <- fromConfig
    liftIO $ mapM_ sync (Set.toList dotfiles)

  gitClone repo localDir = do
    (env, _) <- ask
    liftIO $ setCurrentDirectory (envRoot env)
    liftIO $ mkdir (envBackupDir env)
    call $ unwords ["git clone", repo, normalize (envRoot env) localDir]
    liftIO $ createSymbolicLink
      (replace (envStorage env) (envRoot env) (envCfgPath env))
      (envCfgPath env) -- link .dotconfig.yml first
    dotfiles <- Set.toList `fmap` fromConfig
    liftIO $ mapM_ (backup env) dotfiles
    liftIO $ mapM_ link dotfiles


uninstall :: Command
uninstall = fromConfig >>= liftIO . mapM_ unlink . Set.toList


addDotfiles :: Command
addDotfiles = do
  dotfiles   <- fromConfig
  candidates <- fromArgs
  save $ Set.union dotfiles candidates
  liftIO $ mapM_ sync (Set.toList $ Set.difference candidates dotfiles)


forgetDotfiles :: Command
forgetDotfiles = do
  dotfiles   <- fromConfig
  candidates <- fromArgs
  save $ Set.difference dotfiles candidates
  liftIO $ mapM_ unlink (Set.toList candidates)


syncDotfiles :: Command
syncDotfiles = fromConfig >>= liftIO . mapM_ sync . Set.toList


resolve :: Command
resolve = do
  (_, args) <- ask
  dotfiles  <-
    (filter (\df -> dfStatus df == Conflicts) . Set.toList) `fmap` fromConfig
  case args of
    (cmd:_) -> case cmd of
      "local" -> do
        let dfs = map (\df -> df { dfStatus = PendingLeft }) dotfiles
        sync' dfs

      "remote" -> do
        let dfs = map (\df -> df { dfStatus = PendingRight }) dotfiles
        sync' dfs

      _ -> resolveHelp
    _ -> resolveHelp
 where
  resolveHelp = liftIO $ putStrLn $ unlines
    [ "possible args to `resolve` are:"
    , "\tlocal  -- keep local files (ACHTUNG! remote files will be replaced)"
    , "\tremote -- use remote files (ACHTUNG! local files will be replaced)"
    ]
  sync' = liftIO . mapM_ sync


gitCommitAndPush :: Command
gitCommitAndPush = do
  (env, args) <- ask
  liftIO . setCurrentDirectory $ envAppDir env
  call "git add ."
  call $ unwords ["git commit -am ", msg args]
  call "git push"
  call "git status"
 where
  msg args = case args of
    [] -> show "regular update"
    xs -> unwords xs


call :: String -> Command
call = liftIO . void . system


showStatus :: Command
showStatus = do
  (env, _) <- ask

  dotfiles <- fromConfig
  let tracked' = Set.filter ((== Tracked) . dfStatus) dotfiles
  let pending' = Set.filter
        (\df -> dfStatus df == PendingLeft || dfStatus df == PendingRight)
        dotfiles
  let invalid'  = Set.filter ((== Invalid) . dfStatus) dotfiles
  let conflict' = Set.filter ((== Conflicts) . dfStatus) dotfiles
  let alien'    = Set.filter ((== Alien) . dfStatus) dotfiles
  let unknown'  = Set.filter ((== Unknown) . dfStatus) dotfiles

  liftIO $ mapM_
    putStrLn
    (  pprint "Tracked:"  tracked'
    ++ pprint "Pending:"  pending'
    ++ pprint "Conflict:" conflict'
    ++ pprint "Invalid:"  invalid'
    ++ pprint "Alien:"    alien'
    ++ pprint "Unknown:"  unknown'
    )

  liftIO $ setCurrentDirectory (envAppDir env)
  call "git status"
 where
  pprint header dfs | Set.null dfs = []
                    | otherwise    = header : tab dfs
  tab xs = fmap ("\t" ++) (unpack xs)


save :: Dotfiles -> Command
save dfs = do
  (env, _) <- ask
  liftIO $ saveConfig env (unpack dfs)


commands :: Map.Map String Command
commands = Map.fromList
  [ ("add"      , addDotfiles)
  , ("commit"   , gitCommitAndPush)
  , ("forget"   , forgetDotfiles)
  , ("install"  , install)
  , ("resolve"  , resolve)
  , ("status"   , showStatus)
  , ("sync"     , syncDotfiles)
  , ("uninstall", uninstall)
  ]


showHelp :: IO ()
showHelp = do
  putStrLn $ unwords ["hdotfiles ", Version.showVersion version]
  mapM_ putStrLn $ "Available commands:" : fmap ("\t- " ++) (Map.keys commands)


runApp :: IO ()
runApp = do
  args <- getArgs
  env  <- defaultEnv
  case args of
    []     -> showHelp
    (x:xs) -> case Map.lookup x commands of
      Just cmd -> runCommand env cmd xs
      Nothing  -> showHelp

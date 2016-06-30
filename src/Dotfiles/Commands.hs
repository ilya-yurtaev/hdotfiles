{-# LANGUAGE OverloadedStrings #-}
module Dotfiles.Commands where

import           Control.Exception (catch, SomeException(..))
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           System.Directory
  ( getHomeDirectory
  , setCurrentDirectory
  , getTemporaryDirectory)
import           System.Environment (getArgs)
import           System.FilePath
import           System.Process (system)

import           Dotfiles
import           Dotfiles.Utils


type Args = [String]

type Action = ReaderT (Env, Args) IO

type Command = Action ()


fromConfig :: Action Dotfiles
fromConfig = do
  (env, _) <- ask
  cfg <- liftIO (readCfg env)
  liftIO $ mkDotfiles env cfg


fromArgs :: Action Dotfiles
fromArgs = do
  (env, args) <- ask
  liftIO $ mkDotfiles env (Set.fromList args)


runCommand :: Command -> Args -> IO ()
runCommand cmd args = do
  env <- mkEnv `fmap` getHomeDirectory
  runReaderT cmd (env, args)


install :: Command
install = do
  (env, args) <- ask
  case args of
    [] -> doSimpleInstall
    -- TODO (x:y:_) -> gitClone x y
    (repoURL:_) -> gitClone repoURL (envRoot env)
  where 
    doSimpleInstall = do
        (env, _) <- ask
        liftIO $ mapM_ mkdir [envRoot env, envFilesDir env]
        _ <- liftIO $ readCfg env
            `catch` (\(SomeException _) -> do
                        writeFile (envCfgPath env) defaultCfg
                        readCfg env
                    )
        dotfiles <- fromConfig
        liftIO $ mapM_ sync dotfiles

    gitClone repo localDir = do
        (env, _) <- ask
        liftIO $ setCurrentDirectory (envHome env)
        liftIO $ mkdir (envBackupDir env)
        call $ unwords ["git clone", repo, normalize env localDir]
        dotfiles <- fromConfig
        liftIO $ mapM_ (backup env) dotfiles
        liftIO $ mapM_ link dotfiles


uninstall :: Command
uninstall = fromConfig >>= liftIO . mapM_ unlink


addDotfiles :: Command
addDotfiles = do
  dotfiles <- fromConfig
  candidates <- fromArgs
  save $ Set.union dotfiles candidates
  liftIO $ mapM_ sync (Set.difference candidates dotfiles)


forgetDotfiles :: Command
forgetDotfiles = do
  dotfiles <- fromConfig
  candidates <- fromArgs
  save $ Set.difference dotfiles candidates
  liftIO $ mapM_ unlink candidates


syncDotfiles :: Command
syncDotfiles = fromConfig >>= liftIO . mapM_ sync


gitCommitAndPush :: Command
gitCommitAndPush = do
  (env, args) <- ask
  liftIO . setCurrentDirectory $ envRoot env
  call "git add ."
  call $ unwords ["git commit -am ", msg args]
  call "git push"
  call "git status"
    where msg args = case args of
                    [] -> show "regular update"
                    xs -> unwords xs


call :: String -> Command
call = liftIO . void . system


showStatus :: Command
showStatus = do
  (env,  _) <- ask

  dotfiles <- fromConfig
  let tracked' = Set.filter ((==Tracked) . dfStatus) dotfiles
  let pending' = Set.filter (\df -> dfStatus df == PendingLeft || dfStatus df == PendingRight) dotfiles
  let invalid' = Set.filter ((==Invalid) . dfStatus) dotfiles
  let conflict' = Set.filter ((==Conflicts) . dfStatus) dotfiles
  let alien' = Set.filter ((==Alien) . dfStatus) dotfiles
  let unknown' = Set.filter ((==Unknown) . dfStatus) dotfiles

  liftIO $ mapM_ putStrLn (
    pprint "Tracked:" tracked' ++
    pprint "Pending:" pending' ++
    pprint "Conflict:" conflict' ++
    pprint "Invalid:" invalid' ++
    pprint "Alien:" alien' ++
    pprint "Unknown:" unknown'
    ) 

  liftIO $ setCurrentDirectory (envRoot env)
  call "git status"
    where
        pprint header dfs | Set.null dfs = []
                          | otherwise = header : tab dfs
        tab xs = fmap ("\t" ++) (map dfName $ Set.toList xs)


save :: Dotfiles -> Command
save dfs = do
  (env, _) <- ask
  tmpDir <- liftIO $ getTemporaryDirectory `catch` (\(SomeException _) -> return (envHome env))
  let cfgTmp = tmpDir </> ".dotconfig"
  liftIO $ writeFile cfgTmp (unlines $ map dfName $ Set.toList dfs)
  liftIO $ rm (envCfgPath env)
  liftIO $ mv cfgTmp (envCfgPath env)


commands :: Map.Map String Command
commands = Map.fromList
  [ ("add", addDotfiles)
  , ("commit", gitCommitAndPush)
  , ("forget", forgetDotfiles)
  , ("install", install)
  , ("status", showStatus)
  , ("sync", syncDotfiles)
  , ("uninstall", uninstall)
  ]


showHelp :: IO ()
showHelp =
  mapM_ putStrLn $ "Available commands:" : fmap ("\t- "++) (Map.keys commands)


runApp :: IO ()
runApp = do
  args <- getArgs
  case args of
    [] -> showHelp
    (x:xs) -> case Map.lookup x commands of
      Just cmd -> runCommand cmd xs
      Nothing -> showHelp

module Dotfiles.Commands where

import           Control.Exception (catch, SomeException(..))
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           System.Directory (setCurrentDirectory)
import           System.Environment (getArgs)
import           System.Process (system)

import           Dotfiles
import           Dotfiles.Utils


type Args = [String]

type Action = ReaderT (Env, Args) IO

type Command = Action ()

type MkDotfiles = Action Dotfiles


dotfiles :: (Dotfile -> IO Bool) -> MkDotfiles
dotfiles f = do
  (env, _) <- ask
  cfg <- liftIO $ readCfg env
  liftIO $ filter' f (mkDotfiles env cfg)


candidates :: MkDotfiles
candidates = do
  (env, args) <- ask
  return $ mkDotfiles env (Set.fromList args)


runCommand :: Command -> Args -> IO ()
runCommand cmd args = do
  env <- getEnv
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
        dfs <- dotfiles pending
        liftIO $ mapM_ sync dfs

    gitClone repo localDir = do
        (env, _) <- ask
        liftIO $ setCurrentDirectory (envHome env)
        call $ unwords ["git clone", repo, normalize env localDir]
        dotfiles pending >>= \dfs -> liftIO $ mapM_ (backup env) dfs >> mapM_ link dfs


uninstall :: Command
uninstall = dotfiles tracked >>= liftIO . mapM_ unlink


addDotfiles :: Command
addDotfiles = do
  dfs <- dotfiles tracked
  cnds <- candidates
  save $ Set.union dfs cnds
  liftIO $ mapM_ sync (Set.difference cnds dfs)


forgetDotfiles :: Command
forgetDotfiles = do
  dfs <- dotfiles tracked
  dfs2remove <- candidates
  save $ Set.difference dfs dfs2remove
  liftIO $ mapM_ unlink dfs2remove


syncDotfiles :: Command
syncDotfiles = dotfiles pending >>= liftIO . mapM_ sync


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
  liftIO $ setCurrentDirectory (envRoot env)
  tracked' <- dotfiles tracked
  pending' <- dotfiles pending
  invalid' <- dotfiles invalid
  liftIO $ mapM_ putStrLn (
    "Tracked:": tab env tracked' ++
    "Pending:": tab env pending' ++
    "Invalid:": tab env invalid'
    ) 
  call "git status"
    where
        tab :: Env -> Dotfiles -> [String]
        tab env xs = fmap ("\t" ++) (unpack env xs)


save :: Dotfiles -> Command
save dfs = do
  (env, _) <- ask
  liftIO $ writeFile (envTmpCfgPath env) (unlines $ unpack env dfs)
  liftIO $ rm (envCfgPath env)
  liftIO $ mv (envTmpCfgPath env) (envCfgPath env)


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

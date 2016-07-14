{-# LANGUAGE DeriveGeneric #-}

module Dotfiles.Config where

import           Control.Exception (catch, SomeException(..))
import           Data.Maybe
import qualified Data.Set as Set
import           Data.String.Utils (replace)
import           Data.Yaml
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           System.Directory (getHomeDirectory, getTemporaryDirectory)
import           System.FilePath ((</>))
import           System.Posix (readSymbolicLink)

import           Dotfiles
import           Dotfiles.Utils


data Config = Config
  { appDir :: Maybe String -- ^ Application directory where all original files are stored
  , dfNames :: [String]      -- ^ List of dotfiles 
  } deriving (Generic)


instance ToJSON Config where
instance FromJSON Config where
  

defaultConfig :: Config
defaultConfig = Config
  { appDir = Just "~/.dotfiles"
  , dfNames = []
  }
  

defaultEnv :: IO Env
defaultEnv = getHomeDirectory >>= readEnv

  
readEnv :: FilePath -> IO Env
readEnv root = do
  c <- Yaml.decodeFileEither cfgPath
  case c of
    Right cfg -> buildEnv cfg
    _ -> Yaml.encodeFile cfgPath defaultConfig >> buildEnv defaultConfig

  where cfgPath = root </> ".dotconfig.yml"
        buildEnv cfg = do
          tmpDir <- getTemporaryDirectory
          return Env
            { envRoot = root
            , envAppDir = appDir'
            , envTmp = tmpDir
            , envStorage = appDir' </> "files"
            , envCfgPath = cfgPath
            , envBackupDir = appDir' </> "backups"
            , envDotfiles = Set.fromList (dfNames cfg)
            }
          where appDir' = normalize root $ fromJust (appDir cfg)

readConfig :: FilePath -> IO Config
readConfig path = do
  c <- Yaml.decodeFileEither path
  case c of
    Right cfg -> return cfg
    _ -> error $ unwords ["Error reading ", path]


-- | alias to Yaml.encodeFile
writeConfig :: FilePath -> Config -> IO ()
writeConfig = Yaml.encodeFile
 

saveConfig :: Env -> [String] -> IO ()
saveConfig env names = do
  writeConfig tmpPath cfg
  cfgPath <- getTargetPath (envCfgPath env)
  rm cfgPath
  mv tmpPath cfgPath
  where cfg = Config {appDir = Just $ denormalize (envRoot env) (envAppDir env), dfNames = names}
        tmpPath = replace (envRoot env) (envTmp env) (envCfgPath env) 


getTargetPath :: FilePath -> IO FilePath
getTargetPath path = readSymbolicLink path `catch` (\(SomeException _) -> return path)

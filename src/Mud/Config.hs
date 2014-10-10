module Mud.Config
  ( Config(..)
  , parseConfigFiles
  , computeConfig
  ) where

import Control.Monad.Error

import Data.List

import System.Directory
import System.FilePath

import Paths_mud

import Mud.Error
import Mud.Options

data Config = Config
  { cfgDeployScript   :: FilePath
  , cfgUndeployScript :: FilePath
  , cfgBasePath       :: FilePath
  , cfgUser           :: Maybe String
  , cfgGroup          :: Maybe String
  , cfgVersion        :: String
  , cfgVars           :: [(String, String)]
  } deriving (Show, Eq)

defaultConfig :: FilePath -> Config
defaultConfig path = Config
  { cfgDeployScript   = path <.> "deploy"
  , cfgUndeployScript = path <.> "undeploy"
  , cfgBasePath       = "/tmp"
  , cfgUser           = Nothing
  , cfgGroup          = Nothing
  , cfgVersion        = ""
  , cfgVars           = []
  }

parseConfigFiles :: String -> ErrorT MudError IO [Config]
parseConfigFiles projectName = do
    sysconfdir <- liftIO $ getSysconfDir
    let configBasePath = sysconfdir </> "mud" </> projectName
        configFilePath = configBasePath <.> "conf"

    checkFileExistence <- liftIO $ doesFileExist configFilePath
    if checkFileExistence
      then do
        contents <- liftIO $ readFile configFilePath
        cfg      <- parseConfigFile configBasePath contents
        return [cfg]

      else do
        checkDirExistence <- liftIO $ doesDirectoryExist configBasePath
        if checkDirExistence
          then do
            paths <- liftIO $ getDirectoryContents configBasePath
            forM (sort paths) $ \path -> do
              contents <- liftIO $ readFile path
              parseConfigFile configBasePath contents
          else
            throwError $ MudErrorNoConfigFound configBasePath

  where
    parseConfigFile :: FilePath -> String -> ErrorT MudError IO Config
    parseConfigFile configBasePath =
      either throwError return
        . foldl' buildConfig (Right $ defaultConfig configBasePath)
        . map ((\(a,b) -> (a, drop 1 b)) . break (=='='))
        . filter (/="")
        . lines

    buildConfig :: Either MudError Config -> (String, String)
                -> Either MudError Config
    buildConfig err@(Left _) _ = err
    buildConfig (Right config) (name, value) = case name of
      "deploy"   -> Right config { cfgDeployScript   = value }
      "undeploy" -> Right config { cfgUndeployScript = value }
      "basepath" -> Right config { cfgBasePath       = value }
      "user"     -> Right config { cfgUser           = Just value }
      "group"    -> Right config { cfgGroup          = Just value }
      "version"  -> Right config { cfgVersion        = value }
      'v' : 'a' : 'r' : ':' : n ->
        let vars = filter ((/= n) . fst) $ cfgVars config
        in Right config { cfgVars = (n, value) : vars }
      _ -> Left $ MudErrorUnreadableConfig $ "Invalid variable '" ++ name ++ "'"

computeConfig :: Options -> Config -> Config
computeConfig opts = changeUser . changeGroup
  where
    changeUser  cfg = maybe cfg (\u -> cfg { cfgUser  = Just u })
                            (optUser  opts)
    changeGroup cfg = maybe cfg (\g -> cfg { cfgGroup = Just g })
                            (optGroup opts)

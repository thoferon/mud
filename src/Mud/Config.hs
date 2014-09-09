module Mud.Config
  ( Config(..)
  , parseConfigFile
  , computeConfig
  ) where

import Paths_mud

import Data.List

import System.FilePath

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

parseConfigFile :: String -> IO (Either String Config)
parseConfigFile projectName = do
    sysconfdir <- getSysconfDir
    let configBasePath = sysconfdir </> "mud" </> projectName
    contents <- readFile $ configBasePath <.> "conf"
    return
      . foldl' buildConfig (Right $ defaultConfig configBasePath)
      . map ((\(a,b) -> (a, drop 1 b)) . break (=='='))
      . filter (/="")
      $ lines contents
  where
    buildConfig :: Either String Config -> (String, String)
                -> Either String Config
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
      _ -> Left $ "Invalid variable '" ++ name ++ "'"

computeConfig :: Options -> Config -> Config
computeConfig opts = changeUser . changeGroup
  where
    changeUser  cfg = maybe cfg (\u -> cfg { cfgUser  = Just u })
                            (optUser  opts)
    changeGroup cfg = maybe cfg (\g -> cfg { cfgGroup = Just g })
                            (optGroup opts)

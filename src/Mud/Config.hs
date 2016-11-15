module Mud.Config where

import Prelude hiding (readFile)

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Free

import Data.List

import System.FilePath

import Mud.Error
import Mud.FileSystem
import Mud.Options

data Config = Config
  { cfgDeployScript   :: FilePath
  , cfgUndeployScript :: FilePath
  , cfgBasePath       :: FilePath
  , cfgUser           :: Maybe String
  , cfgGroup          :: Maybe String
  , cfgVars           :: [(String, String)]
  } deriving (Show, Eq)

defaultConfig :: FilePath -> Config
defaultConfig path = Config
  { cfgDeployScript   = path <.> "deploy"
  , cfgUndeployScript = path <.> "undeploy"
  , cfgBasePath       = "/tmp"
  , cfgUser           = Nothing
  , cfgGroup          = Nothing
  , cfgVars           = []
  }

data ConfigF a
  = ParseConfigFiles String ([Config] -> a)
  deriving Functor

type ConfigT = FreeT ConfigF

runConfigT :: MonadError MudError m => ConfigT m a -> FileSystemT m a
runConfigT = iterTM interpreter
  where
    interpreter :: MonadError MudError m
                => ConfigF (FileSystemT m a) -> FileSystemT m a
    interpreter (ParseConfigFiles projectName f) =
      actualParseConfigFiles projectName >>= f

class Monad m => MonadConfig m where
  parseConfigFiles :: String -> m [Config]

instance MonadError MudError m => MonadConfig (ConfigT m) where
  parseConfigFiles projectName = liftF $ ParseConfigFiles projectName id

instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadConfig m, Monad (t m))
  => MonadConfig (t m) where
  parseConfigFiles = lift . parseConfigFiles

actualParseConfigFiles :: (MonadFileSystem m, MonadError MudError m) => String
                       -> m [Config]
actualParseConfigFiles projectName = do
    sysconfdir <- getSysconfDir
    let mudBasePath         = sysconfdir </> "mud"
        configBasePathDirty = mudBasePath </> projectName
    configBasePath <- canonicalizePath configBasePathDirty

    unless ((mudBasePath ++ "/") `isPrefixOf` configBasePath) $
      throwError MudErrorNotInMudDirectory

    let configFilePath = configBasePath <.> "conf"
    checkFileExistence <- doesFileExist configFilePath
    if checkFileExistence
      then do
        contents <- readFile configFilePath
        cfg      <- parseConfigFile configBasePath contents
        return [cfg]

      else do
        checkDirExistence <- doesDirectoryExist configBasePath
        if checkDirExistence
          then do
            paths <- getDirectoryContents configBasePath
            forM (sort $ filter (".conf" `isSuffixOf`) paths) $ \path -> do
              contents <- readFile $ configBasePath </> path
              parseConfigFile configBasePath contents
          else
            throwError $ MudErrorNoConfigFound configBasePath

  where
    parseConfigFile :: (MonadFileSystem m, MonadError MudError m) => FilePath
                    -> String -> m Config
    parseConfigFile configBasePath =
      either throwError return
        . foldl' buildConfig (Right $ defaultConfig configBasePath)
        . map ((\(a,b) -> (a, drop 1 b)) . break (=='='))
        . filter (/="")
        . lines

    buildConfig :: Either MudError Config -> (String, String)
                -> Either MudError Config
    buildConfig eConfig (name, value) = do
      config <- eConfig
      case name of
        "deploy"   -> Right config { cfgDeployScript   = value }
        "undeploy" -> Right config { cfgUndeployScript = value }
        "basepath" -> Right config { cfgBasePath       = value }
        "user"     -> Right config { cfgUser           = Just value }
        "group"    -> Right config { cfgGroup          = Just value }
        'v' : 'a' : 'r' : ':' : n ->
          let vars = filter ((/= n) . fst) $ cfgVars config
          in Right config { cfgVars = vars ++ [(n, value)] }
        _ -> Left $ MudErrorUnreadableConfig $
               "invalid variable '" ++ name ++ "'"

computeConfig :: Options -> Config -> Config
computeConfig opts = changeUser . changeGroup
  where
    changeUser  cfg = maybe cfg (\u -> cfg { cfgUser  = Just u })
                            (optUser  opts)
    changeGroup cfg = maybe cfg (\g -> cfg { cfgGroup = Just g })
                            (optGroup opts)

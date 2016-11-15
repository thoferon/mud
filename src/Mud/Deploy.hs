module Mud.Deploy
  ( deployCommand
  , deploy
  ) where

import Data.Maybe

import Mud.Config
import Mud.Common
import Mud.History
import Mud.Options

deployCommand :: String -> String -> [(String, String)] -> Mud ()
deployCommand projectName version customVars = do
  configs <- parseConfigFiles projectName
  deploy projectName version customVars configs
  addToHistoryFiles configs $ \t ->
    HistDeploy projectName t version customVars

deploy :: String -> String -> [(String, String)] -> [Config] -> Mud ()
deploy projectName version customVars =
  mapM_ (runDeployScript projectName version customVars)

runDeployScript :: String -> String -> [(String, String)] -> Config -> Mud ()
runDeployScript projectName version customVars rawCfg = do
  opts <- getOptions
  let Config{..} = computeConfig opts rawCfg
      vars = uniqAssocList $ cfgVars ++ customVars
      args = [ projectName
             , version
             , fromMaybe cfgBasePath (optBasePath opts)
             ]
  runScript cfgUser cfgGroup cfgDeployScript args vars

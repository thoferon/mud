module Mud.Undeploy
  ( undeployCommand
  , undeploy
  ) where

import Data.Maybe

import Mud.Common
import Mud.History
import Mud.Options

undeployCommand :: String -> String -> [(String, String)] -> Mud ()
undeployCommand projectName version customVars = do
  configs <- parseConfigFiles projectName
  withNewHistoryEntries configs
    (\t done -> HistUndeploy projectName t done version)
    (undeploy projectName version customVars configs)

undeploy :: String -> String -> [(String, String)] -> [Config] -> Mud ()
undeploy projectName version customVars = mapM_ $ \config -> do
  deployVars <- getDeployVariables projectName version config
  runUndeployScript projectName version deployVars customVars config

getDeployVariables :: String -> String -> Config -> Mud [(String, String)]
getDeployVariables projectName version Config{..} = do
  Options{..} <- getOptions
  let path = fromMaybe cfgBasePath optBasePath
  History{..} <- readHistory path
  let step acc = \case
        HistDeploy n _ _ v vars
          | projectName == n && version == v -> vars
        _ -> acc
  return $ foldl step [] histEntries

runUndeployScript :: String -> String -> [(String, String)]
                  -> [(String, String)] -> Config -> Mud ()
runUndeployScript projectName version deployVars customVars rawCfg = do
  opts <- getOptions
  let Config{..} = computeConfig opts rawCfg
      vars = uniqAssocList $ cfgVars ++ deployVars ++ customVars
      args = [ projectName
             , version
             , fromMaybe cfgBasePath (optBasePath opts)
             ]
  runScript cfgUser cfgGroup cfgUndeployScript args vars

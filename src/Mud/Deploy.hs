module Mud.Deploy
  ( deploy
  ) where

import Data.Maybe

import Mud.Config
import Mud.Common
import Mud.Options

deploy :: String -> Maybe String -> [(String, String)] -> Mud ()
deploy projectName mVersion customVars = do
  rawCfgs <- parseConfigFiles projectName
  mapM_ (runDeployScript projectName mVersion customVars) rawCfgs

runDeployScript :: String -> Maybe String -> [(String, String)] -> Config
                -> Mud ()
runDeployScript projectName mVersion customVars rawCfg = do
  opts <- getOptions
  let Config{..} = computeConfig opts rawCfg
      vars = uniqAssocList $ cfgVars ++ customVars
      args = [ projectName
             , fromMaybe cfgVersion mVersion
             , fromMaybe cfgBasePath (optBasePath opts)
             ]
  runScript cfgUser cfgGroup cfgDeployScript args vars

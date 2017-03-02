module Mud.Rollback
  ( rollbackCommand
  ) where

import Control.Monad.Except

import Data.Maybe

import Mud.Common
import Mud.Deploy
import Mud.Error
import Mud.History
import Mud.Options
import Mud.Undeploy

rollbackCommand :: String -> Mud ()
rollbackCommand projectName = do
  configs <- parseConfigFiles projectName
  withNewHistoryEntries configs (HistRollback projectName) $
    rollback projectName configs

rollback :: String -> [Config] -> Mud ()
rollback _ [] = throwError MudErrorNoRollbackPlanFound
rollback projectName configs@(Config{..} : _) = do
  Options{..} <- getOptions
  let path = fromMaybe cfgBasePath optBasePath
  History{..} <- readHistory path
  let relevantEntries = catMaybes $ flip map histEntries $ \case
        HistDeploy n _ _ v vars | n == projectName -> Just (v, vars)
        _ -> Nothing
  case reverse relevantEntries of
    (v1, vars1) : (v2, vars2) : _ -> do
      undeploy projectName v1 vars1 configs
      deploy   projectName v2 vars2 configs
    _ -> throwError MudErrorNoRollbackPlanFound

module Mud.TrimHistory
  ( trimHistoryCommand
  ) where

import Control.Monad

import Mud.Common
import Mud.History

trimHistoryCommand :: String -> Bool -> Int -> Mud ()
trimHistoryCommand projectName permanent n = do
  configs <- parseConfigFiles projectName
  paths   <- workingBasePaths configs
  forM_ paths $ \path -> do
    history <- readHistory path
    let history' = history
          { histLimit = if permanent then Just n else histLimit history
          , histEntries = takeLast n $ histEntries history
          }
    writeHistory path history'

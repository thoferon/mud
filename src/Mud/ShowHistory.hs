module Mud.ShowHistory
  ( showHistoryCommand
  ) where

import Control.Monad

import Data.Time

import Mud.Common
import Mud.History
import Mud.Printer

showHistoryCommand :: String -> Mud ()
showHistoryCommand projectName = do
  configs <- parseConfigFiles projectName
  paths   <- workingBasePaths configs
  forM_ paths $ \path -> do
    History{..} <- readHistory path
    printHistory path $
      filter ((==projectName) . historyEntryProject) histEntries
    putLine ""

printHistory :: FilePath -> [HistoryEntry] -> Mud ()
printHistory path entries = do
  putLine $ "History in " ++ path ++ ":"
  let putLine' = putLine . ('\t' :)
  forM_ entries $ \case
    HistDeploy _ t done version _ ->
      putLine' $ showTime t ++ "deployed   " ++ version
                 ++ if done then " (done)" else ""
    HistUndeploy _ t done version ->
      putLine' $ showTime t ++ "undeployed " ++ version
                 ++ if done then " (done)" else ""
    HistRollback _ t done ->
      putLine' $ showTime t ++ "rollback"
                 ++ if done then " (done)" else ""

showTime :: UTCTime -> String
showTime = (\s -> "[" ++ s ++ "] ") . formatTime defaultTimeLocale "%F %T"

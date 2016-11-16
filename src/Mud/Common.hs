module Mud.Common
  ( module Mud.Common
  , module Mud.Config
  , module Mud.FileSystem
  , module Mud.RunProcess
  ) where

import Control.Monad.Except
import Control.Monad.Trans.Free
import Control.Monad.Reader

import Data.List
import Data.Time

import Mud.Config
import Mud.Error
import Mud.FileSystem
import Mud.History
import Mud.Options
import Mud.Printer
import Mud.RunProcess

type Mud = ReaderT (Options, UTCTime)
                   (ConfigT (HistoryT (ExceptT MudError (PrinterT RunProcess))))

runMud :: Options -> Mud a -> IO (Either MudError a)
runMud options action = do
  time <- getCurrentTime
  let dryRun = optDryRun options
      runPRP :: PrinterT RunProcess a -> IO a
      runPRP = runPrinterT . hoistFreeT (runRunProcess dryRun)
  runExceptT $ runFileSystemT dryRun $
    hoistFreeT (mapExceptT runPRP) $ collapseFileSystemT $
      hoistFreeT runHistoryT $ runConfigT options $
        runReaderT action (options, time)

getOptions :: Mud Options
getOptions = fst <$> ask

getTime :: Mud UTCTime
getTime = snd <$> ask

-- | Keep the last occurence for every key of an association list
uniqAssocList :: Eq a => [(a, b)] -> [(a, b)]
uniqAssocList = snd . foldr step ([], [])
  where
    step :: Eq a => (a, b) -> ([a], [(a, b)]) -> ([a], [(a, b)])
    step pair@(key, _) acc@(keys, pairs) =
      if key `elem` keys then acc else (key : keys, pair : pairs)

addToHistoryFiles :: [Config] -> (UTCTime -> HistoryEntry) -> Mud ()
addToHistoryFiles configs f = do
  t     <- getTime
  paths <- workingBasePaths configs
  mapM_ (\path -> addToHistory path (f t)) paths

workingBasePaths :: [Config] -> Mud [FilePath]
workingBasePaths configs = do
  Options{..} <- getOptions
  case optBasePath of
    Just path -> return [path]
    Nothing   -> return $ nub $ map cfgBasePath configs

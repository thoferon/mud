module Mud.Common
  ( module Mud.Common
  , module Mud.Config
  , module Mud.FileSystem
  , module Mud.RunProcess
  ) where

import Control.Monad.Except
import Control.Monad.Trans.Free
import Control.Monad.Reader

import Mud.Config
import Mud.Error
import Mud.FileSystem
import Mud.Options
import Mud.RunProcess

type Mud = ReaderT Options (ConfigT (ExceptT MudError RunProcess))

runMud :: Options -> Mud a -> IO (Either MudError a)
runMud options action = do
  let dryRun = optDryRun options
  runExceptT $ runFileSystemT dryRun $
    hoistFreeT (mapExceptT (runRunProcess dryRun)) $
      runConfigT $ runReaderT action options

getOptions :: Mud Options
getOptions = ask

-- | Keep the last occurence for every key of an association list
uniqAssocList :: Eq a => [(a, b)] -> [(a, b)]
uniqAssocList = snd . foldr step ([], [])
  where
    step :: Eq a => (a, b) -> ([a], [(a, b)]) -> ([a], [(a, b)])
    step pair@(key, _) acc@(keys, pairs) =
      if key `elem` keys then acc else (key : keys, pair : pairs)

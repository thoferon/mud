module Mud.Error
  ( MudError(..)
  , humanReadableMudError
  ) where

import Control.Monad.Error

import GHC.Conc.Signal

data MudError
  = MudErrorNoConfigFound FilePath
  | MudErrorNotInMudDirectory
  | MudErrorUnreadableConfig String
  | MudErrorScriptFailure (Either Int Signal)
  | MudErrorString String
  deriving (Show, Eq)

instance Error MudError where
  strMsg = MudErrorString

humanReadableMudError :: MudError -> String
humanReadableMudError mudError = case mudError of
  MudErrorNoConfigFound path -> "no configuration file found for base: " ++ path
  MudErrorNotInMudDirectory -> "the configuration must be in the mud directory"
  MudErrorUnreadableConfig str -> "can't read the configuration file: " ++ str
  MudErrorScriptFailure (Left code) ->
    "script failed with exit code " ++ show code
  MudErrorScriptFailure (Right sig) ->
    "script interrupted with signal " ++ show sig
  MudErrorString str -> str

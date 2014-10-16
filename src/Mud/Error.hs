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

instance Error MudError where
  strMsg = MudErrorString

humanReadableMudError :: MudError -> String
humanReadableMudError mudError = case mudError of
  MudErrorNoConfigFound path -> "No configuration file found for base: " ++ path
  MudErrorNotInMudDirectory -> "The configuration must be in the mud directory"
  MudErrorUnreadableConfig str -> "Can't read the configuration file: " ++ str
  MudErrorScriptFailure (Left code) ->
    "Deployment script failed with exit code " ++ show code
  MudErrorScriptFailure (Right sig) ->
    "Deployment script interrupted with signal " ++ show sig
  MudErrorString str -> str

module Mud.Error
  ( MudError(..)
  , humanReadableMudError
  ) where

import GHC.Conc.Signal

data MudError
  = MudErrorNoConfigFound FilePath
  | MudErrorNotInMudDirectory
  | MudErrorUnreadableConfig String
  | MudErrorUnreadableHistory String
  | MudErrorNoRollbackPlanFound
  | MudErrorScriptFailure (Either Int Signal)
  | MudErrorString String
  deriving (Show, Eq)

humanReadableMudError :: MudError -> String
humanReadableMudError mudError = case mudError of
  MudErrorNoConfigFound path -> "no configuration file found for base: " ++ path
  MudErrorNotInMudDirectory -> "configuration must be in the mud directory"
  MudErrorUnreadableConfig str -> "can't read configuration file: " ++ str
  MudErrorUnreadableHistory str -> "can't read history file: " ++ str
  MudErrorNoRollbackPlanFound -> "can't find a rollback plan"
  MudErrorScriptFailure (Left code) ->
    "script failed with exit code " ++ show code
  MudErrorScriptFailure (Right sig) ->
    "script interrupted with signal " ++ show sig
  MudErrorString str -> str

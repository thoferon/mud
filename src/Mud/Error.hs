module Mud.Error
  ( MudError(..)
  , humanReadableMudError
  ) where

data MudError
  = MudErrorUnreadableConfig String
  | MudErrorScriptFailure Int
  | MudErrorString String

humanReadableMudError :: MudError -> String
humanReadableMudError mudError = case mudError of
  MudErrorUnreadableConfig str -> "Can't read the configuration file: " ++ str
  MudErrorScriptFailure code -> "Deployment script failed with code "
                                ++ show code
  MudErrorString str -> str

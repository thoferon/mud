module Mud.Options
  ( Options(..)
  , getOptions
  ) where

import System.Environment

import Options.Applicative
import Options.Applicative.Types
import Options.Applicative.Internal -- FIXME

data Options = Options
  { optDryRun :: Bool
  , optUser   :: Maybe String
  , optGroup  :: Maybe String
  , optDeploy :: Bool
  } deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions = Options
    <$> flag False True (long "dry-run"
                         <> help "Don't actually run anything on the system")
    <*> maybeOption (short 'u' <> long "user"  <> help "User to deploy as")
    <*> maybeOption (short 'g' <> long "group" <> help "Group to deploy as")
    <*> flag True False (long "undeploy"
                         <> help "Undeploy the specified version of the app")
  where
    maybeOption :: Mod OptionFields String -> Parser (Maybe String)
    maybeOption mods =
      (\s -> if null s then Nothing else Just s) <$> strOption (mods <> value "")

getOptions :: IO (Options, [String])
getOptions = do
  args <- getArgs
  let pp = ParserPrefs
        { prefMultiSuffix     = ""
        , prefDisambiguate    = False
        , prefShowHelpOnError = True
        , prefBacktrack       = True
        , prefColumns         = 80
        }
  let (eRes, _) = runP (runParser SkipOpts parseOptions args) pp
  case eRes of
    Left  err -> error $ show err
    Right res -> return res

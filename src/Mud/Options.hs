module Mud.Options
  ( Options(..)
  , Command(..)
  , getCommandOptions
  ) where

import Options.Applicative

data Options = Options
  { optDryRun    :: Bool
  , optUser      :: Maybe String
  , optGroup     :: Maybe String
  , optBasePath  :: Maybe FilePath
  , optConfigDir :: Maybe FilePath
  }

instance Monoid Options where
  mempty = Options False Nothing Nothing Nothing Nothing
  mappend a b =
    let choose f = maybe (f a) Just (f b) -- choose last one
    in Options
      { optDryRun    = optDryRun a || optDryRun b
      , optUser      = choose optUser
      , optGroup     = choose optGroup
      , optBasePath  = choose optBasePath
      , optConfigDir = choose optConfigDir
      }

data Command
  = Deploy String String [(String, String)]
  | Undeploy String String [(String, String)]
  | Rollback String
  | ShowHistory String
  | TrimHistory String Bool Int

optParser :: Parser Options
optParser = Options
    <$> switch (long "dry-run"
                <> help "Don't actually run anything on the system")
    <*> maybeOption (short 'u' <> long "user" <> metavar "USER"
                     <> help "User to deploy as")
    <*> maybeOption (short 'g' <> long "group" <> metavar "GROUP"
                     <> help "Group to deploy as")
    <*> maybeOption (short 'd' <> long "base-path" <> metavar "DIR"
                     <> help "Base directory")
    <*> maybeOption (short 'c' <> long "config-directory" <> metavar "DIR"
                     <> help "Directory containing configuration files")

  where
    maybeOption :: Mod OptionFields String -> Parser (Maybe String)
    maybeOption mods =
      (\s -> if null s then Nothing else Just s)
      <$> strOption (mods <> value "")

withOptions :: Parser (a -> (a, Options))
withOptions = (\o x -> (x, o)) <$> optParser

projectArgument :: Parser String
projectArgument = strArgument $ metavar "PROJECT"

versionArgument :: Parser String
versionArgument = strArgument $ metavar "VERSION"

variableOptions :: Parser [(String, String)]
variableOptions =
  let reader = do
        s <- str
        case break (=='=') s of
          (name, '=' : val) -> return (name, val)
          _ -> fail $ "invalid variable " ++ show s
      mods = short 'v' <> long "var" <> help "Custom variables"
             <> metavar "NAME=VALUE"
  in many $ option reader mods

cmdParser :: Parser (Command, Options)
cmdParser = subparser
  (command "deploy"
    (info (helper <*> withOptions <*>
           (Deploy <$> projectArgument <*> versionArgument <*> variableOptions))
     (progDesc "Deploy the given version in the base directory"))
   <> command "undeploy"
        (info (helper <*> withOptions <*>
               (Undeploy <$> projectArgument <*> versionArgument
                         <*> variableOptions))
         (progDesc "Undeploy the given version from the base directory"))
   <> command "rollback"
        (info (helper <*> withOptions <*> (Rollback <$> projectArgument))
         (progDesc "Undeploy the last version from the base directory and\
                   \ redeploy the previous version"))
   <> command "show-history"
        (info (helper <*> withOptions <*> (ShowHistory <$> projectArgument))
         (progDesc "Show the history of commands ran on this project"))
   <> command "trim-history"
        (info (helper <*> withOptions
               <*> (TrimHistory
                    <$> projectArgument
                    <*> switch (long "permanent"
                                <> help "Automatically trim on new entries")
                    <*> argument auto (metavar "N")))
         (progDesc "Remove old history entries")))

cmdOptParser :: Parser (Command, Options)
cmdOptParser = (\o (c, o') -> (c, o <> o')) <$> optParser <*> cmdParser

getCommandOptions :: IO (Command, Options)
getCommandOptions = execParser $ info (helper <*> cmdOptParser) $
  fullDesc
  <> progDesc "Application deployer with support for multiple versions"
  <> footer "This program is licensed under the BSD-3 license."

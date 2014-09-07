module Mud.Deploy
  ( deploy
  ) where

import System.Environment
import System.Exit
import System.IO
import System.Posix.User
import System.Process hiding (env)

import Mud.Config
import Mud.Error
import Mud.Options

deploy :: Options -> String -> Maybe String -> Maybe FilePath -> [String]
       -> IO (Maybe MudError)
deploy opts projectName mVersion mBasePath customArgs = do
  eConfig <- parseConfigFile projectName

  case eConfig of
    Left err -> return $ Just $ MudErrorUnreadableConfig err
    Right rawCfg -> do
      let cfg = computeConfig opts rawCfg

      case cfgGroup cfg of
        Just name -> do
          groupEntry <- getGroupEntryForName name
          setGroupID $ groupID groupEntry
        Nothing -> return ()

      case cfgUser cfg of
        Just name -> do
          userEntry <- getUserEntryForName name
          setUserID $ userID userEntry
        Nothing -> return ()

      let args =
            [ projectName
            , maybe (cfgVersion  cfg) id mVersion
            , maybe (cfgBasePath cfg) id mBasePath
            ] ++ customArgs

      env <- getEnvironment
      let varNames = map fst $ cfgVars cfg
          env'     = cfgVars cfg ++ filter ((`notElem` varNames) . fst) env

      if optDryRun opts
        then do
          hPutStrLn stderr $ "Would execute " ++ cfgScriptPath cfg ++ " with "
                                              ++ show args
          return Nothing
        else do
          ph <- runProcess (cfgScriptPath cfg) args Nothing (Just env')
                           Nothing Nothing Nothing
          exitCode <- waitForProcess ph

          return $ case exitCode of
            ExitSuccess      -> Nothing
            ExitFailure code -> Just $ MudErrorScriptFailure code
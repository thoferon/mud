module Mud.Deploy
  ( runScripts
  ) where

import Control.Monad
import Control.Monad.Error

import System.Environment
import System.Exit
import System.IO
import System.Posix.User
import System.Posix.Process
import System.Process hiding (env)

import Mud.Config
import Mud.Error
import Mud.Options

runScripts :: Options -> String -> Maybe String -> Maybe FilePath -> [String]
           -> ErrorT MudError IO ()
runScripts opts projectName mVersion mBasePath customArgs = do
  rawCfgs <- parseConfigFiles projectName
  forM_ rawCfgs $ \rawCfg -> do
    pid <- liftIO $ forkProcess $
      runScript opts projectName mVersion mBasePath customArgs rawCfg
    mProcessStatus <- liftIO $ getProcessStatus True True pid
    case mProcessStatus of
      Nothing -> -- NOT normal!
        throwError $ MudErrorString "getProcessStatus returned Nothing"
      Just (Exited ExitSuccess)        -> return ()
      Just (Exited (ExitFailure code)) ->
        throwError $ MudErrorScriptFailure $ Left code
      Just (Terminated sig _) ->
        throwError $ MudErrorScriptFailure $ Right sig
      Just (Stopped sig)      ->
        throwError $ MudErrorScriptFailure $ Right sig

runScript :: Options -> String -> Maybe String -> Maybe FilePath -> [String]
          -> Config -> IO ()
runScript opts projectName mVersion mBasePath customArgs rawCfg = do
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

  let script | optDeploy opts = cfgDeployScript   cfg
             | otherwise      = cfgUndeployScript cfg
      args =
        [ projectName
        , maybe (cfgVersion  cfg) id mVersion
        , maybe (cfgBasePath cfg) id mBasePath
        ] ++ customArgs

  env <- getEnvironment
  let varNames = map fst $ cfgVars cfg
      env'     = cfgVars cfg ++ filter ((`notElem` varNames) . fst) env

  if optDryRun opts
    then do
      hPutStrLn stderr $ "Would execute " ++ script ++ " with " ++ show args
      exitSuccess
    else do
      ph <- runProcess script args Nothing (Just env')
                       Nothing Nothing Nothing
      exitCode <- waitForProcess ph
      exitWith exitCode

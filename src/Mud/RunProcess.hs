module Mud.RunProcess where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Free

import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix.Process
import           System.Posix.User
import           System.Process hiding (env, runProcess)
import qualified System.Process as P

import           Mud.Error

data RunProcessF a
  = RunProcess (Maybe String) (Maybe String) FilePath [String]
               [(String, String)] (ProcessStatus -> a)
  deriving Functor

type RunProcess = Free RunProcessF

runRunProcess :: Bool -> RunProcess a -> IO a
runRunProcess dryRun =
    foldFree (if dryRun then fakeInterpreter else interpreter)
  where
    interpreter :: RunProcessF a -> IO a
    interpreter (RunProcess mUser mGroup path args vars f) =
      fmap f . liftIO $ actualRunProcess mUser mGroup path args vars

    fakeInterpreter :: RunProcessF a -> IO a
    fakeInterpreter (RunProcess mUser mGroup path args vars f) = do
      let asUserGroup = case (mUser, mGroup) of
            (Nothing, Nothing) -> ""
            (Just u,  Nothing) -> " with user " ++ u
            (Nothing, Just g)  -> " with group " ++ g
            (Just u,  Just g)  -> " with user " ++ u ++ " and group " ++ g

      let showVar (k, v) = "      - " ++ k ++ " = " ++ show v ++ "\n"
          varsStr
            | null vars = "none"
            | otherwise = foldl (\acc var -> acc ++ showVar var) "\n" vars

      hPutStrLn stderr $
        "* Would run " ++ path ++ asUserGroup
        ++ "\n    Arguments: " ++ show args
        ++ "\n    Variables: " ++ varsStr

      return $ f $ Exited ExitSuccess

class Monad m => MonadRunProcess m where
  runProcess :: Maybe String -> Maybe String -> FilePath -> [String]
             -> [(String, String)] -> m ProcessStatus

instance MonadRunProcess (Free RunProcessF) where
  runProcess mUser mGroup path args vars =
    liftF $ RunProcess mUser mGroup path args vars id

instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadRunProcess m, Monad (t m))
  => MonadRunProcess (t m) where
  runProcess mUser mGroups path args vars =
    lift $ runProcess mUser mGroups path args vars

actualRunProcess :: Maybe String -> Maybe String -> FilePath -> [String]
                 -> [(String, String)] -> IO ProcessStatus
actualRunProcess mUser mGroup path args vars = do
  pid <- forkProcess $ do
    case mGroup of
      Just name -> do
        groupEntry <- getGroupEntryForName name
        setGroupID $ groupID groupEntry
      Nothing -> return ()

    case mUser of
      Just name -> do
        userEntry <- getUserEntryForName name
        setUserID $ userID userEntry
      Nothing -> return ()

    env <- getEnvironment
    let varNames = map fst vars
        env'     = vars ++ filter ((`notElem` varNames) . fst) env

    ph <- P.runProcess path args Nothing (Just env')
                     Nothing Nothing Nothing
    exitCode <- waitForProcess ph
    exitWith exitCode

  mStatus <- getProcessStatus True True pid
  case mStatus of
    Nothing ->
      throw $ AssertionFailed "getProcessStatus True always returns Nothing"
    Just status -> return status

runScript :: (MonadError MudError m, MonadRunProcess m) => Maybe String
          -> Maybe String -> FilePath -> [String] -> [(String, String)] -> m ()
runScript mUser mGroup path args vars = do
  status <- runProcess mUser mGroup path args vars
  case status of
    Exited ExitSuccess -> return ()
    Exited (ExitFailure code) ->
      throwError $ MudErrorScriptFailure $ Left code
    Terminated sig _ ->
      throwError $ MudErrorScriptFailure $ Right sig
    Stopped sig ->
      throwError $ MudErrorScriptFailure $ Right sig

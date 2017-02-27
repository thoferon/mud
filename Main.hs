import System.Exit
import System.IO

import Mud.Common
import Mud.Deploy
import Mud.Error
import Mud.Options
import Mud.Rollback
import Mud.ShowHistory
import Mud.TrimHistory
import Mud.Undeploy

main :: IO ()
main = do
  (cmd, options) <- getCommandOptions

  eRes <- runMud options $ case cmd of
    Deploy projectName mVersion vars ->
      deployCommand projectName mVersion vars
    Undeploy projectName mVersion vars ->
      undeployCommand projectName mVersion vars
    Rollback    projectName        -> rollbackCommand    projectName
    ShowHistory projectName        -> showHistoryCommand projectName
    TrimHistory projectName perm n -> trimHistoryCommand projectName perm n

  case eRes of
    Left err -> do
      hPutStrLn stderr $ "Deployment error: " ++ humanReadableMudError err
      exitFailure
    Right () -> return ()

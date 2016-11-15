import System.Exit
import System.IO

import Mud.Common
import Mud.Deploy
import Mud.Error
import Mud.Options
import Mud.ShowHistory

main :: IO ()
main = do
  (cmd, options) <- getCommandOptions

  eRes <- runMud options $ case cmd of
    Deploy projectName mVersion vars -> deployCommand projectName mVersion vars
    ShowHistory projectName          -> showHistoryCommand projectName

  case eRes of
    Left err -> do
      hPutStrLn stderr $ "Deployment error: " ++ humanReadableMudError err
      exitFailure
    Right () -> return ()

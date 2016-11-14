import Control.Monad.Error

import System.Exit
import System.IO

import Mud.Common
import Mud.Deploy
import Mud.Error
import Mud.Options

main :: IO ()
main = do
  (cmd, options) <- getCommandOptions

  eRes <- runMud options $ case cmd of
    Deploy projectName mVersion args -> deploy projectName mVersion args

  case eRes of
    Left err -> do
      hPutStrLn stderr $ "Deployment error: " ++ humanReadableMudError err
      exitFailure
    Right () -> hPutStrLn stderr "Successful deployment."

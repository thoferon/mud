import System.Exit
import System.IO

import Mud.Deploy
import Mud.Error
import Mud.Options

main :: IO ()
main = do
  (options, args) <- getOptions

  (projectName, mVersion, mDestination, customArgs) <- case args of
    n : "--" : args'         -> return (n, Nothing, Nothing, args')
    n : v : "--" : args'     -> return (n, Just v,  Nothing, args')
    n : v : d : "--" : args' -> return (n, Just v,  Just d,  args')
    n : []                   -> return (n, Nothing, Nothing, [])
    n : v : []               -> return (n, Just v,  Nothing, [])
    n : v : d : []           -> return (n, Just v,  Just d,  [])
    _ -> hPutStrLn stderr "Error: Invalid parameters" >> exitFailure

  mErr <- deploy options projectName mVersion mDestination customArgs
  case mErr of
    Just err -> do
      hPutStrLn stderr $ "Deployment error: " ++ humanReadableMudError err
      exitFailure
    Nothing -> hPutStrLn stderr "Successful deployment."

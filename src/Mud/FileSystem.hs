module Mud.FileSystem where

import           Prelude hiding (readFile, writeFile)

import           Control.Monad.Trans
import           Control.Monad.Trans.Free

import qualified System.Directory as D
import qualified System.IO as SIO

import qualified Paths_mud

data FileSystemF a
  = GetSysconfDir                 (String     -> a)
  | CanonicalizePath FilePath     (FilePath   -> a)
  | DoesFileExist FilePath        (Bool       -> a)
  | DoesDirectoryExist FilePath   (Bool       -> a)
  | ReadFile FilePath             (String     -> a)
  | GetDirectoryContents FilePath ([FilePath] -> a)
  | WriteFile FilePath String     (()         -> a)
  deriving Functor

type FileSystemT = FreeT FileSystemF

runFileSystemT :: MonadIO m => Bool -> FileSystemT m a -> m a
runFileSystemT dryRun = iterT interpreter
  where
    interpreter :: MonadIO m => FileSystemF (m a) -> m a
    interpreter = \case
      GetSysconfDir             f -> liftIO Paths_mud.getSysconfDir       >>= f
      CanonicalizePath     path f -> liftIO (D.canonicalizePath path)     >>= f
      DoesFileExist        path f -> liftIO (D.doesFileExist path)        >>= f
      DoesDirectoryExist   path f -> liftIO (D.doesDirectoryExist path)   >>= f
      ReadFile             path f -> liftIO (SIO.readFile path)           >>= f
      GetDirectoryContents path f -> liftIO (D.getDirectoryContents path) >>= f
      WriteFile path contents   f
        | dryRun -> do
            let n = 60
            liftIO $ SIO.hPutStrLn SIO.stderr $
              "* Would write to file " ++ path ++ ":\n\t" ++ take n contents
              ++ if length contents > n then " ...\n" else "\n"
            f ()
        | otherwise -> liftIO (SIO.writeFile path contents) >>= f

class Monad m => MonadFileSystem m where
  getSysconfDir        :: m FilePath
  canonicalizePath     :: FilePath -> m FilePath
  doesFileExist        :: FilePath -> m Bool
  doesDirectoryExist   :: FilePath -> m Bool
  readFile             :: FilePath -> m String
  getDirectoryContents :: FilePath -> m [FilePath]
  writeFile            :: FilePath -> String -> m ()

instance Monad m => MonadFileSystem (FreeT FileSystemF m) where
  getSysconfDir             = liftF $ GetSysconfDir id
  canonicalizePath     path = liftF $ CanonicalizePath path id
  doesFileExist        path = liftF $ DoesFileExist path id
  doesDirectoryExist   path = liftF $ DoesDirectoryExist path id
  readFile             path = liftF $ ReadFile path id
  getDirectoryContents path = liftF $ GetDirectoryContents path id
  writeFile path contents   = liftF $ WriteFile path contents id

instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadFileSystem m, Monad (t m))
  => MonadFileSystem (t m) where
  getSysconfDir           = lift getSysconfDir
  canonicalizePath        = lift . canonicalizePath
  doesFileExist           = lift . doesFileExist
  doesDirectoryExist      = lift . doesDirectoryExist
  readFile                = lift . readFile
  getDirectoryContents    = lift . getDirectoryContents
  writeFile path contents = lift $ writeFile path contents

module Mud.FileSystem where

import           Prelude hiding (readFile, writeFile)

import           Control.Monad.Trans
import           Control.Monad.Trans.Free

import qualified System.Directory as D
import qualified System.IO as SIO

data FileSystemF a
  = CanonicalizePath FilePath     (FilePath   -> a)
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

collapseFileSystemT :: Monad m => FileSystemT (FileSystemT m) a
                    -> FileSystemT m a
collapseFileSystemT = iterT interpreter
  where
    interpreter :: Monad m => FileSystemF (FileSystemT m a) -> FileSystemT m a
    interpreter = \case
      CanonicalizePath     path f -> canonicalizePath path     >>= f
      DoesFileExist        path f -> doesFileExist path        >>= f
      DoesDirectoryExist   path f -> doesDirectoryExist path   >>= f
      ReadFile             path f -> readFile path             >>= f
      GetDirectoryContents path f -> getDirectoryContents path >>= f
      WriteFile path contents   f -> writeFile path contents   >>= f

class Monad m => MonadFileSystem m where
  canonicalizePath     :: FilePath -> m FilePath
  doesFileExist        :: FilePath -> m Bool
  doesDirectoryExist   :: FilePath -> m Bool
  readFile             :: FilePath -> m String
  getDirectoryContents :: FilePath -> m [FilePath]
  writeFile            :: FilePath -> String -> m ()

instance Monad m => MonadFileSystem (FreeT FileSystemF m) where
  canonicalizePath     path = liftF $ CanonicalizePath path id
  doesFileExist        path = liftF $ DoesFileExist path id
  doesDirectoryExist   path = liftF $ DoesDirectoryExist path id
  readFile             path = liftF $ ReadFile path id
  getDirectoryContents path = liftF $ GetDirectoryContents path id
  writeFile path contents   = liftF $ WriteFile path contents id

instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadFileSystem m, Monad (t m))
  => MonadFileSystem (t m) where
  canonicalizePath        = lift . canonicalizePath
  doesFileExist           = lift . doesFileExist
  doesDirectoryExist      = lift . doesDirectoryExist
  readFile                = lift . readFile
  getDirectoryContents    = lift . getDirectoryContents
  writeFile path contents = lift $ writeFile path contents

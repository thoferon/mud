module SpecHelpers
  ( module SpecHelpers
  , module Test.Hspec
  ) where

import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Trans.Free hiding (iter)

import           Data.Maybe

import           System.Posix.Process
import qualified System.Directory as D

import           Test.Hspec

import           Mud.Common
import           Mud.Error
import           Mud.Options

runFakeMud :: Options -> (String -> [Config])
           -> (Maybe String -> Maybe String -> FilePath -> [String]
                            -> [(String, String)] -> ProcessStatus)
           -> Mud a -> Either MudError a
runFakeMud options fakeParseConfigFiles fakeRunProcess action = do
    iter rpInterpreter $ runExceptT $ iterT configInterpreter $
      runReaderT action options

  where
    rpInterpreter :: RunProcessF a -> a
    rpInterpreter (RunProcess mUser mGroup path args vars f) =
      f $ fakeRunProcess mUser mGroup path args vars

    configInterpreter :: Monad m => ConfigF (m a) -> m a
    configInterpreter (ParseConfigFiles projectName f) =
      f $ fakeParseConfigFiles projectName

runFakeFileSystem :: [(FilePath, String)] -> [(FilePath, [FilePath])]
                  -> (FilePath -> FilePath) -> FileSystemT Identity a -> a
runFakeFileSystem files dirs fakeCanonicalizePath =
    runIdentity . iterT interpreter
  where
    interpreter :: Monad m => FileSystemF (m a) -> m a
    interpreter = \case
      GetSysconfDir             f -> f "/etc"
      CanonicalizePath     path f -> f $ fakeCanonicalizePath path
      DoesFileExist        path f -> f $ isJust $ lookup path files
      DoesDirectoryExist   path f -> f $ isJust $ lookup path dirs
      ReadFile             path f ->
        case lookup path files of
          Nothing -> fail $ "File not found: " ++ path
          Just v  -> f v
      GetDirectoryContents path f ->
        case lookup path dirs of
          Nothing -> fail $ "Directory not found: " ++ path
          Just v  -> f v

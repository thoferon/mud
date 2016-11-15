module SpecHelpers
  ( module SpecHelpers
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  ) where

import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Free hiding (iter)

import Data.Maybe

import System.Posix.Process

import Test.Hspec
import Test.Hspec.QuickCheck

import Mud.Common
import Mud.Error
import Mud.Options

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

type FakeFS = ([(FilePath, String)], [(FilePath, [FilePath])])

runFakeFileSystem :: [(FilePath, String)] -> [(FilePath, [FilePath])]
                  -> (FilePath -> FilePath) -> FileSystemT (State FakeFS) a -> a
runFakeFileSystem fs ds fakeCanonicalizePath =
    fst . flip runState (fs, ds) . iterT interpreter
  where
    interpreter :: FileSystemF (State FakeFS a) -> State FakeFS a
    interpreter = \case
      GetSysconfDir             f -> f "/etc"
      CanonicalizePath     path f -> f $ fakeCanonicalizePath path
      DoesFileExist        path f -> get >>= f . isJust . lookup path . fst
      DoesDirectoryExist   path f -> get >>= f . isJust . lookup path . snd
      ReadFile             path f -> do
        (files, _) <- get
        case lookup path files of
          Nothing -> fail $ "File not found: " ++ path
          Just v  -> f v
      GetDirectoryContents path f -> do
        (_, dirs) <- get
        case lookup path dirs of
          Nothing -> fail $ "Directory not found: " ++ path
          Just v  -> f v
      WriteFile path contents f -> do
        (files, dirs) <- get
        let files' = (path, contents) : filter ((/= path) . fst) files
        put (files', dirs)
        f ()

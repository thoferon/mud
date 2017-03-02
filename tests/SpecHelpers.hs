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
import Data.Time

import System.Posix.Process

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Function
import Data.List

import Mud.Common
import Mud.Error
import Mud.History
import Mud.Options
import Mud.Printer

someTime :: UTCTime
someTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

runFakeMud :: Options -> (String -> [Config])
           -> (Maybe String -> Maybe String -> FilePath -> [String]
                            -> [(String, String)] -> ProcessStatus)
           -> Mud a -> Either MudError a
runFakeMud options fakeParseConfigFiles fakeRunProcess =
  fmap fst . runFakeMudHist options fakeParseConfigFiles fakeRunProcess []

runFakeMudHist :: Options -> (String -> [Config])
               -> (Maybe String -> Maybe String -> FilePath -> [String]
                                -> [(String, String)] -> ProcessStatus)
               -> [(FilePath, History)] -> Mud a
               -> Either MudError (a, [(FilePath, History)])
runFakeMudHist options fakeParseConfigFiles fakeRunProcess hs action = do
    fmap (fmap (sortBy (compare `on` fst))) $ iter rpInterpreter $
      iterT printerInterpreter $ runExceptT $ flip runStateT hs $
        iterTM historyInterpreter $ iterT configInterpreter $
          runReaderT action (options, someTime)

  where
    rpInterpreter :: RunProcessF a -> a
    rpInterpreter (RunProcess mUser mGroup path args vars f) =
      f $ fakeRunProcess mUser mGroup path args vars

    printerInterpreter :: PrinterF (m a) -> m a
    printerInterpreter (PutLine _ f) = f ()

    configInterpreter :: ConfigF (m a) -> m a
    configInterpreter (ParseConfigFiles projectName f) =
      f $ fakeParseConfigFiles projectName

    historyInterpreter :: Monad m => HistoryF (StateT [(FilePath, History)] m a)
                       -> StateT [(FilePath, History)] m a
    historyInterpreter = \case
      ReadHistory path f -> do
        histories <- get
        case lookup path histories of
          Nothing -> f defaultHistory
          Just h  -> f h
      WriteHistory path hist f -> do
        histories <- get
        let hist' = trimHistory hist
        put $ case lookup path histories of
          Nothing -> (path, hist') : histories
          Just _  -> (path, hist') : filter ((/= path) . fst) histories
        f ()

type FakeFS = ([(FilePath, String)], [(FilePath, [FilePath])])

runFakeFileSystem :: [(FilePath, String)] -> [(FilePath, [FilePath])]
                  -> (FilePath -> FilePath) -> FileSystemT (State FakeFS) a -> a
runFakeFileSystem fs ds fakeCanonicalizePath =
    fst . flip runState (fs, ds) . iterT interpreter
  where
    interpreter :: FileSystemF (State FakeFS a) -> State FakeFS a
    interpreter = \case
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

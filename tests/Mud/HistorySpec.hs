{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mud.HistorySpec where

import Control.Monad.Except

import Data.Time

import Test.QuickCheck

import Mud.Error
import Mud.History

import SpecHelpers

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> (ModifiedJulianDay <$> arbitrary)
                      <*> pure (secondsToDiffTime 3000)

instance Arbitrary HistoryEntry where
  arbitrary =
    let str  = listOf (elements "abcdefghijklm\t\r\n\\\"\'")
        vars = listOf ((,) <$> str <*> str)
    in oneof
      [ HistDeploy   <$> str <*> arbitrary <*> str <*> vars
      , HistUndeploy <$> str <*> arbitrary <*> str
      , HistRollback <$> str <*> arbitrary
      ]

instance Arbitrary History where
  arbitrary = History <$> (arbitrary `suchThatMaybe` (>0)) <*> arbitrary

spec :: Spec
spec = do
  describe "historyToString" $ do
    prop "is reversed by stringToHistory" $ \hist ->
      stringToHistory "/some/file" (historyToString hist) `shouldBe` Right hist

  let entry1 = HistDeploy "some-project" someTime "version1" []
      entry2 = HistUndeploy "some-project" someTime "version1"
      entry3 = HistDeploy "some-project" someTime "version2" [("a", "b")]
      history = History (Just 20) [entry1, entry2]
      files = [ ( "/base/path/.mud-history" , historyToString history)
              , ("/path/to/wrong/.mud-history", "corrupted")
              ]
      run = runFakeFileSystem files []
              (\_ -> error "no mock for canonicalizePath") . runExceptT

  describe "actualReadHistory" $ do
    it "reads history entries from the file .mud-history" $ do
      run (actualReadHistory "/base/path") `shouldBe` Right history

    it "returns an empty history if the file does not exist" $ do
      run (actualReadHistory "/empty/path") `shouldBe` Right defaultHistory

    it "throws MudErrorUnreadbleHistory if the file is corrupted" $ do
      let check (Left (MudErrorUnreadableHistory _)) = True
          check _ = False
      run (actualReadHistory "/path/to/wrong") `shouldSatisfy` check

  describe "actualWriteHistory" $ do
    it "writes the new history to the file" $ do
      let action = do
            actualWriteHistory "/base/path" history
            actualReadHistory "/base/path"
      run action `shouldBe` Right history

  describe "addToHistory" $ do
    it "appends a new history to the file" $ do
      let history' = history { histEntries = histEntries history ++ [entry3] }
          action   = do
            writeHistory "/base/path" history
            addToHistory "/base/path" entry3
            readHistory  "/base/path"

      runFakeMud mempty (const []) (\_ _ _ _ _ -> undefined)
                 action `shouldBe` Right history'

    it "appends a new entry and trims the history if a limit is given" $ do
      let history'  = history
                        { histLimit = Just $ length $ histEntries history }
          history'' = history' { histEntries = drop 1 (histEntries history)
                                               ++ [entry3] }
          action    = do
            writeHistory "/base/path" history'
            addToHistory "/base/path" entry3
            readHistory  "/base/path"

      runFakeMud mempty (const []) (\_ _ _ _ _ -> undefined)
                 action `shouldBe` Right history''

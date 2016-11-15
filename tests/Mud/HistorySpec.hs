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

spec :: Spec
spec = do
  describe "historyToString" $ do
    prop "is reversed by stringToHistory" $ \hist ->
      stringToHistory "/some/file" (historyToString hist) `shouldBe` Right hist

  let entry1 = HistDeploy "some-project" someTime "version1" []
      entry2 = HistUndeploy "some-project" someTime "version1"
      entry3 = HistDeploy "some-project" someTime "version2" [("a", "b")]
      files = [ ("/base/path/.mud-history", historyToString [entry1, entry2])
              , ("/path/to/wrong/.mud-history", "corrupted")
              ]
      run = runFakeFileSystem files []
              (\_ -> error "no mock for canonicalizePath") . runExceptT

  describe "actualReadHistory" $ do
    it "reads history entries from the file .mud-history" $ do
      run (actualReadHistory "/base/path") `shouldBe` Right [entry1, entry2]

    it "returns an empty history if the file does not exist" $ do
      run (actualReadHistory "/empty/path") `shouldBe` Right []

    it "throws MudErrorUnreadbleHistory if the file is corrupted" $ do
      let check (Left (MudErrorUnreadableHistory _)) = True
          check _ = False
      run (actualReadHistory "/path/to/wrong") `shouldSatisfy` check

  describe "actualAddToHistory" $ do
    it "appends a new history entry to the file" $ do
      let action = do
            actualAddToHistory "/base/path" entry3
            actualReadHistory "/base/path"
      run action `shouldBe` Right [entry1, entry2, entry3]

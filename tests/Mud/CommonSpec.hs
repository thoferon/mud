module Mud.CommonSpec where

import Mud.Common

import SpecHelpers

spec :: Spec
spec = do
  describe "uniqAssocList" $ do
    it "chooses the last occurence of a key appearing several times" $ do
      uniqAssocList [(1,2),(3,4),(1,5)]
        `shouldBe` ([(3,4),(1,5)] :: [(Int, Int)])

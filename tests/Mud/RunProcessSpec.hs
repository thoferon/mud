module Mud.RunProcessSpec where

import System.Exit
import System.Posix.Process

import Mud.Config
import Mud.RunProcess

import SpecHelpers

spec :: Spec
spec = do
  describe "runScript" $ do
    it "runs the process with the given arguments" $ do
      let action = runScript (Just "user") (Just "group") "/path/to/script"
                             ["arg1", "arg2"] [("var", "value")]

          fakeRunProcess (Just "user") (Just "group") "/path/to/script"
                         ["arg1", "arg2"] [("var", "value")] =
            Exited ExitSuccess
          fakeRunProcess _ _ _ _ _ = error "Wrong arguments"

      runFakeMud mempty (\_ -> []) fakeRunProcess action
        `shouldBe` Right ()

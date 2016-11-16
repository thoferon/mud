{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Mud.RollbackSpec where

import System.Exit
import System.Posix.Process

import Mud.Config (defaultConfig)
import Mud.History
import Mud.Rollback

import SpecHelpers

spec :: Spec
spec = do
  let parseConfigFiles = \case
        "project" -> [defaultConfig "/etc/mud/project"]
        name -> error $ "no mock found for parseConfigFiles " ++ show name

  describe "rollbackCommand" $ do
    it "runs both undeploy and deploy scripts" $ do
      let runProcess Nothing Nothing "/etc/mud/project.undeploy"
                     ["project", "version2", "/tmp"] [] = Exited ExitSuccess
          runProcess Nothing Nothing "/etc/mud/project.deploy"
                     ["project", "version1", "/tmp"] [] = Exited ExitSuccess

          entry1    = HistDeploy "project" someTime "version1" []
          entry2    = HistDeploy "project" someTime "version2" []
          history   = [entry1, entry2]
          history'  = history ++ [HistRollback "project" someTime]
          histories = [("/tmp", history)]

      runFakeMudHist mempty parseConfigFiles runProcess histories
                     (rollbackCommand "project")
        `shouldBe` Right ((), [("/tmp", history')])
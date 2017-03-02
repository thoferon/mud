{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Mud.UndeploySpec where

import System.Exit
import System.Posix.Process

import Mud.Config (Config(..), defaultConfig)
import Mud.History
import Mud.Options
import Mud.Undeploy

import SpecHelpers

spec :: Spec
spec = do
  let parseConfigFiles = \case
        "simple"  -> [defaultConfig "/etc/mud/simple"]
        "complex" -> [ (defaultConfig "/etc/mud/complex")
                         { cfgBasePath = "/one" }
                     , (defaultConfig "/etc/mud/complex")
                         { cfgBasePath = "/two" }
                     ]
        "rootwheel" -> [ (defaultConfig "/etc/mud/rootwheel")
                           { cfgUser     = Just "root"
                           , cfgGroup    = Just "wheel"
                           , cfgBasePath = "/root"
                           }
                       ]
        "overwrite" -> [ (defaultConfig "/etc/mud/overwrite")
                           { cfgVars = [("one", "1"), ("two", "2")] }
                       ]
        name -> error $ "no mock found for parseConfigFiles " ++ show name

  describe "undeployCommand" $ do
    it "runs the script specified in the config" $ do
      let runProcess Nothing Nothing "/etc/mud/simple.undeploy"
                     ["simple", "", "/tmp"] [] = Exited ExitSuccess

      runFakeMud mempty parseConfigFiles runProcess
                 (undeployCommand "simple" "" [])
        `shouldBe` Right ()

    it "runs the script with the user and group specified in the config" $ do
      let runProcess (Just "root") (Just "wheel") "/etc/mud/rootwheel.undeploy"
                     ["rootwheel", "", "/root"] [] = Exited ExitSuccess

      runFakeMud mempty parseConfigFiles runProcess
                 (undeployCommand "rootwheel" "" [])
        `shouldBe` Right ()

    it "runs all the scripts if several configs are available" $ do
      let runProcess Nothing Nothing "/etc/mud/complex.undeploy"
                     ["complex", "", "/one"] [] = Exited ExitSuccess
          runProcess Nothing Nothing "/etc/mud/complex.undeploy"
                     ["complex", "", "/two"] [] = Exited ExitSuccess

      runFakeMud mempty parseConfigFiles runProcess
                 (undeployCommand "complex" "" []) `shouldBe` Right ()

    it "overwrites the variables if available" $ do
      let runProcess Nothing Nothing "/etc/mud/overwrite.undeploy"
                     ["overwrite", "some-version", "/tmp"]
                     [("two", "2"), ("three", "3"), ("one", "42")] =
            Exited ExitSuccess
          runProcess mUser mGroup path args vars =
            error $ show (mUser, mGroup, path, args, vars)

          entry = HistDeploy "overwrite" someTime True "some-version"
                             [("one", "0"), ("three", "3")]
          histories = [("/tmp", defaultHistory { histEntries = [entry] })]

          action = runFakeMudHist mempty parseConfigFiles runProcess histories
                                  (undeployCommand "overwrite" "some-version"
                                                   [("one", "42")])

      fmap fst action `shouldBe` Right ()

    it "overwrites the user, group and base path if given in the options" $ do
      let runProcess (Just "www") (Just "daemon") "/etc/mud/rootwheel.undeploy"
                     ["rootwheel", "", "/var/www"] [] = Exited ExitSuccess
          options = mempty
            { optUser     = Just "www"
            , optGroup    = Just "daemon"
            , optBasePath = Just "/var/www"
            }

      runFakeMud options parseConfigFiles runProcess
                 (undeployCommand "rootwheel" "" [])
        `shouldBe` Right ()

    describe "on history" $ do
      let runProcess _ _ _ _ _ = Exited ExitSuccess
          entry1a = HistDeploy   "complex" someTime True "version1" [("a","b")]
          entry1b = HistDeploy   "complex" someTime True "version1" [("c","d")]
          entry2  = HistUndeploy "complex" someTime True "version1"
          histories = [ ("/one", defaultHistory { histEntries = [entry1a] })
                      , ("/two", defaultHistory { histEntries = [entry1b] })
                      ]

      it "adds a new entry to each history file" $ do
        let histories' =
              [ ("/one", defaultHistory { histEntries = [entry1a, entry2] })
              , ("/two", defaultHistory { histEntries = [entry1b, entry2] }) ]
        runFakeMudHist mempty parseConfigFiles runProcess histories
                       (undeployCommand "complex" "version1" [])
          `shouldBe` Right ((), histories')

      it "adds a new entry to the base path given in the options if any" $ do
        let histories' =
              [ ("/one", defaultHistory { histEntries = [entry1a, entry2] })
              , ("/two", defaultHistory { histEntries = [entry1b] }) ]
        runFakeMudHist (mempty { optBasePath = Just "/one" })
                       parseConfigFiles runProcess histories
                       (undeployCommand "complex" "version1" [])
          `shouldBe` Right ((), histories')

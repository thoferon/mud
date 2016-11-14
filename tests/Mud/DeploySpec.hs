{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Mud.DeploySpec where

import System.Exit
import System.Posix.Process

import Mud.Config (Config(..), defaultConfig)
import Mud.Deploy
import Mud.Options

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

  describe "deploy" $ do
    it "runs the script specified in the config" $ do
      let runProcess Nothing Nothing "/etc/mud/simple.deploy"
                     ["simple", "", "/tmp"] [] = Exited ExitSuccess

      runFakeMud mempty parseConfigFiles runProcess (deploy "simple" Nothing [])
        `shouldBe` Right ()

    it "runs the script with the user and group specified in the config" $ do
      let runProcess (Just "root") (Just "wheel") "/etc/mud/rootwheel.deploy"
                     ["rootwheel", "", "/root"] [] = Exited ExitSuccess

      runFakeMud mempty parseConfigFiles runProcess
                 (deploy "rootwheel" Nothing [])
        `shouldBe` Right ()

    it "runs all the scripts if several configs are available" $ do
      let runProcess Nothing Nothing "/etc/mud/complex.deploy"
                     ["complex", "", "/one"] [] = Exited ExitSuccess
          runProcess Nothing Nothing "/etc/mud/complex.deploy"
                     ["complex", "", "/two"] [] = Exited ExitSuccess

      runFakeMud mempty parseConfigFiles runProcess
                 (deploy "complex" Nothing []) `shouldBe` Right ()

    it "overwrites the variables and version if available" $ do
      let runProcess Nothing Nothing "/etc/mud/overwrite.deploy"
                     ["overwrite", "some-version", "/tmp"]
                     [("two", "2"), ("one", "0"), ("three", "3")] =
            Exited ExitSuccess

      runFakeMud mempty parseConfigFiles runProcess
                 (deploy "overwrite" (Just "some-version")
                                     [("one", "0"), ("three", "3")])
        `shouldBe` Right ()

    it "overwrites the user, group and base path if given in the options" $ do
      let runProcess (Just "www") (Just "daemon") "/etc/mud/rootwheel.deploy"
                     ["rootwheel", "", "/var/www"] [] = Exited ExitSuccess
          options = mempty
            { optUser     = Just "www"
            , optGroup    = Just "daemon"
            , optBasePath = Just "/var/www"
            }

      runFakeMud options parseConfigFiles runProcess
                 (deploy "rootwheel" Nothing [])
        `shouldBe` Right ()

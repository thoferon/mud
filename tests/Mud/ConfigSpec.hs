module Mud.ConfigSpec where

import Control.Monad.Except

import Mud.Config
import Mud.Error

import SpecHelpers

spec :: Spec
spec = do
  let files = [ ( "/etc/mud/simple.conf"
                , "deploy=/etc/mud/different.deploy\n\
                  \user=somebody\n\
                  \var:var1=one\n\
                  \var:var2=two\n" )
              , ("/etc/mud/complex/01_first.conf",  "basepath=/one")
              , ("/etc/mud/complex/02_second.conf", "basepath=/two")
              , ("/etc/mud/wrong.conf", "mistake=some value")
              ]
      dirs  = [("/etc/mud/complex", ["01_first.conf", "02_second.conf"])]

      canonicalize = \case
        "/etc/mud"         -> "/etc/mud"
        "/etc/mud/simple"  -> "/etc/mud/simple"
        "/etc/mud/complex" -> "/etc/mud/complex"
        "/etc/mud/missing" -> "/etc/mud/missing"
        "/etc/mud/wrong"   -> "/etc/mud/wrong"
        "/etc/mud/../blah" -> "/etc/blah"
        path -> error $ "no mock found for canonicalizePath " ++ show path

      run = runFakeFileSystem files dirs canonicalize . runExceptT

  describe "actualParseConfigFiles" $ do
    it "returns a config for /etc/mud/simple.conf" $ do
      let config = (defaultConfig "/etc/mud/simple")
            { cfgDeployScript = "/etc/mud/different.deploy"
            , cfgUser         = Just "somebody"
            , cfgVars         = [("var1", "one"), ("var2", "two")]
            }
      run (actualParseConfigFiles "/etc/mud" "simple") `shouldBe` Right [config]

    it "returns several configs for /etc/mud/complex/*.conf" $ do
      let mk path = (defaultConfig "/etc/mud/complex") { cfgBasePath = path }
          config1 = mk "/one"
          config2 = mk "/two"
      run (actualParseConfigFiles "/etc/mud" "complex")
        `shouldBe` Right [config1, config2]

    it "throws MudErrorNoConfigFound if the config does not exist" $ do
      run (actualParseConfigFiles "/etc/mud" "missing")
        `shouldBe` Left (MudErrorNoConfigFound "/etc/mud/missing")

    it "throws MudErrorUnreadableConfig if the config is wrong" $ do
      run (actualParseConfigFiles "/etc/mud" "wrong")
        `shouldBe` Left (MudErrorUnreadableConfig "invalid variable 'mistake'")

    it "throws MudErrorNotInMudDirectory if we attempt to go out of the\
       \ configuration directory" $ do
      run (actualParseConfigFiles "/etc/mud" "../blah")
        `shouldBe` Left MudErrorNotInMudDirectory

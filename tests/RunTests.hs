{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

import Cradle
import Data.Functor ((<&>))
import System.Directory (getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec

main :: IO ()
main = hspec $ do
  repoRoot <-
    runIO getCurrentDirectory
      <&> takeDirectory
  let system = "x86_64-linux"
  describe "simple-hpack" $ do
    it "allows to run a simple app" $ do
      StdoutTrimmed output <-
        run $
          cmd "nix"
            & addArgs
              [ "run",
                ".#apps." <> system <> ".foo",
                "-L",
                "--override-input",
                "garnix-haskell",
                "path://" <> repoRoot
              ]
            & setWorkingDir "./simple-hpack"
      output `shouldBe` "hello world"

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

import Cradle
import Data.Functor ((<&>))
import Data.String.Conversions (cs)
import System.Directory (setCurrentDirectory)
import System.Environment (getEnv)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec
import Test.Mockery.Directory

main :: IO ()
main = hspec $ do
  repoRoot <- runIO $ getEnv "REPO_FILES"
  let wrap test = inTempDirectory $ do
        run_ $ cmd "cp" & addArgs ["-r", repoRoot </> ".", "."]
        setCurrentDirectory "./tests"
        test
  let system = "x86_64-linux"
  around_ wrap $ do
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

      context "devShell" $ do
        it "provides a working LSP server" $ do
          StderrRaw output <-
            run $
              cmd "nix"
                & addArgs
                  [ "develop",
                    "-L",
                    "--override-input",
                    "garnix-haskell",
                    "path://" <> repoRoot,
                    "-c",
                    "haskell-language-server-wrapper",
                    "typecheck",
                    "Main.hs"
                  ]
                & setWorkingDir "./simple-hpack"
          cs output `shouldContain` "1 file worked"

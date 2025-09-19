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
        run_ $ cmd "chmod" & addArgs ["+rwX", "-R", "."]
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
        let runInDev :: (Output o) => [String] -> IO o
            runInDev command = do
              run $
                cmd "nix"
                  & addArgs
                    ( [ "develop",
                        "-L",
                        "--ignore-env",
                        "--override-input",
                        "garnix-haskell",
                        "path://" <> repoRoot,
                        "-c"
                      ]
                        <> command
                    )
                  & setWorkingDir "./simple-hpack"
        it "provides a working ghc and cabal" $ do
          StdoutTrimmed output <- runInDev ["ghc", "--version"]
          cs output `shouldContain` "Glorious Glasgow Haskell Compilation System, version"

        it "provides a working hpack & cabal" $ do
          () <- runInDev ["hpack"]
          StdoutTrimmed output <- runInDev ["cabal", "run"]
          cs output `shouldContain` "hello world"

        it "provides a working LSP server" $ do
          StderrRaw output <- runInDev ["haskell-language-server-wrapper", "typecheck", "Main.hs"]
          cs output `shouldContain` "1 file worked"

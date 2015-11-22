module Main(main) where

import Control.Monad (void)

import Test.Tasty
import Test.Tasty.Golden

import System.FilePath
import System.Process

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

mkTest :: String -> TestTree
mkTest f = goldenVsFile f gld out act
  where inp = "tests" </> f <.> "hs"
        hi  = "tests" </> f <.> "hi"
        o   = "tests" </> f <.> "o"
        gld = "tests/GoldFiles" </> f <.> "hs.golden"
        out = "tests/GoldFiles" </> f <.> "hs.current"
        act = do void $ system $ unwords ["ghc", "-fplugin=Data.SBV.Plugin", "-c", inp, ">", out, "2>&1"]
                 void $ system $ unwords ["/bin/rm", "-f", hi, o]

unitTests :: TestTree
unitTests = testGroup "Unit tests" $ map mkTest ["T01", "T02", "T03", "T04", "T05"]

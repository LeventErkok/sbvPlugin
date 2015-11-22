module Main(main) where

import Control.Monad (void)

import Data.Char (isDigit)

import Test.Tasty
import Test.Tasty.Golden

import System.Directory
import System.FilePath
import System.Process

main :: IO ()
main = do tests <- findTests
          defaultMain (testGroup "Tests" [unitTests tests])
  where unitTests = testGroup "Unit tests" . map (runTest . takeBaseName)

findTests :: IO [FilePath]
findTests = do allEntries <- getDirectoryContents "tests"
               let testFile f = let b = takeBaseName f
                                    e = takeExtension f
                                in e == ".hs" && case b of
                                                  'T':xs -> all isDigit xs
                                                  _      -> False
               return $ filter testFile allEntries

runTest :: String -> TestTree
runTest f = goldenVsFile f gld out act
  where (inp, hi, o, gld, out) = fileNames f
        act = do void $ system $ unwords ["ghc", "-package", "sbvPlugin", "-fplugin=Data.SBV.Plugin", "-c", inp, ">", out, "2>&1"]
                 void $ system $ unwords ["/bin/rm", "-f", hi, o]

fileNames :: FilePath -> (FilePath, FilePath, FilePath, FilePath, FilePath)
fileNames fp = (inp, hi, o, gld, out)
  where f   = takeBaseName fp
        inp = "tests" </> f <.> "hs"
        hi  = "tests" </> f <.> "hi"
        o   = "tests" </> f <.> "o"
        gld = "tests/GoldFiles" </> f <.> "hs.golden"
        out = "tests/GoldFiles" </> f <.> "hs.current"

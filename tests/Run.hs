module Main(main) where

import Control.Monad (void)

import Test.Tasty
import Test.Tasty.Golden

import System.Directory
import System.FilePath
import System.Process

import System.Environment

import Data.Char (isDigit)

main :: IO ()
main = do args <- getArgs
          case args of
            ("--gold" : gs) -> do tests <- case gs of
                                            [] -> findTests
                                            _  -> return gs
                                  mapM_ generateGold tests
            _               -> do tests <- findTests
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
  where (inp, hi, o) = genFiles f
        gld = "tests/GoldFiles" </> f <.> "hs.golden"
        out = "tests/GoldFiles" </> f <.> "hs.current"
        act = do void $ system $ unwords ["ghc", "-fplugin=Data.SBV.Plugin", "-c", inp, ">", out, "2>&1"]
                 void $ system $ unwords ["/bin/rm", "-f", hi, o]

generateGold :: String -> IO ()
generateGold f = act
  where (inp, hi, o) = genFiles f
        gld = "tests/GoldFiles" </> takeBaseName f <.> "hs.golden"
        act = do void $ system $ unwords ["ghc", "-fplugin=Data.SBV.Plugin", "-c", inp, ">", gld, "2>&1"]
                 void $ system $ unwords ["/bin/rm", "-f", hi, o]
                 putStrLn $ "Generated " ++ show gld

genFiles :: String -> (String, String, String)
genFiles fp = (inp, hi, o)
  where f   = takeBaseName fp
        inp = "tests" </> f <.> "hs"
        hi  = "tests" </> f <.> "hi"
        o   = "tests" </> f <.> "o"

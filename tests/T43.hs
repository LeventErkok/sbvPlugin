{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T43 where

import Data.SBV.Plugin

s :: [Int] -> Int
s []     = 0
s (x:xs) = x + s xs

{-# ANN t theorem #-}
t :: Int -> Bool
t x = 3 * x == s lst
 where lst | x == 0 = [x, x, x]
           | True   = [x, x, x]

{-# ANN r theorem {options = [IgnoreFailure]} #-}
r :: Int -> Bool
r x = 3 * x == s lst
 where lst | x == 0 = [x, x, x]
           | True   = [x, x, x, x]

{-# ANN s ("HLint: ignore Use foldr" :: String) #-}

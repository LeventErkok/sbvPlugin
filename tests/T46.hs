{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T46 where

import Data.SBV.Plugin

type Valid a = (a, Bool)

markValid :: (a -> Bool) ->[a] -> [Valid a]
markValid f xs = [(x, f x) | x <- xs]

foldValid :: (a -> b -> b) -> b -> [Valid a] -> b
foldValid _ b []                = b
foldValid f b ((_, False):rest) =     foldValid f b rest
foldValid f b ((x, True) :rest) = f x (foldValid f b rest)

len :: [Valid a] -> Int
len = foldValid (\_ n -> n+1) 0

{-# ANN filtLenGood theorem {options = [ListSize 5, IgnoreFailure]} #-}
filtLenGood :: (Int -> Bool) -> [Int] -> Bool
filtLenGood f xs = len (markValid f xs) <= len (markValid (\_ -> False) xs)

{-# ANN filtLenBad theorem {options = [ListSize 5]} #-}
filtLenBad :: (Int -> Bool) -> [Int] -> Bool
filtLenBad f xs = len (markValid f xs) <= len (markValid (\_ -> True) xs)

{-# ANN filtLenGood ("HLint: ignore Use const") #-}
{-# ANN filtLenBad  ("HLint: ignore Use const") #-}

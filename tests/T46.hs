{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T46 where

import Data.SBV.Plugin

type Valid = (Int, Bool)

markValid :: (Int -> Bool) -> [Int] -> [Valid]
markValid f xs = [(x, f x) | x <- xs]

foldValid :: (Int -> Int -> Int) -> Int -> [Valid] -> Int
foldValid _ b []                = b
foldValid f b ((_, False):rest) =     foldValid f b rest
foldValid f b ((x, True) :rest) = f x (foldValid f b rest)

len :: [Valid] -> Int
len = foldValid (\_ n -> n+1) 0

{-# ANN filtLenBad theorem {options = [ListSize 5, IgnoreFailure]} #-}
filtLenBad :: (Int -> Bool) -> [Int] -> Bool
filtLenBad f xs = len (markValid f xs) <= len (markValid (\_ -> False) xs)

{-# ANN filtLenGood theorem {options = [ListSize 5]} #-}
filtLenGood :: (Int -> Bool) -> [Int] -> Bool
filtLenGood f xs = len (markValid f xs) <= len (markValid (\_ -> True) xs)

{-# ANN filtLenBad  ("HLint: ignore Use const") #-}
{-# ANN filtLenGood ("HLint: ignore Use const") #-}

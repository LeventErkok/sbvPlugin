{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T45 where

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

{-# ANN filtLen theorem {options = [ListSize 5]} #-}
filtLen :: [Int] -> Bool
filtLen xs = len (markValid (\x -> x `quot` 2 == 0) xs) <= len (markValid (\_ -> True) xs)

{-# ANN filtLen ("HLint: ignore Use const") #-}

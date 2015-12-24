{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T42 where

import Data.SBV.Plugin

{-# ANN f theorem {options = [ListSize 5]} #-}
f :: [Int] -> Bool
f xs = s xs /= 10
  where s []     = 0
        s (x:xs) = x + s xs

{-# ANN f ("HLint: ignore Use foldr" :: String) #-}

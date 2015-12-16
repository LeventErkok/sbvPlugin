{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T31 where

import Data.SBV.Plugin

{-# ANN f theorem {options = [IgnoreFailure]} #-}
f :: Int -> Int
f x = x + 1

{-# ANN g theorem {options = [IgnoreFailure]} #-}
g :: Char
g = 'a'

{-# ANN h theorem {options = [IgnoreFailure]} #-}
h :: Double
h = 0

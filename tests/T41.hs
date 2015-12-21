{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T41 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Int -> Int -> Bool
f x y = [x, y, x] == [x, y, x]

{-# ANN g theorem {options = [IgnoreFailure]} #-}
g :: Int -> Int -> Bool
g x y = [x, y] == [y, x]

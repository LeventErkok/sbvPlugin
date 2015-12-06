{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T17 where

import Data.SBV.Plugin

{-# ANN g sbv {options = [Uninterpret]} #-}
g :: Int -> Int
g _x = 0

{-# ANN f theorem #-}
f :: Int -> Bool
f x = g x == g (x+1)

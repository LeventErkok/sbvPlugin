{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T11 where

import Data.SBV.Plugin

h :: Integer -> Integer
h x = x - 1

g :: Integer -> Integer
g x = if x < 12 then x+1 else h x

{-# ANN f theorem #-}
f :: Integer -> Bool
f x = g x < g (x+1)

{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T10 where

import Data.SBV.Plugin

g :: Integer -> Integer
g x = x * 2 + 12

{-# ANN f theorem #-}
f :: Integer -> Bool
f x = g x < g (x+1)

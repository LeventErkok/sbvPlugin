module T01 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Double -> Double -> Double -> Bool
f x y z = x + (y + z) == (x + y) + z

module T06 where

import Data.Int
import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Int8 -> Bool
f x = abs x >= 0

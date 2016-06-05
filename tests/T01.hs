{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T01 where

import Data.SBV.Plugin

nan :: Double -> Bool
nan x = x /= x

{-# ANN f theorem #-}
f :: Double -> Double -> Double -> Bool
f x y z
 | nan x || nan y || nan z = True
 | True                    = x + (y + z) == (x + y) + z

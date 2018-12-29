{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T01 where

import Data.SBV.Plugin

bad :: Double -> Bool
bad x =  (x /= x)      -- avoid NaN
      || (x == (1/0))  -- avoid +Inf
      || (x == -(1/0)) -- avoid -Inf

{-# ANN f theorem #-}
f :: Double -> Double -> Double -> Bool
f x y z
 | bad (x + (y + z)) || bad ((x + y) + z) = True
 | True                                   = x + (y + z) == (x + y) + z

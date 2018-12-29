{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T08 where

import Data.SBV.Plugin

bad :: Double -> Bool
bad x =  (x /= x)      -- avoid NaN
      || (x == (1/0))  -- avoid +Inf
      || (x == -(1/0)) -- avoid -Inf

{-# ANN f theorem #-}
f :: Double -> Bool
f x
 | bad x = True
 | True  = x == 2.321

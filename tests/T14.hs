{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T14 where

import Data.SBV.Plugin
import Data.Ratio

{-# ANN f theorem #-}
f :: Rational -> Rational -> Bool
f x y = (x == y) == (numerator x * denominator y == numerator y * denominator x)

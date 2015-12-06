{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T14 where

import Data.SBV.Plugin
import Data.Ratio

{-# ANN f theorem {options = [Names ["x"]]} #-}
f :: Rational -> Bool
f 0 = True
f x = 1 / (1 / x) /= x

{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T39 where

import Data.SBV.Plugin

{-# ANN f theorem {options = [Names ["p"]] } #-}
f :: (Int, Int) -> Bool
f (a, b) = (b, a) == (a, b)

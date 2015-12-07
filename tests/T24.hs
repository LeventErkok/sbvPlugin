{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T24 where

import Data.SBV.Plugin
import Data.Int

{-# ANN f theorem #-}
f :: Int8 -> Int8 -> Bool
f x 0 = True
f x y = x == q * y + r
  where q = x `quot` y
        r = x `rem`  y

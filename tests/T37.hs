{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T37 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Int -> Double -> Bool
f a b = case (a, b) of
          (2, c) -> c /= 2.3
          (_, 3) -> a == a
          _      -> True

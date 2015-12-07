{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T23 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Int -> Int -> Bool
f x y = lhs == rhs
  where lhs = (x-y) * (x+y)
        rhs = x*x - y*y

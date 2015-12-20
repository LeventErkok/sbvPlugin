{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T36 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Int -> Int -> Int -> Bool
f a b c = rot (rot (rot (a, b, c))) == (a, b, c)
  where rot (x, y, z) = (y, z, x)

{-# ANN g theorem {options = [IgnoreFailure]} #-}
g :: Int -> Int -> Int -> Bool
g a b c = rot (rot (a, 3, b, c)) == (a, b, c, 3::Int)
  where rot (x, y, z, w) = (y, z, w, x)

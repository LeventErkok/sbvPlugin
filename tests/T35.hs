{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T35 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Int -> Int -> Bool
f x y = (x, y) == (y, x)

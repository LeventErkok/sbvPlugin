{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T34 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Int -> Int -> Bool
f x y = (x, y) == (x, y)

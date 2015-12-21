{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T22 where

import Data.SBV.Plugin

f :: Int -> Bool
f x = x == 0

{-# ANN g theorem #-}
g :: Int -> Bool
g = f

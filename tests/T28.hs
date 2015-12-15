{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T28 where

import Data.SBV.Plugin

f :: Int -> Bool
f 0 = True
f x = f x

{-# ANN g theorem #-}
g :: Bool
g = f 0

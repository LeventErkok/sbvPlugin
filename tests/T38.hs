{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T36 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Int -> Int -> Int -> Bool
f a b c = case arrange (a, (b, c)) of
            ((1, 2), (1, 4), 4) -> False
            ((d, e), (f, g), h) -> True
 where arrange (x, (y, z)) = ((x, y), (x, z), z)

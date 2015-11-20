module Main where

import Data.Word
import Data.SBV.Plugin

{-# ANN f theorem {options = [WarnIfFails]} #-}
f :: Double -> Double -> Double -> Bool
f x y z = x + (y + z) == (x + y) + z

{-# ANN g theorem {options = [WarnIfFails]} #-}
g :: Integer -> Integer -> Bool
g x y = x == y

{-# ANN h theorem #-}
t :: Word8 -> Bool
t x = x >= x

{-# ANN h theorem {solvers = [AnySolver]} #-}
h :: Word8 -> Bool
h x = x == x

{-# ANN q theorem {options = [WarnIfFails], solvers = [CVC4, Yices]} #-}
q :: Integer -> Integer -> Bool
q x y = x + y >= x - y

main :: IO ()
main = return ()

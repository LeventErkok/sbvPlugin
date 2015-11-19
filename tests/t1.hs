module Main where

import Data.Word
import Data.SBV.Plugin

{-# ANN f (SBVTheorem [WarnIfFails]) #-}
f :: Double -> Double -> Double -> Bool
f x y z = x + (y + z) == (x + y) + z

{-# ANN g (SBVTheorem [WarnIfFails]) #-}
g :: Integer -> Integer -> Bool
g x y = x == y

{-# ANN h (SBVTheorem [AnySolver]) #-}
h :: Word8 -> Bool
h x = x == x

{-# ANN q (SBVTheorem [WarnIfFails]) #-}
q :: Integer -> Integer -> Bool
q x y = x + y >= x - y

main :: IO ()
main = return ()

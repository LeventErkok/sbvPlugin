module Main where

import Data.SBV.Plugin

{-# ANN f SBVTheorem #-}
f :: Double -> Double -> Double -> Bool
f x y z = x + (y + z) == (x + y) + z

{-# ANN g SBVTheorem #-}
g :: Integer -> Integer -> Bool
g x y = x == y

{-# ANN h SBVTheorem #-}
h :: Integer -> Bool
h x = x == x

{-# ANN q SBVTheorem #-}
q :: Integer -> Integer -> Bool
q x y = x + y >= x - y

main :: IO ()
main = return ()

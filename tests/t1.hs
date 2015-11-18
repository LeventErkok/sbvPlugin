module Main where

import Data.SBV.Plugin

{-# ANN f SBVTheorem #-}
f :: Double -> Double -> Bool
f x y = x + y == y

{-# ANN g SBVTheorem #-}
g :: Integer -> Integer -> Bool
g x y = x == y

{-# ANN h SBVTheorem #-}
h :: Integer -> Bool
h x = x == x

main :: IO ()
main = print $ f 2 3

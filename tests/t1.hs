module Main where

import Data.SBV.Plugin

f :: Double -> Double -> Bool
f x y = x + y == y

{-# ANN g SBVTheorem #-}
g :: Integer -> Integer -> Bool
g x y = x == y

main :: IO ()
main = print $ f 2 3

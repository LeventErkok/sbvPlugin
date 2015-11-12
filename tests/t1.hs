module Main where

f :: Double -> Double -> Double
f x y = x + y

g :: Integer -> Integer -> Integer
g x y = x * y

main :: IO ()
main = print $ f 2 3

module T02 where

import Data.SBV.Plugin

{-# ANN f theorem {options = [WarnIfFails]} #-}
f :: Integer -> Integer -> Bool
f x y = x == y

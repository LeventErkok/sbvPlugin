{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T33 where

import Data.SBV.Plugin

{-# ANN f theorem {options = [IgnoreFailure]} #-}
f :: Int -> Bool
f x =  foo x == x
  where -- foo :: Int -> Int. -- Type commented out on purpose to test the polymorphism here.
        foo a = a+1

{-# ANN g theorem #-}
g :: Int -> Bool
g x =  foo x == x
  where -- foo :: Int -> Int. -- Type commented out on purpose to test the polymorphism here.
        foo a = a

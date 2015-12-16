{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T30 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Int -> Bool
f x
 | x < 0  = True
 | x > 10 = True
 | True   = foo x 10 == x*(x+1) `div` 2
  where -- Note the use of counter to stop symbolic recursion!
        foo :: Int -> Int -> Int
        foo _ 0 = 0
        foo 0 _ = 0
        foo n c = n + foo (n-1) (c-1)

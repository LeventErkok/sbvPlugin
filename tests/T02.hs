{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T02 where

import Data.SBV.Plugin

{-# ANN f theorem {options = [IgnoreFailure]} #-}
f :: Integer -> Integer -> Bool
f x y = x == y

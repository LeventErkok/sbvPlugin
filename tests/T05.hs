{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T05 where

import Data.SBV.Plugin

{-# ANN f theorem {options = [WarnIfFails, CVC4, Yices]} #-}
f :: Integer -> Integer -> Bool
f x y = x + y >= x - y

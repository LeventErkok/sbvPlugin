module T04 where

import Data.Word
import Data.SBV.Plugin

{-# ANN f theorem {options = [AnySolver]} #-}
f :: Word8 -> Bool
f x = x == x

module T03 where

import Data.Word
import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Word8 -> Bool
f x = x >= x

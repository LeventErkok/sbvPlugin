{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T32 where

import Data.SBV.Plugin
import Data.Word
import Data.Bits

{-# ANN f theorem #-}
f :: Word8 -> Bool
f x = x `shiftL` 2 == x * 4

{-# ANN g theorem {options = [IgnoreFailure]} #-}
g :: Word8 -> Bool
g x = x `shiftL` 2 == x * 2

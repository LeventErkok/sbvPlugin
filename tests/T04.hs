{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T04 where

import Data.Word
import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Word8 -> Bool
f x = x == x

{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T20 where

import Data.Word
import Data.SBV.Plugin

{-# ANN f theorem {options = [Skip "Don't want to prove this now."]} #-}
f :: Word8 -> Bool
f x = x >= x

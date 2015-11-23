{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T00 where

import Data.Word
import Data.Int

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Word8 -> Bool -> Bool
f _ _ = True

{-# ANN g theorem {options = [WarnIfFails]} #-}
g :: Int8 -> Bool -> Bool
g _x _y = False

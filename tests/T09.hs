{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T09 where

import Data.Word
import Data.SBV.Plugin

{-# ANN f theorem {options = [QuickCheck]} #-}
f :: Word32 -> Bool
f x = x == x

{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T08 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Double -> Bool
f x = x == 2.321

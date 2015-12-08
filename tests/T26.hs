{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T26 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Bool
f = False

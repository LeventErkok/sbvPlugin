{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T29 where

import Data.SBV.Plugin

f :: Bool -> Bool
f True  = False
f False = f False

{-# ANN g theorem #-}
g :: Bool
g = f True

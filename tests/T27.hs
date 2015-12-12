{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T27 where

import Data.SBV.Plugin

{-# ANN g theorem {options=[Verbose]}#-}
g :: Bool
g = let x = 3
        y = 2
        z = x - y
    in z == 1

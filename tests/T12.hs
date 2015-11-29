{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T12 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Integer -> Bool -> Bool
f i b = case i of
         1 -> True
         _ -> b

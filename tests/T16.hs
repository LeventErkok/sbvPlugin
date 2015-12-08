{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T16 where

import Data.SBV.Plugin

newtype Age = Age Int deriving Eq

{-# ANN f theorem {options = [Names ["age"]]} #-}
f :: Age -> Bool
f (Age i) = i == i+1

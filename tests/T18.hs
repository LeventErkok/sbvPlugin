{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T18 where

import Data.SBV.Plugin

newtype Age = Age Int deriving Eq

{-# ANN f theorem {options = [Verbose]} #-}
f :: Age -> Age -> Bool
f a@(Age i) b@(Age j) = a == b

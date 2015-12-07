{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T21 where

import Data.SBV.Plugin

{-# ANN f theorem {options=[Verbose]} #-}
f :: Char -> String -> Bool
f c s = c == c && s == s

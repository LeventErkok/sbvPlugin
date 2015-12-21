{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T22 where

import Data.SBV.Plugin

{-# ANN f theorem {options=[Verbose, ListSize 5]} #-}
f :: String -> Bool
f s = reverse (reverse s) == s

{-# ANN f ("HLint: ignore Use String"    :: String) #-}
{-# ANN f ("HLint: ignore Avoid reverse" :: String) #-}

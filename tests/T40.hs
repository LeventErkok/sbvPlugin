{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T40 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Bool
f = [] == ([] :: [Int])

{-# ANN module ("HLint: ignore Use null" :: String) #-}

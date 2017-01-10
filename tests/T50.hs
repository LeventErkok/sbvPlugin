{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T50 where

import Data.SBV.Plugin.Data

integerAssociative :: Proved (Integer -> Integer -> Integer -> Bool)
integerAssociative x y z = ((x + y) + z) == (x + (y + z))

isTrue :: Proved Bool
isTrue = True || False

{-# ANN module ("HLint: ignore Evaluate" :: String) #-}

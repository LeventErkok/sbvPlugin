{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T19 where

import Data.SBV.Plugin

{-# ANN f theorem {options=[ListSize 5]} #-}
f :: String -> Bool
f s = rev (rev s) == s
  where rev [a, b, c, d, e] = [e, d, c, b, a]
        rev xs              = 'a':xs

{-# ANN g theorem {options=[ListSize 6, IgnoreFailure]} #-}
g :: String -> Bool
g s = f s

{-# ANN g ("HLint: ignore Eta reduce" :: String) #-}

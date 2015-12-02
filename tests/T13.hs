{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T13 where

import Data.SBV.Plugin

{-# ANN f theorem #-}
f :: Integer -> Double -> Bool -> Bool
f i d b = case i of
           1 -> True
           _ -> case d of
                  3.5 -> True
                  _   -> (d /= d) || b

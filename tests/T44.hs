{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module T43 where

import Data.SBV.Plugin

-- Prove both addition and multiplication are
-- commutative over integers; with a higher-order
-- case!
{-# ANN t theorem #-}
t :: Bool -> Integer -> Integer -> Bool
t b x y = f x y == f y x
 where f | b    = (+)
         | True = (*)

-- Simiar, except we put in subtraction in one case
-- to cause failure!
{-# ANN r theorem {options = [IgnoreFailure]} #-}
r :: Bool -> Integer -> Integer -> Bool
r b x y = f x y == f y x
 where f | b    = (+)
         | True = (-)

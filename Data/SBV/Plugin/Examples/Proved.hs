-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Examples.Proved
-- Copyright   :  (c) Nickolas Fotopoulos
-- License     :  BSD3
-- Maintainer  :  nickolas.fotopoulos@gmail.com
-- Stability   :  experimental
--
-- An example of activating sbvPlugin by wrapping types in Proved
-- instead of using an annotation.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.SBV.Plugin.Examples.Proved where

import Data.SBV.Plugin

-- | A top-level binding with its type wrapped in Proved causes sbvPlugin to
-- run a proof on the expression.
integerAssociative :: Proved (Integer -> Integer -> Integer -> Bool)
integerAssociative x y z = ((x + y) + z) == (x + (y + z))

-- | Simple booleans can be made theorems too.
isTrue :: Proved Bool
isTrue = True || False

{-# ANN module ("HLint: ignore Evaluate" :: String) #-}

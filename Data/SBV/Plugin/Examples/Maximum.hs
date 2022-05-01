-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Examples.Maximum
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Shows that a naive definition of maximum doing bit-vector arithmetic
-- is incorrect.
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#ifndef HADDOCK
{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}
#endif

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.SBV.Plugin.Examples.Maximum where

import Data.SBV.Plugin

-- | Compute the maximum of three integers, which
-- is intuitively correct for unbounded values, but
-- not for bounded bit-vectors.
myMax :: Int -> Int -> Int -> Int
myMax x y z | x-y >= 0 && x-z >= 0 = x
            | y-x >= 0 && y-z >= 0 = y
            | otherwise            = z

-- | Show that this function fails to compute maximum correctly.
-- We have:
--
-- @
-- [SBV] a.hs:11:1-7 Proving "correct", using Z3.
-- [Z3] Falsifiable. Counter-example:
--   x = -2816883406898309583 :: Int64
--   y = -2816883406898309583 :: Int64
--   z =  6694719001794338309 :: Int64
-- @
correct :: Proved (Int -> Int -> Int -> Bool)
correct x y z = m >= x && m >= y && m >= z
  where m = myMax  x y z

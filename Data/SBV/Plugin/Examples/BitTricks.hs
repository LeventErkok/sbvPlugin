-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Examples.BitTricks
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Checks the correctness of a few tricks from the large collection found in:
--      <http://graphics.stanford.edu/~seander/bithacks.html>
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}

module Data.SBV.Plugin.Examples.BitTricks where

import Data.SBV.Plugin

import Data.Bits
import Data.Word

import Prelude hiding(elem)

-- | SBVPlugin can only see definitions in the current module. So we define `elem` ourselves.
elem :: Eq a => a -> [a] -> Bool
elem _ []     = False
elem k (x:xs) = k == x || elem k xs

-- | Returns 1 if bool is @True@
oneIf :: Num a => Bool -> a
oneIf True  = 1
oneIf False = 0

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#IntegerMinOrMax>
{-# ANN fastMinCorrect theorem #-}
fastMinCorrect :: Int -> Int -> Bool
fastMinCorrect x y = m == fm
  where m  = if x < y then x else y
        fm = y `xor` ((x `xor` y) .&. (-(oneIf (x < y))));

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#IntegerMinOrMax>
{-# ANN fastMaxCorrect theorem #-}
fastMaxCorrect :: Int -> Int -> Bool
fastMaxCorrect x y = m == fm
  where m  = if x < y then y else x
        fm = x `xor` ((x `xor` y) .&. (-(oneIf (x < y))));

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#DetectOppositeSigns>
{-# ANN oppositeSignsCorrect theorem #-}
oppositeSignsCorrect :: Int -> Int -> Bool
oppositeSignsCorrect x y = r == os
  where r  = (x < 0 && y >= 0) || (x >= 0 && y < 0)
        os = (x `xor` y) < 0

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#ConditionalSetOrClearBitsWithoutBranching>
{-# ANN conditionalSetClearCorrect theorem #-}
conditionalSetClearCorrect :: Bool -> Word32 -> Word32 -> Bool
conditionalSetClearCorrect f m w = r == r'
  where r  | f    = w .|. m
           | True = w .&. complement m
        r' = w `xor` ((-(oneIf f) `xor` w) .&. m)

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2>
{-# ANN powerOfTwoCorrect theorem #-}
powerOfTwoCorrect :: Word32 -> Bool
powerOfTwoCorrect v = f == (v `elem` [2^i | i <- [(0 :: Word32) .. 31]])
  where f = (v /= 0) && ((v .&. (v-1)) == 0)

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#MaskedMerge>
{-# ANN maskedMergeCorrect theorem #-}
maskedMergeCorrect :: Word32 -> Word32 -> Word32 -> Bool
maskedMergeCorrect a b mask = slow == fast
  where slow = (a .&. complement mask) .|. (b .&. mask)
        fast = a `xor` ((a `xor` b) .&. mask)

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2>
{-# ANN roundPowerOfTwoCorrect theorem #-}
roundPowerOfTwoCorrect :: Word32 -> Bool
roundPowerOfTwoCorrect v = f == find [2^i | i <- [(0 :: Word32) .. 31]]
  where f = let v1 = v - 1
                v2 = v1 .|. (v1 `shiftR`  1)
                v3 = v2 .|. (v2 `shiftR`  2)
                v4 = v3 .|. (v3 `shiftR`  4)
                v5 = v4 .|. (v4 `shiftR`  8)
                v6 = v5 .|. (v5 `shiftR` 16)
                v7 = v6 + 1
                v8 = v7 + oneIf (v7 == 0)
            in  v8

        -- walk down the powers and return the closest one up
        find :: [Word32] -> Word32
        find []     = 1
        find (x:xs)
          | v > x = find xs
          | True   = x

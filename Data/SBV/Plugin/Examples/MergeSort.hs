-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Examples.MergeSort
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- An implementation of merge-sort and its correctness.
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#ifndef HADDOCK
{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}
#endif

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.SBV.Plugin.Examples.MergeSort where

import Data.SBV.Plugin

-----------------------------------------------------------------------------
-- * Implementing merge-sort
-- ${mergeSort}
-----------------------------------------------------------------------------
{- $mergeSort
A straightforward implementation of merge sort. We simply divide the input list
in to two halves so long as it has at least two elements, sort each half on its
own, and then merge.
-}

-- | Merging two given sorted lists, preserving the order.
merge :: [Int] -> [Int] -> [Int]
merge []     ys           = ys
merge xs     []           = xs
merge xs@(x:xr) ys@(y:yr)
  | x < y                 = x : merge xr ys
  | True                  = y : merge xs yr

-- | Simple merge-sort implementation.
mergeSort :: [Int] -> [Int]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort th) (mergeSort bh)
   where (th, bh) = halve xs ([], [])
         halve :: [Int] -> ([Int], [Int]) -> ([Int], [Int])
         halve []     sofar    = sofar
         halve (a:as) (fs, ss) = halve as (ss, a:fs)

-----------------------------------------------------------------------------
-- * Proving correctness of sorting
-- ${props}
-----------------------------------------------------------------------------
{- $props
There are two main parts to proving that a sorting algorithm is correct:

       * Prove that the output is non-decreasing
 
       * Prove that the output is a permutation of the input
-}

-- | Check whether a given sequence is non-decreasing.
nonDecreasing :: [Int] -> Bool
nonDecreasing []       = True
nonDecreasing [_]      = True
nonDecreasing (a:b:xs) = a <= b && nonDecreasing (b:xs)

-- | Check whether two given sequences are permutations. We simply check that each sequence
-- is a subset of the other, when considered as a set. The check is slightly complicated
-- for the need to account for possibly duplicated elements.
isPermutationOf :: [Int] -> [Int] -> Bool
isPermutationOf as bs = go as [(b, True) | b <- bs] && go bs [(a, True) | a <- as]
  where go :: [Int] -> [(Int, Bool)] -> Bool
        go []     _  = True
        go (x:xs) ys = found && go xs ys'
           where (found, ys') = mark x ys

        -- Go and mark off an instance of 'x' in the list, if possible. We keep track
        -- of unmarked elements by associating a boolean bit. Note that we have to
        -- keep the lists equal size for the recursive result to merge properly.
        mark :: Int -> [(Int, Bool)] -> (Bool, [(Int, Bool)])
        mark _ []            = (False, [])
        mark x ((y, v) : ys)
          | v && x == y      = (True, (y, not v) : ys)
          | True             = (r, (y, v) : ys')
          where (r, ys') = mark x ys

-----------------------------------------------------------------------------
-- * The correctness theorem
-----------------------------------------------------------------------------

-- | Asserting correctness of merge-sort for a list of the given size. Note that we can
-- only check correctness for fixed-size lists. Also, the proof will get more and more
-- complicated for the backend SMT solver as @n@ increases. Here we try it with 4.
--
-- We have:
--
-- @
--   [SBV] tests/T48.hs:100:1-16 Proving "mergeSortCorrect", using Z3.
--   [Z3] Q.E.D.
-- @
{-# ANN mergeSortCorrect theorem { options = [ListSize 4] } #-}
mergeSortCorrect :: [Int] -> Bool
mergeSortCorrect xs = nonDecreasing ys && isPermutationOf xs ys
   where ys = mergeSort xs

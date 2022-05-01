-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Examples.MicroController
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- A transcription of Anthony Cowley's MicroController example, using
-- the SBV plugin. For the original, see: <http://acowley.github.io/NYHUG/FunctionalRoboticist.pdf>
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#ifndef HADDOCK
{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}
#endif

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.SBV.Plugin.Examples.MicroController where

import Data.SBV.Plugin

-----------------------------------------------------------------------------
-- * Parameters
-----------------------------------------------------------------------------

-- | The range detector must output if the range is larger than this amount.
safetyDistance :: Int
safetyDistance = 200

-- | The range detector must have sent an output before this many cycles have past.
maxTimeSince :: Int
maxTimeSince = 10

-----------------------------------------------------------------------------
-- * The specification
-----------------------------------------------------------------------------

-- | Given a last-signal-time calculator, named @calculate@, check that it satisfies the following
-- three requirements: We must've just sent a signal if:
--
--    * /minRate/:        The last-time we sent is strictly less than the 'maxTimeSince' amount
--    * /minRange/:       We must've just sent a signal if the range is beyond 'safetyDistance'
--    * /manualOverride/: We must've just sent a signal if the manual-override is specified
checkSpec :: (Int -> Bool -> Int -> Int) -> Int -> Bool -> Int -> Bool
checkSpec calculate r m t = minRate && minRange && manualOverride

  where sinceLast      = calculate r m t

        -- Never exceed the max-time allowed
        minRate        = sinceLast < maxTimeSince

        -- If the range is exceeded, always send a signal
        minRange       = r <= safetyDistance || sinceLast == 0

        -- Manual override, always signals
        manualOverride = not m || sinceLast == 0

-----------------------------------------------------------------------------
-- * A bad implementation
-----------------------------------------------------------------------------

-- | A "bad" implementation, see if you can spot the problem with it, before looking
-- at the failed theorem below!
computeLastBad :: Int -> Bool -> Int -> Int
computeLastBad range manual timeSince
   | range > safetyDistance       = 0
   | manual                       = 0
   | timeSince > maxTimeSince - 1 = 0
   | True                         = timeSince + 1

-- | Using SBV, prove that the 'computeLastBad' is indeed a bad implementation. Here's the output
-- we get from the plugin:
--
-- @
--   [SBV] MicroController.hs:85:1-8 Proving "checkBad", using Z3.
--   [Z3] Falsifiable. Counter-example:
--     range     =     0 :: Int64
--     manual    = False :: Bool
--     timeSince =     9 :: Int64
-- @
--
-- We're being told that if the range is 0, and manual override is off, and time-since last is 9,
-- then our "calculator" returns 10. But that violates the @minRate@ requirement, since we
-- never want to go 'maxTimeSince' cycles without sending a signal!
{-# ANN checkBad theorem {options = [IgnoreFailure]} #-}
checkBad :: Int -> Bool -> Int -> Bool
checkBad range manual timeSince = checkSpec computeLastBad range manual timeSince

-----------------------------------------------------------------------------
-- * A correct implementation
-----------------------------------------------------------------------------

-- | A "good" implementation, properly handling the off-by-one error of the original.
computeLastGood :: Int -> Bool -> Int -> Int
computeLastGood range manual timeSince
   | range > safetyDistance       = 0
   | manual                       = 0
   | timeSince > maxTimeSince - 2 = 0
   | True                         = timeSince + 1

-- | We now verify that the good variant is indeed good.
-- We have:
--
-- @
--   [SBV] MicroController.hs:108:1-9 Proving "checkGood", using Z3.
--   [Z3] Q.E.D.
-- @
checkGood :: Proved (Int -> Bool -> Int -> Bool)
checkGood range manual timeSince = checkSpec computeLastGood range manual timeSince

-----------------------------------------------------------------------------
-- * Exercise for the reader
-- $exercise
-----------------------------------------------------------------------------
{- $exercise
It is easy to see that an implementation that always returns @0@ (i.e., one that always
sends a signal) will also pass our specification!

   * First, use the plugin to prove that such an implementation is indeed accepted.

   * Then, modify the spec so that we require the @timeSince@ parameter to actually get
     incremented under the correct conditions.

   * Show that your new spec outlaws the always @0@ producing implementation.
-}

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

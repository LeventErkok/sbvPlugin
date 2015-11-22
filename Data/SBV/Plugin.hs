---------------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- (The sbvPlugin is hosted at <http://github.com/LeventErkok/sbvPlugin>.
-- Comments, bug reports, and patches are always welcome.)
--
-- SBVPlugin: A GHC Plugin for SBV, SMT Based Verification
--
-- <http://github.com/LeventErkok/sbv SBV> is a library for express properties about Haskell programs and
-- automatically proving them using SMT solvers. The SBVPlugin allows
-- simple annotations on Haskell functions to prove them directly during
-- GHC compilation time.
--
-- Consider the following simple program:
--
--  > {-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}
--  >
--  > module Test where
--  >
--  > import Data.SBV.Plugin
--  >
--  > {-# ANN test theorem #-}
--  > test :: Integer -> Integer -> Bool
--  > test x y = x + y >= x - y
--
-- We have:
--
--  > $ ghc -c Test.hs
--  >
--  > [SBV] Test.hs:9:1-4 Proving "test", using Z3.
--  > [Z3] Falsifiable. Counter-example:
--  >   x = 0 :: Integer
--  >   y = -1 :: Integer
--  > [SBV] Failed. (Use option 'WarnIfFails' to continue.)
--
-- Note that the compilation will be aborted, since the theorem doesn't hold. As shown in the hint, GHC
-- can be instructed to continue in that case, using an annotation of the form:
--
-- > {-# ANN test theorem {options = [WarnIfFails]} #-}
--
-- The plugin should work from GHCi with no changes.  Note that when run from GHCi, the plugin will
-- behave as if the /WarnIfFails/ option is given on all annotations, so that failures do not stop
-- the load process.
---------------------------------------------------------------------------------
module Data.SBV.Plugin(
       -- * Entry point
         plugin
       -- * Annotations
       , SBVAnnotation(..)
       , theorem
       , safe
       , uninterpret
       -- * Plugin options
       , SBVOption(..)
       ) where

import Data.SBV.Plugin.Plugin
import Data.SBV.Plugin.Data

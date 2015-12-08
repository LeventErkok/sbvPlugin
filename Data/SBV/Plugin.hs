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
-- == SBVPlugin: A GHC Plugin for SBV, SMT Based Verification
--
-- <http://github.com/LeventErkok/sbv SBV> is a library for express properties about Haskell programs and
-- automatically proving them using SMT solvers. The SBVPlugin allows
-- simple annotations on Haskell functions to prove them directly during
-- GHC compilation time.
--
-- === /Example/
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
-- When compiled via GHC or loaded into GHCi, we get:
--
--  > [SBV] Test.hs:9:1-4 Proving "test", using Z3.
--  > [Z3] Falsifiable. Counter-example:
--  >   x =  0 :: Integer
--  >   y = -1 :: Integer
--  > [SBV] Failed. (Use option 'IgnoreFailure' to continue.)
--
-- Note that the compilation will be aborted, since the theorem doesn't hold. As shown in the hint, GHC
-- can be instructed to continue in that case, using an annotation of the form:
--
-- > {-# ANN test theorem {options = [IgnoreFailure]} #-}
--
-- === /Using the plugin from GHCi/
-- The plugin should work from GHCi with no changes.  Note that when run from GHCi, the plugin will
-- behave as if the @IgnoreFailure@ option is given on all annotations, so that failures do not stop
-- the load process.
--
-- === /Plugin order/
-- By default, sbvPlugin runs before GHCs optimizer passes. While the order of the run should
-- not matter in general, the simplifier can rearrange the core in various ways that can have
-- an impact on the verification conditions generated by the plugin. As an experiment, you can
-- pass the argument @runLast@ to the plugin to see if it makes any difference, using the following
-- argument to GHC:
--
-- @
--   -fplugin-opt Data.SBV.Plugin:runLast
-- @
--
-- Please report if you find any crucial differences when the plugin is run first or last, especially
-- if the outputs are different.
---------------------------------------------------------------------------------
module Data.SBV.Plugin(
       -- * Entry point
         plugin
       -- * Annotations
       , SBVAnnotation(..)
       , sbv, theorem
       -- * Plugin options
       , SBVOption(..)
       ) where

import Data.SBV.Plugin.Plugin
import Data.SBV.Plugin.Data

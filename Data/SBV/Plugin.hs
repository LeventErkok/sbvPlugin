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
-- SBV is a library for express properties about Haskell programs and
-- automatically proving them using SMT solvers. The SBVPlugin allows
-- simple annotations on Haskell functions to prove them directly during
-- GHC compilation time.
--
-- Consider the following simple program:
--
-- >
-- > module Test where
-- >
-- > import Data.SBV.Plugin
-- >
-- > {-# ANN test theorem #-}
-- > test :: Integer -> Integer -> Bool
-- > test x y = x + y >= x - y
-- >
--
-- If we compile this program with the SBVPlugin enabled, SBV will automatically attempt to prove
--  this theorem, and will produce a counter-example at compile time:
--
-- >
-- > $ ghc -c -package sbvPlugin -fplugin=Data.SBV.Plugin Test.hs
-- >
-- > [SBV] Test.hs:7:1-4 Proving "test", using Z3.
-- > [Z3] Falsifiable. Counter-example:
-- >   x = 0 :: Integer
-- >   y = -1 :: Integer
-- > [SBV] Failed. (Use option 'WarnIfFails' to continue.)
-- >
--
-- Note that the compilation will be aborted, since the theorem doesn't hold. As shown in the hint, GHC
-- can be instructed to continue in that case, using an annotation of the form:
--
-- >
-- > {-# ANN test theorem {options = [WarnIfFails]} #-}
-- >
---------------------------------------------------------------------------------
module Data.SBV.Plugin(
       -- * Entry point
         plugin
       -- * Annotations
       , SBVAnnotation(..)
       , theorem
       , safe
       , uninterpret
       -- * Options to the solvers
       , SBVOption(..)
       ) where

import Data.SBV.Plugin.Plugin
import Data.SBV.Plugin.Data

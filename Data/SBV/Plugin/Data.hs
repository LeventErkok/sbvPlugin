---------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Data
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Internal data-structures for the sbvPlugin
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Data.SBV.Plugin.Data where

import Data.Data  (Data, Typeable)

-- | Plugin options. Note that we allow picking multiple solvers, which
-- will all be run in parallel. If you want to run all available solvers,
-- use the option 'AnySolver'. The default is to error-out on failure, using
-- the default-SMT solver picked by SBV, which is currently Z3.
data SBVOption = IgnoreFailure  -- ^ Continue even if proof fails
               | Verbose        -- ^ Produce verbose output, good for debugging
               | Safety         -- ^ Check for safety
               | QuickCheck     -- ^ Perform quickCheck
               | Uninterpret    -- ^ Uninterpret this binding for proof purposes
               | Names [String] -- ^ Use these names for the arguments; need not be exhaustive
               | Z3             -- ^ Use Z3
               | Yices          -- ^ Use Yices
               | Boolector      -- ^ Use Boolector
               | CVC4           -- ^ Use CVC4
               | MathSAT        -- ^ Use MathSAT
               | ABC            -- ^ Use ABC
               | AnySolver      -- ^ Use all installed solvers
               deriving (Show, Eq, Data, Typeable)

-- | The actual annotation.
newtype SBVAnnotation = SBV {options :: [SBVOption]}
                      deriving (Eq, Data, Typeable)

-- | A property annotation, using default options.
sbv :: SBVAnnotation
sbv = SBV {options = []}

-- | Synonym for sbv really, just looks cooler
theorem :: SBVAnnotation
theorem = sbv

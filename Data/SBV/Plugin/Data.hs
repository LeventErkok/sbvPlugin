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

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.SBV.Plugin.Data where

import Data.Data (Data)

-- | Plugin options. Note that we allow picking multiple solvers, which
-- will all be run in parallel. You can pick and choose any number of them,
-- or if you want to run all available solvers, then use the option 'AnySolver'.
-- The default behavior is to error-out on failure, using the default-SMT solver picked by SBV, which is currently Z3.
data SBVOption = IgnoreFailure  -- ^ Continue even if proof fails
               | Skip String    -- ^ Skip the proof. Can be handy for properties that we currently do not want to focus on.
               | Verbose        -- ^ Produce verbose output, good for debugging
               | Debug          -- ^ Produce really verbose output, use only when things go really wrong!
               | QuickCheck     -- ^ Perform quickCheck
               | Uninterpret    -- ^ Uninterpret this binding for proof purposes
               | Names [String] -- ^ Use these names for the arguments; need not be exhaustive
               | ListSize Int   -- ^ If a list-input is found, use this length. If not specified, we will complain!
               | Z3             -- ^ Use Z3
               | Yices          -- ^ Use Yices
               | Boolector      -- ^ Use Boolector
               | CVC4           -- ^ Use CVC4
               | CVC5           -- ^ Use CVC5
               | DReal          -- ^ Use DReal
               | MathSAT        -- ^ Use MathSAT
               | OpenSMT        -- ^ Use OpenSMT
               | ABC            -- ^ Use ABC
               | Bitwuzla       -- ^ Use Bitwuzla
               | AnySolver      -- ^ Run all installed solvers in parallel, and report the result from the first to finish
               deriving (Show, Eq, Data)

-- | The actual annotation.
newtype SBVAnnotation = SBV {options :: [SBVOption]}
                      deriving (Eq, Data)

-- | A property annotation, using default options.
sbv :: SBVAnnotation
sbv = SBV {options = []}

-- | Synonym for sbv really, just looks cooler
theorem :: SBVAnnotation
theorem = sbv

-- | Importable type as an annotation alternative
type Proved a = a

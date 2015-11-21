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
data SBVOption = WarnIfFails    -- ^ Continue even if proof fails
               | Debug          -- ^ Produce verbose output
               | Z3             -- ^ Use Z3
               | Yices          -- ^ Use Yices
               | Boolector      -- ^ Use Boolector
               | CVC4           -- ^ Use CVC4
               | MathSAT        -- ^ Use MathSAT
               | ABC            -- ^ Use ABC
               | AnySolver      -- ^ Use all installed solvers
               deriving (Eq, Data, Typeable)

-- | The actual annotation.
data SBVAnnotation = SBVTheorem      {options :: [SBVOption]}  -- ^ Theorem
                   | SBVSafe         {options :: [SBVOption]}  -- ^ Safety checks
                   | SBVUninterpret                            -- ^ Uninterpeted function\/constant
                   deriving (Eq, Data, Typeable)

-- | A theorem annotation, using default options.
theorem :: SBVAnnotation
theorem = SBVTheorem {options = []}

-- | A safe annotation, using default options.
safe :: SBVAnnotation
safe = SBVSafe {options = []}

-- | An uninterpret, using default options.
uninterpret :: SBVAnnotation
uninterpret = SBVUninterpret

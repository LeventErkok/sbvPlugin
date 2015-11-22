---------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Env
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- The environment for mapping concrete functions/types to symbolic ones.
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Data.SBV.Plugin.Env (buildFunEnv, buildTCEnv) where

import GhcPlugins
import qualified Language.Haskell.TH as TH

import qualified Data.Map as M

import Data.Int
import Data.Word

import qualified Data.SBV         as S hiding (proveWith, proveWithAny)
import qualified Data.SBV.Dynamic as S

import Data.SBV.Plugin.Common

-- | Build the initial environment containing types
buildTCEnv :: CoreM (M.Map TyCon S.Kind)
buildTCEnv = M.fromList `fmap` mapM grabTyCon [ (S.KBool,             ''Bool)
                                              , (S.KUnbounded,        ''Integer)
                                              , (S.KFloat,            ''Float)
                                              , (S.KDouble,           ''Double)
                                              , (S.KBounded True   8, ''Int8)
                                              , (S.KBounded True  16, ''Int16)
                                              , (S.KBounded True  32, ''Int32)
                                              , (S.KBounded True  64, ''Int64)
                                              , (S.KBounded False  8, ''Word8)
                                              , (S.KBounded False 16, ''Word16)
                                              , (S.KBounded False 32, ''Word32)
                                              , (S.KBounded False 64, ''Word64)
                                              ]
  where grabTyCon (k, x) = do Just fn <- thNameToGhcName x
                              tc <- lookupTyCon fn
                              return (tc, k)

-- | Build the initial environment containing functions
buildFunEnv :: CoreM (M.Map (Id, S.Kind) Val)
buildFunEnv = M.fromList `fmap` mapM grabVar binOps
  where grabVar (n, k, sfn) = do Just fn <- thNameToGhcName n
                                 f <- lookupId fn
                                 return ((f, k), sfn)

-- | Binary operatos supported by the plugin.
binOps :: [(TH.Name, S.Kind, Val)]
binOps =  -- equality is for all kinds
          [('(==), k, lift2 S.svEqual) | k <- allKinds]

          -- arithmetic
       ++ [(op,    k, lift2 sOp)       | k <- arithKinds, (op, sOp) <- arithOps]

          -- comparisons
       ++ [(op,    k, lift2 sOp)       | k <- arithKinds, (op, sOp) <- compOps ]
 where
       -- Bit-vectors
       bvKinds    = [S.KBounded s sz | s <- [False, True], sz <- [8, 16, 32, 64]]

       -- Arithmetic kinds
       arithKinds = [S.KUnbounded, S.KFloat, S.KDouble] ++ bvKinds

       -- Everything
       allKinds   = S.KBool : arithKinds

       -- Binary arithmetic UOPs
       arithOps   = [ ('(+), S.svPlus)
                    , ('(-), S.svMinus)
                    , ('(*), S.svTimes)
                    ]

       -- Comparisons
       compOps    = [ ('(<),  S.svLessThan)
                    , ('(>),  S.svGreaterThan)
                    , ('(<=), S.svLessEq)
                    , ('(>=), S.svGreaterEq)
                    ]

-- | Lift a two argument SBV function to our the plugin value space
lift2 :: (S.SVal -> S.SVal -> S.SVal) -> Val
lift2 f = Func $ \(Base a) -> Func $ \(Base b) -> Base (f a b)

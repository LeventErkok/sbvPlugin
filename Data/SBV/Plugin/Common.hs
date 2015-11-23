---------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Common
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Common data-structures/utilities
-----------------------------------------------------------------------------

module Data.SBV.Plugin.Common where

import CostCentre
import GhcPlugins

import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import qualified Data.SBV         as S
import qualified Data.SBV.Dynamic as S

import Data.SBV.Plugin.Data

-- | Configuration info as we run the plugin
data Config = Config { dflags        :: DynFlags
                     , isGHCi        :: Bool
                     , opts          :: [SBVAnnotation]
                     , knownTCs      :: M.Map TyCon S.Kind
                     , knownFuns     :: M.Map (Var, S.Kind) Val
                     , knownSpecials :: M.Map Var Val
                     , sbvAnnotation :: Var -> [SBVAnnotation]
                     , allBinds      :: M.Map Var CoreExpr
                     }

-- | Given the user options, determine which solver(s) to use
pickSolvers :: [SBVOption] -> IO [S.SMTConfig]
pickSolvers slvrs
  | AnySolver `elem` slvrs = S.sbvAvailableSolvers
  | True                   = case mapMaybe (`lookup` solvers) slvrs of
                                [] -> return [S.defaultSMTCfg]
                                xs -> return xs
  where solvers = [ (Z3,        S.z3)
                  , (Yices,     S.yices)
                  , (Boolector, S.boolector)
                  , (CVC4,      S.cvc4)
                  , (MathSAT,   S.mathSAT)
                  , (ABC,       S.abc)
                  ]

-- | The values kept track of by the interpreter
data Val = Base S.SVal
         | Func (Val -> Val)

-- | Compute the span given a Tick. Returns the old-span if the tick span useless.
tickSpan :: Tickish t -> SrcSpan -> SrcSpan
tickSpan (ProfNote cc _ _) _ = cc_loc cc
tickSpan (SourceNote s _)  _ = RealSrcSpan s
tickSpan _                 s = s

-- | Compute the span for a binding.
bindSpan :: Var -> SrcSpan
bindSpan = nameSrcSpan . varName

-- | Show a GHC span in user-friendly form.
showSpan :: Config -> Var -> SrcSpan -> String
showSpan cfg b s = showSDoc (dflags cfg) $ if isGoodSrcSpan s then ppr s else ppr b

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

{-# LANGUAGE NamedFieldPuns       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.SBV.Plugin.Common where

import Control.Monad.Reader

import CostCentre
import GhcPlugins

import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Data.IORef

import qualified Data.SBV         as S
import qualified Data.SBV.Dynamic as S

import Data.SBV.Plugin.Data

-- | Certain "very-polymorphic" things are just special
data Specials = Specials { isEquality :: Var -> Maybe Val
                         , isTuple    :: Var -> Maybe Val
                         }

-- | Interpreter environment
data Env = Env { curLoc         :: SrcSpan
               , flags          :: DynFlags
               , machWordSize   :: Int
               , uninteresting  :: [Type]
               , rUninterpreted :: IORef [((Var, Type), (String, Val))]
               , rUsedNames     :: IORef [String]
               , rUITypes       :: IORef [(Type, S.Kind)]
               , specials       :: Specials
               , tcMap          :: M.Map (TyCon, [TyCon]) S.Kind
               , envMap         :: M.Map (Var, SKind) Val
               , destMap        :: M.Map (Var, SKind) (S.SVal -> [Var] -> (S.SVal, [((Var, SKind), Val)]))
               , coreMap        :: M.Map Var CoreExpr
               }


-- | The interpreter monad
type Eval a = ReaderT Env S.Symbolic a

-- | Configuration info as we run the plugin
data Config = Config { isGHCi        :: Bool
                     , opts          :: [SBVAnnotation]
                     , sbvAnnotation :: Var -> [SBVAnnotation]
                     , cfgEnv        :: Env
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

-- | The kinds used by the plugin
data SKind = KBase S.Kind
           | KFun  S.Kind SKind
           deriving (Eq, Ord)

-- | The values kept track of by the interpreter
data Val = Base S.SVal
         | Typ  S.Kind
         | Tup  [Val]
         | Func (Maybe String) (Val -> Eval Val)

-- | Outputable instance for SKind
instance Outputable SKind where
   ppr (KBase k)   = text (show k)
   ppr (KFun  k r) = text (show k) <+> text "->" <+> ppr r

-- | Outputable instance for S.Kind
instance Outputable S.Kind where
   ppr = text . show

-- | Outputable instance for Val
instance Outputable Val where
   ppr (Base s)   = text (show s)
   ppr (Typ  k)   = text (show k)
   ppr (Tup vs)   = parens $ sep $ punctuate (text ",") (map ppr vs)
   ppr (Func k _) = text ("Func<" ++ show k ++ ">")

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
showSpan Config{cfgEnv} b s = showSDoc (flags cfgEnv) $ if isGoodSrcSpan s then ppr s else ppr b

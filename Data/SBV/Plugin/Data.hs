{-# LANGUAGE DeriveDataTypeable #-}
module Data.SBV.Plugin.Data where

import Data.Data (Data, Typeable)
import qualified Data.Map as M

import CostCentre
import GhcPlugins

import qualified Data.SBV         as S
import qualified Data.SBV.Dynamic as S

-- | SBV Annotations
data SBVAnnotation = SBVTheorem deriving (Eq, Data, Typeable)

-- | Configuration info as we run the plugin
data Config = Config { dflags        :: DynFlags
                     , knownTCs      :: M.Map TyCon S.Kind
                     , knownFuns     :: M.Map (Var, S.Kind) Val
                     , sbvAnnotation :: Var -> [SBVAnnotation]
                     }

data Val = Base S.SVal
         | Func (Val -> Val)

lift2 :: (S.SVal -> S.SVal -> S.SVal) -> Val
lift2 f = Func $ \(Base a) -> Func $ \(Base b) -> Base (f a b)

tickSpan :: Tickish t -> SrcSpan -> SrcSpan
tickSpan (ProfNote cc _ _) _ = cc_loc cc
tickSpan (SourceNote s _)  _ = RealSrcSpan s
tickSpan _                 s = s

bindSpan :: Var -> SrcSpan
bindSpan = nameSrcSpan . varName

showSpan :: Config -> Var -> SrcSpan -> String
showSpan cfg b s = showSDoc (dflags cfg) $ if isGoodSrcSpan s then ppr s else ppr b

{-# LANGUAGE DeriveDataTypeable #-}
module Data.SBV.Plugin.Data where

import Data.Data (Data, Typeable)

import CostCentre
import GhcPlugins

import qualified Data.SBV as SBV

-- | SBV Annotations
data SBVAnnotation = SBVTheorem deriving (Eq, Data, Typeable)

-- | Configuration info as we run the plugin
data Config = Config { dflags        :: DynFlags
                     , knownTCs      :: [(TyCon, SBV.Kind)]
                     , sbvAnnotation :: Var -> [SBVAnnotation]
                     }

tickSpan :: Tickish t -> SrcSpan -> SrcSpan
tickSpan (ProfNote cc _ _) _ = cc_loc cc
tickSpan (SourceNote s _)  _ = RealSrcSpan s
tickSpan _                 s = s

bindSpan :: Var -> SrcSpan
bindSpan = nameSrcSpan . varName

showSpan :: Config -> Var -> SrcSpan -> String
showSpan cfg b s = showSDoc (dflags cfg) $ if isGoodSrcSpan s then ppr s <> text ":" <> ppr b else ppr b

module Data.SBV.PluginData where

import GhcPlugins

import qualified Data.SBV as SBV

-- | Configuration info as we run the plugin
data Config = Config { dflags   :: DynFlags
                     , knownTCs :: [(TyCon, SBV.Kind)]
                     }

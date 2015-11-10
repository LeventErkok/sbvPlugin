module Data.SBV.PluginData where

import GhcPlugins

-- | Configuration info as we run the plugin
data Config = Config { dflags   :: DynFlags
                     , knownTCs :: [TyCon]
                     }

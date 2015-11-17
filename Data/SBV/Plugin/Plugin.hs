{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.SBV.Plugin.Plugin(plugin) where

import GhcPlugins
import Control.Monad (when)

import Data.Int
import Data.Word

import qualified Data.SBV.Internals as SBV

import Data.SBV.Plugin.Data
import Data.SBV.Plugin.Analyze (prove)

-- | Entry point to the plugin
plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install}
 where install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
       install _opts todos = do
          reinitializeGlobals
          return $ sbvPass : todos

       sbvPass = CoreDoPluginPass "SBV based analysis" pass

       pass :: ModGuts -> CoreM ModGuts
       pass guts@(ModGuts {mg_binds}) = do

          df <- getDynFlags

          let grabTyCon (k, x) = do Just fn <- thNameToGhcName x
                                    tc <- lookupTyCon fn
                                    return (tc, k)

          baseTCs <- mapM grabTyCon [ (SBV.KBool,             ''Bool)
                                    , (SBV.KUnbounded,        ''Integer)
                                    , (SBV.KFloat,            ''Float)
                                    , (SBV.KDouble,           ''Double)
                                    , (SBV.KBounded True   8, ''Int8)
                                    , (SBV.KBounded True  16, ''Int16)
                                    , (SBV.KBounded True  32, ''Int32)
                                    , (SBV.KBounded True  64, ''Int64)
                                    , (SBV.KBounded False  8, ''Word8)
                                    , (SBV.KBounded False 16, ''Word16)
                                    , (SBV.KBounded False 32, ''Word32)
                                    , (SBV.KBounded False 64, ''Word64)
                                    ]

          anns <- getAnnotations deserializeWithData guts

          let cfg = Config { dflags        = df
                           , knownTCs      = baseTCs
                           , sbvAnnotation = lookupWithDefaultUFM anns [] . varUnique
                           }

          mapM_ (analyzeBind cfg) mg_binds

          return guts

-- | Dispatch the analyzer recursively over subexpressions.
analyzeBind :: Config -> CoreBind -> CoreM ()
analyzeBind cfg@Config{sbvAnnotation} = go
  where go (NonRec b e) = bind (b, e)
        go (Rec binds)  = mapM_ bind binds
        bind (b, e) = do let anns = sbvAnnotation b
                         when (SBVTheorem `elem` anns) $ liftIO $ prove cfg b (bindSpan b) e

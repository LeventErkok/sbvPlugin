{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.SBV.SBVPlugin(plugin) where

import GhcPlugins

import Control.Monad (unless)

import Data.Int
import Data.Word

import Data.SBV.PluginData
import Data.SBV.SBVAnalyze (analyze)

-- | Entry point to the plugin
plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install}
 where install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
       install _opts todos = do
          reinitializeGlobals

          df <- getDynFlags

          let grabTyCon x = thNameToGhcName x >>= \(Just fn) -> lookupTyCon fn

          baseTCs <- mapM grabTyCon [ ''Bool
                                    , ''Int
                                    , ''Float, ''Double
                                    , ''Int8,  ''Int16,  ''Int32,  ''Int64
                                    , ''Word8, ''Word16, ''Word32, ''Word64
                                    ]

          let cfg = Config { dflags   = df
                           , knownTCs = baseTCs
                           }

          return $ sbvPass cfg : todos
       sbvPass = CoreDoPluginPass "SBV based analysis" . pass
       pass :: Config -> ModGuts -> CoreM ModGuts
       pass cfg guts@(ModGuts {mg_binds}) = do mapM_ (analyzeBind cfg) mg_binds
                                               return guts

-- | Dispatch the analyzer recursively over subexpressions.
analyzeBind :: Config -> CoreBind -> CoreM ()
analyzeBind cfg = bind noSrcSpan
  where bind l (NonRec _ e)    = expr l e
        bind l (Rec binds)     = mapM_ (expr l . snd) binds

        expr l e = do analyzed <- liftIO $ analyze cfg l e
                      unless analyzed $ subExpr l e

        subExpr _ (Var{})         = return ()
        subExpr _ (Lit{})         = return ()
        subExpr l (App f a)       = expr l f >> expr l a
        subExpr l (Lam _ b)       = expr l b
        subExpr l (Let b e)       = bind l b >> expr l e
        subExpr l (Case e _ _ as) = expr l e >> mapM_ (alt l) as
        subExpr l (Cast e _)      = expr l e
        subExpr l (Tick t e)      = expr (tickSpan t l) e
        subExpr _ (Type _)        = return ()
        subExpr _ (Coercion _)    = return ()

        alt l (_, _, e)        = expr l e

        tickSpan (SourceNote l _) _ = RealSrcSpan l
        tickSpan _                l = l

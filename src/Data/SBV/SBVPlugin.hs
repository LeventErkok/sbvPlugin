{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.SBV.SBVPlugin(plugin) where

import GhcPlugins

import Data.SBV.PluginData
import Data.SBV.SBVAnalyze (analyze)

-- | Entry point to the plugin
plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install}
 where install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
       install _opts todos = do reinitializeGlobals

                                df <- getDynFlags

                                Just fn <- thNameToGhcName ''Double
                                ftc     <- lookupTyCon fn

                                Just dn <- thNameToGhcName ''Float
                                dtc     <- lookupTyCon dn

                                let cfg = Config { dflags   = df
                                                 , floatTC  = ftc
                                                 , doubleTC = dtc
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

        expr _ (Var{})         = return ()
        expr _ (Lit{})         = return ()
        expr l (App f a)       = expr l f >> expr l a
        expr l e@(Lam{})       = liftIO $ analyze cfg l e
        expr l (Let b e)       = bind l b >> expr l e
        expr l (Case e _ _ as) = expr l e >> mapM_ (alt l) as
        expr l (Cast e _)      = expr l e
        expr l (Tick t e)      = expr (tickSpan t l) e
        expr _ (Type _)        = return ()
        expr _ (Coercion _)    = return ()

        alt l (_, _, e)        = expr l e

        tickSpan (SourceNote l _) _ = RealSrcSpan l
        tickSpan _                l = l

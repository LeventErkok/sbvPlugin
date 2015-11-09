{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.SBVPlugin(plugin) where

import GhcPlugins
import PprCore

import Data.SBV.SBVAnalyze (analyze)

plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _opts todos = do reinitializeGlobals
                         return $ sbvPass : todos
  where sbvPass = CoreDoPluginPass "SBV based analysis" pass
        pass :: ModGuts -> CoreM ModGuts
        pass guts@(ModGuts {mg_binds}) = do mapM_ analyzeBind mg_binds
                                            return guts

analyzeBind :: CoreBind -> CoreM ()
analyzeBind = bind noSrcSpan
  where bind l (NonRec _ e)    = expr l e
        bind l (Rec binds)     = mapM_ (expr l . snd) binds

        expr _ (Var{})         = return ()
        expr _ (Lit{})         = return ()
        expr l (App f a)       = expr l f >> expr l a
        expr l e@(Lam{})       = analyze l e
        expr l (Let b e)       = bind l b >> expr l e
        expr l (Case e _ _ as) = expr l e >> mapM_ (alt l) as
        expr l (Cast e _)      = expr l e
        expr l (Tick t e)      = expr (tickSpan t l) e
        expr _ (Type _)        = return ()
        expr _ (Coercion _)    = return ()

        alt l (_, _, e)        = expr l e

        tickSpan (SourceNote l _) _ = RealSrcSpan l
        tickSpan _                l = l

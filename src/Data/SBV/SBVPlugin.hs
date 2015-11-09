{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.SBVPlugin(plugin) where

import GhcPlugins
import PprCore

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
analyzeBind = bind
  where bind (NonRec _ e)    = expr e
        bind (Rec binds)     = mapM_ (expr . snd) binds

        expr (Var{})         = return ()
        expr (Lit{})         = return ()
        expr (App f a)       = expr f >> expr a
        expr l@(Lam{})       = analyze l
        expr (Let b e)       = bind b >> expr e
        expr (Case e _ _ as) = expr e >> mapM_ alt as
        expr (Cast e _)      = expr e
        expr (Tick _ e)      = expr e
        expr (Type _)        = return ()
        expr (Coercion _)    = return ()

        alt (_, _, e)        = expr e

        skipping e = do df <- getDynFlags
                        liftIO $ putStrLn $ "SBVPlugin, skipping: " ++ showSDoc df (pprCoreExpr e)

        analyze e@(Lam{} ) = do df <- getDynFlags
                                liftIO $ putStrLn $ "SBVPlugin, considering: " ++ showSDoc df (pprCoreExpr e)
        analyze e          = skipping e

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
analyzeBind (NonRec _ expr) = analyzeExpr expr
analyzeBind (Rec binds)     = mapM_ (analyzeExpr . snd) binds

analyzeExpr :: CoreExpr ->  CoreM ()
analyzeExpr e = do df <- getDynFlags
                   liftIO $ print $ showSDoc df $ pprCoreExpr e

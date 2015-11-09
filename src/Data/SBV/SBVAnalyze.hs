module Data.SBV.SBVAnalyze (analyze) where

import GhcPlugins
import PprCore

analyze :: SrcSpan -> CoreExpr -> CoreM ()
analyze s e = do df <- getDynFlags
                 liftIO $ putStrLn $ "[SBVPlugin] " ++ showLoc s ++ showSDoc df (pprCoreExpr e)
  where showLoc rs@(RealSrcSpan{}) = show rs ++ ": "
        showLoc _                  = ""

{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.SBVAnalyze (analyze) where

import GhcPlugins
import PprCore

import Data.SBV.PluginData

-- | Create a basic tag for the current analysis step
tag :: String -> Maybe String -> String
tag w Nothing  = "[SBVPlugin, " ++ w ++ "]"
tag w (Just l) = "[SBVPlugin, " ++ w ++ "] " ++ l ++ ":"

-- | Analyze a core-expression
analyze :: Config -> SrcSpan -> CoreExpr -> IO Bool
analyze cfg s e
  | isInteresting cfg t = stable cfg loc e
  | True                = return False
  where t = exprType e
        loc = if isGoodSrcSpan s then Just (showSDoc (dflags cfg) (ppr s)) else Nothing

-- | Check if a given expression is interesting, i.e., something we can prove something about
isInteresting :: Config -> Type -> Bool
isInteresting Config{knownTCs} t = case splitTyConApp_maybe t of
                                     Just (tc, []) -> tc `elem` knownTCs
                                     _             -> False

-- | Check if a Floating-Point function is stable. i.e., given normal-arguments, it should
-- produce normal results.
stable :: Config -> Maybe String -> CoreExpr -> IO Bool
stable cfg l e = do putStrLn $ t ++ " " ++ showSDoc (dflags cfg) (pprCoreExpr e)
                    return True
  where t = tag "stable" l

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
  | isInteresting cfg t = do stable cfg loc e
                             return True
  | True                = return False
  where t = exprType e
        loc = case s of
                RealSrcSpan{} -> Just $ show s
                _             -> Nothing

-- | Check if a given expression is interesting, i.e., something we can prove something about
isInteresting :: Config -> Type -> Bool
isInteresting Config{knownTCs} t = case splitTyConApp_maybe t of
                                     Just (tc, []) -> tc `elem` knownTCs
                                     _             -> False

-- | Check if a Floating-Point function is stable. i.e., given normal-arguments, it should
-- produce normal results.
stable :: Config -> Maybe String -> CoreExpr -> IO ()
stable cfg l e = putStrLn $ t ++ " " ++ showSDoc (dflags cfg) (pprCoreExpr e)
  where t = tag "stable" l

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
analyze :: Config -> SrcSpan -> CoreExpr -> IO ()
analyze cfg s e
  | Just arity <- isFloatingFunc cfg t = stable cfg loc arity e
  | True                               = return ()
  where t = exprType e
        loc = case s of
                RealSrcSpan{} -> Just $ show s
                _             -> Nothing

-- | Check if we have a function that takes a number of double/floats and returns a double or float
-- Currently, we only support simple types, no tupling, or other parameters. Clearly this is
-- not sufficient, but it is a good start.
isFloatingFunc :: Config -> Type -> Maybe Int
isFloatingFunc Config{floatTC, doubleTC} = go
    where isF  tc = tc `elem` [floatTC, doubleTC]
          isFT t  = case splitTyConApp_maybe t of
                      Just (tc, []) -> isF tc
                      _             -> False
          go t | isFT t = Just 1
               | True   = case splitTyConApp_maybe t of
                            Just (tc, [a, b]) | isFunTyCon  tc && isFT a  -> (+1) `fmap` go b
                            _                                             -> Nothing

-- | Check if a Floating-Point function is stable. i.e., given normal-arguments, it should
-- produce normal results.
stable :: Config -> Maybe String -> Int -> CoreExpr -> IO ()
stable cfg l a e = putStrLn $ t ++ " " ++ showSDoc (dflags cfg) (pprCoreExpr e)
  where t = tag ("stable(" ++ show a ++ ")") l

{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.SBVAnalyze (analyze) where

import GhcPlugins
import PprCore

import qualified Data.SBV as S

import Data.Maybe

import Data.SBV.PluginData

-- | Create a basic tag for the current analysis step
tag :: String -> Maybe String -> String
tag w Nothing  = "[SBVPlugin, " ++ w ++ "]"
tag w (Just l) = "[SBVPlugin, " ++ w ++ "] " ++ l ++ ":"

-- | Analyze a core-expression
analyze :: Config -> SrcSpan -> CoreExpr -> IO Bool
analyze cfg s e
  | Just k <- getBaseType cfg t = stable cfg k loc e
  | True                        = return False
  where t = exprType e
        loc = if isGoodSrcSpan s then Just (showSDoc (dflags cfg) (ppr s)) else Nothing

-- | Check if a given expression is interesting, i.e., something we can prove something about
getBaseType :: Config -> Type -> Maybe S.Kind
getBaseType Config{knownTCs} t = case splitTyConApp_maybe t of
                                   Just (tc, []) -> tc `lookup` knownTCs
                                   _             -> Nothing

allBaseTypes :: Config -> VarSet -> Bool
allBaseTypes cfg = isEmptyUniqSet . filterUniqSet (isNothing . getBaseType cfg . varType)

-- | Check if a Floating-Point function is stable. i.e., given normal-arguments, it should
-- produce normal results. Returns 'True' if it did analysis, otherwise 'False' to indicate
-- that the caller should look for subexpressions
stable :: Config -> S.Kind -> Maybe String -> CoreExpr -> IO Bool
stable cfg k l e
  | not ((S.isDouble k || S.isFloat k) && allKnown)
  = return False
  | True
  = case e of
      Var{}      -> return True
      Lit{}      -> return True
      Lam{}      -> return False
      Cast{}     -> return False
      Tick{}     -> return False
      Type{}     -> return False
      Coercion{} -> return False
      App{}      -> todo "App"
      Let{}      -> todo "Let"
      Case{}     -> todo "Case"
  where allKnown = allBaseTypes cfg (exprFreeVars e)
        todo w = do let t = tag ("stable(" ++ show k ++ ", " ++ w ++ ")") l
                    putStrLn $ t ++ " " ++ showSDoc (dflags cfg) (pprCoreExpr e)
                    return True

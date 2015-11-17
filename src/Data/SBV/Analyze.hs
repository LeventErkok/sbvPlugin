{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.SBVAnalyze (analyze) where

import GhcPlugins
import PprCore

import qualified Data.Map as M

import qualified Data.SBV           as S
import qualified Data.SBV.Dynamic   as S
import qualified Data.SBV.Internals as S

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
      App{}      -> interpret cfg l e
      Let{}      -> interpret cfg l e
      Case{}     -> interpret cfg l e
  where allKnown = allBaseTypes cfg (exprFreeVars e)

interpret :: Config -> Maybe String -> CoreExpr -> IO Bool
interpret cfg l topExpr = do mbR <- S.runSymbolic' (S.Proof (False, S.defaultSMTCfg)) $ go M.empty topExpr
                             case mbR of
                               (Nothing,     _) -> do todo
                                                      return False
                               (Just (m, r), _) -> do print r
                                                      todo
                                                      return False -- will be "True" eventually
  where sh = showSDoc (dflags cfg) . pprCoreExpr
        go m e@(Var v) = case v `M.lookup` m of
                           Just s  -> return $ Just (m, s)
                           Nothing -> case getBaseType cfg (exprType e) of
                                        Just k  -> do s <- S.svMkSymVar Nothing k (Just (sh e))
                                                      return $ Just (M.insert v s m, s)
                                        Nothing -> return Nothing
        go _ _         = return Nothing
        todo  = liftIO $ putStrLn $ tag "stable" l ++ " " ++ sh topExpr

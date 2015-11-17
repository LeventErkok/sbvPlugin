{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.Plugin.Analyze (prove) where

import GhcPlugins
import PprCore

import qualified Data.Map as M

import qualified Data.SBV           as S
import qualified Data.SBV.Dynamic   as S
import qualified Data.SBV.Internals as S

import Data.SBV.Plugin.Data

-- | Prove an SBVTheorem
prove :: Config -> Var -> SrcSpan -> CoreExpr -> IO ()
prove cfg b s e
  | isProvable (exprType e) = checkTheorem cfg loc e
  | True                    = error $ "SBVPlugin: " ++ loc ++ " does not have a provable type!"
  where loc = showSDoc (dflags cfg) $ if isGoodSrcSpan s then ppr s <> text ":" <> ppr b else ppr b

-- | Is this a provable type?
-- TODO: Currently we always say yes!
isProvable :: Type -> Bool
isProvable _ = True

getBaseType :: Config -> Type -> Maybe S.Kind
getBaseType Config{knownTCs} t = case splitTyConApp_maybe t of
                                   Just (tc, []) -> tc `lookup` knownTCs
                                   _             -> Nothing

checkTheorem :: Config -> String -> CoreExpr -> IO ()
checkTheorem cfg l topExpr = do
        mbR <- S.runSymbolic' (S.Proof (False, S.defaultSMTCfg)) $ go M.empty topExpr
        case mbR of
          (Nothing,     _) -> todo
          (Just (_, r), _) -> do print r
                                 todo
  where sh = showSDoc (dflags cfg) . pprCoreExpr
        go m e@(Var v) = case v `M.lookup` m of
                           Just s  -> return $ Just (m, s)
                           Nothing -> case getBaseType cfg (exprType e) of
                                        Just k  -> do s <- S.svMkSymVar Nothing k (Just (sh e))
                                                      return $ Just (M.insert v s m, s)
                                        Nothing -> return Nothing
        go _ _         = return Nothing
        todo  = liftIO $ putStrLn $ l ++ ": " ++ sh topExpr

{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.Plugin.Analyze (prove) where

import GhcPlugins

import Data.List
import qualified Data.Map as M

import qualified Data.SBV           as S hiding (proveWith)
import qualified Data.SBV.Dynamic   as S

import qualified Control.Exception as C

import Data.SBV.Plugin.Data

-- | Prove an SBVTheorem
prove :: Config -> Var -> SrcSpan -> CoreExpr -> IO ()
prove cfg b s e
  | isProvable (exprType e) = safely $ checkTheorem cfg loc e
  | True                    = error $ "SBVPlugin: " ++ loc ++ " does not have a provable type!"
  where loc = showSDoc (dflags cfg) $ if isGoodSrcSpan s then ppr s <> text ":" <> ppr b else ppr b

-- | Is this a provable type?
-- TODO: Currently we always say yes!
isProvable :: Type -> Bool
isProvable _ = True

safely :: IO () -> IO ()
safely a = a `C.catch` bad
  where bad :: C.SomeException -> IO ()
        bad = print

getBaseType :: Config -> Type -> Maybe S.Kind
getBaseType Config{knownTCs} t = case splitTyConApp_maybe t of
                                   Just (tc, []) -> tc `lookup` knownTCs
                                   _             -> Nothing

type Env = M.Map Var S.SVal

checkTheorem :: Config -> String -> CoreExpr -> IO ()
checkTheorem cfg l topExpr = print =<< S.proveWith S.defaultSMTCfg (snd `fmap` go M.empty topExpr)
  where tbd w es = error $ intercalate "\n" $ tag ("Skipping proof. " ++ w ++ ":") : map (tag . tab) es
          where tag s = "[SBVPlugin:" ++ l ++ "] " ++ s
                tab s = "    " ++ s

        sh o = showSDoc (dflags cfg) (ppr o)

        go :: Env -> CoreExpr -> S.Symbolic (Env, S.SVal)

        go m e@(Var v)
           | Just s <- v `M.lookup` m
           = return (m, s)
           | True
           = tbd "Expression refers to non-local variable" [sh e]

        go m e@(Lam b body)
           | Just k <- getBaseType cfg (varType b)
           = do s <- S.svMkSymVar Nothing k (Just (sh b))
                go (M.insert b s m) body
           | True
           = tbd "Abstraction with a non-basic binder" [sh e]

        go _ e
          = tbd "Expression with too complicated structure" [sh e]

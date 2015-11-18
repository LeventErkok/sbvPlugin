{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.Plugin.Analyze (prove) where

import GhcPlugins

import Data.List
import qualified Data.Map as M

import qualified Data.SBV         as S hiding (proveWith)
import qualified Data.SBV.Dynamic as S

import qualified Control.Exception as C

import Data.SBV.Plugin.Data

-- | Prove an SBVTheorem
prove :: Config -> Var -> SrcSpan -> CoreExpr -> IO ()
prove cfg b topLoc e
  | isProvable (exprType e) = safely $ checkTheorem cfg (topLoc, b) e
  | True                    = error $ "SBVPlugin: " ++ showSpan cfg b topLoc ++ " does not have a provable type!"

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
                                   Just (tc, []) -> tc `M.lookup` knownTCs
                                   _             -> Nothing

checkTheorem :: Config -> (SrcSpan, Var) -> CoreExpr -> IO ()
checkTheorem cfg (topLoc, topBind) topExpr = print =<< S.proveWith S.defaultSMTCfg res
  where res = do (_, v) <- go topLoc (knownFuns cfg) topExpr
                 case v of
                   Base r -> return r
                   Func _ -> tbd topLoc "Expression too complicated for SBVPlugin" [sh topExpr]

        tbd loc w es = error $ intercalate "\n" $ tag ("Skipping proof. " ++ w ++ ":") : map (tag . tab) es
           where tag s = "[SBVPlugin:" ++ showSpan cfg topBind loc ++ "] " ++ s
                 tab s = "    " ++ s

        sh o = showSDoc (dflags cfg) (ppr o)

        go :: SrcSpan -> Env -> CoreExpr -> S.Symbolic (Env, Val)
        go loc m e@(Var v)
           | Just s <- v `M.lookup` m
           = return (m, s)
           | True
           = tbd loc "Expression refers to non-local variable" [sh e]

        go loc _ e@(Lit _)
           = tbd loc "Unsupported literal" [sh e]

        go loc m (App a (Type _))
           = go loc m a

        go loc m (App f e)
           = do (_, fv) <- go loc m f
                (_, ev) <- go loc m e
                case fv of
                  Base _ -> tbd loc "Unsupported application" [sh f, sh e]
                  Func sf -> return (m, sf ev)

        -- NB: We do *not* have to worry about shadowing when we enter the body
        -- of a lambda, as Core variables are guaranteed unique
        go loc m e@(Lam b body)
           | Just k <- getBaseType cfg (varType b)
           = do s <- S.svMkSymVar Nothing k (Just (sh b))
                go loc (M.insert b (Base s) m) body
           | True
           = tbd loc "Abstraction with a non-basic binder" [sh e]

        go loc _ e@(Let _ _)
           = tbd loc "Unsupported let-binding" [sh e]

        go loc _ e@(Case{})
           = tbd loc "Unsupported case-expression" [sh e]

        go loc _ e@(Cast{})
           = tbd loc "Unsupported cast-expression" [sh e]

        go loc m (Tick t e)
           = go (tickSpan t loc) m e

        go loc _ e@(Type{})
           = tbd loc "Unsupported type-expression" [sh e]

        go loc _ e@(Coercion{})
           = tbd loc "Unsupported coercion-expression" [sh e]

---------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Analyze
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Walk the GHC Core, proving theorems/checking safety as they are found
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.SBV.Plugin.Analyze (analyzeBind) where

import GhcPlugins

import Control.Monad.Reader
import System.Exit hiding (die)

import Data.List  (intercalate, partition)
import qualified Data.Map as M

import qualified Data.SBV           as S hiding (proveWith, proveWithAny)
import qualified Data.SBV.Dynamic   as S

import qualified Control.Exception as C

import Data.SBV.Plugin.Common
import Data.SBV.Plugin.Data

-- | Dispatch the analyzer recursively over subexpressions
analyzeBind :: Config -> CoreBind -> CoreM ()
analyzeBind cfg@Config{sbvAnnotation} = go
  where go (NonRec b e) = bind (b, e)
        go (Rec binds)  = mapM_ bind binds

        bind (b, e) = mapM_ work (sbvAnnotation b)
          where work (SBV opts)
                 | Safety `elem` opts      = error "SBV: Safety pragma is not implemented yet"
                 | Uninterpret `elem` opts = error "SBV: Uninterpret pragma is not implemented yet"
                 | True                    = liftIO $ prove cfg opts b (bindSpan b) e

-- | Prove an SBVTheorem
prove :: Config -> [SBVOption] -> Var -> SrcSpan -> CoreExpr -> IO ()
prove cfg@Config{isGHCi} opts b topLoc e = do
        success <- safely $ proveIt cfg opts (topLoc, b) e
        unless (success || isGHCi || IgnoreFailure `elem` opts) $ do
            putStrLn $ "[SBV] Failed. (Use option '" ++ show IgnoreFailure ++ "' to continue.)" 
            exitFailure

-- | Safely execute an action, catching the exceptions, printing and returning False if something goes wrong
safely :: IO Bool -> IO Bool
safely a = a `C.catch` bad
  where bad :: C.SomeException -> IO Bool
        bad e = do print e
                   return False

instance Outputable S.Kind where
   ppr = text . show

instance Outputable Val where
   ppr (Base s)   = text (show s)
   ppr (Func k _) = text ("Func<" ++ show k ++ ">")

-- | Returns True if proof went thru
proveIt :: Config -> [SBVOption] -> (SrcSpan, Var) -> CoreExpr -> IO Bool
proveIt cfg opts (topLoc, topBind) topExpr = do
        solverConfigs <- pickSolvers opts
        let verbose = Verbose    `elem` opts
            qCheck  = QuickCheck `elem` opts
            runProver prop
              | qCheck = Left  `fmap` S.svQuickCheck prop
              | True   = Right `fmap` S.proveWithAny [s{S.verbose = verbose} | s <- solverConfigs] prop
            loc = "[SBV] " ++ showSpan cfg topBind topLoc
            slvrTag = ", using " ++ tag ++ "."
              where tag = case solverConfigs of
                            []     -> "no solvers"  -- can't really happen
                            [x]    -> show x
                            [x, y] -> show x ++ " and " ++ show y
                            xs     -> intercalate ", " (map show (init xs)) ++ ", and " ++ show (last xs)
        putStrLn $ "\n" ++ loc ++ (if qCheck then " QuickChecking " else " Proving ") ++ show (sh topBind) ++ slvrTag
        finalResult <- runProver res
        case finalResult of
          Right (solver, sres@(S.ThmResult smtRes)) -> do
                let success = case smtRes of
                                S.Unsatisfiable{} -> True
                                S.Satisfiable{}   -> False
                                S.Unknown{}       -> False   -- conservative
                                S.ProofError{}    -> False   -- conservative
                                S.TimeOut{}       -> False   -- conservative
                putStr $ "[" ++ show solver ++ "] "
                print sres
                return success
          Left success -> return success

  where res :: S.Symbolic S.SVal
        res = do v <- runReaderT (symEval topExpr) Env{ curLoc  = topLoc
                                                      , flags   = dflags        cfg
                                                      , envMap  = knownFuns     cfg
                                                      , baseTCs = knownTCs      cfg
                                                      , specMap = knownSpecials cfg
                                                      , coreMap = allBinds      cfg
                                                      }
                 case v of
                   Base r -> return r
                   Func{} -> error "Impossible happened. Final result reduced to a non-base value!"

        die :: SrcSpan -> String -> [String] -> a
        die loc w es = error $ concatMap ("\n" ++) $ tag ("Skipping proof. " ++ w ++ ":") : map tab es
          where marker = "[SBV] " ++ showSpan cfg topBind loc
                tag s = marker ++ " " ++ s
                tab s = replicate (length marker) ' ' ++  "    " ++ s

        tbd :: String -> [String] -> Eval Val
        tbd w ws = do Env{curLoc} <- ask
                      die curLoc w ws

        sh o = showSDoc (dflags cfg) (ppr o)

        -- Given an alleged theorem, first establish it has the right type, and
        -- then go ahead and evaluate it symbolicly after applying it to sufficient
        -- number of symbolic arguments
        symEval :: CoreExpr -> ReaderT Env S.Symbolic Val
        symEval e = do let (bs, body) = collectBinders e
                       ats <- mapM (\b -> getBaseType (varType b) >>= \bt -> return (b, bt)) bs
                       let mkVar ((b, k), mbN) = do v <- S.svMkSymVar Nothing k (mbN `mplus` Just (sh b))
                                                    return ((b, k), Base v)
                       sArgs <- mapM (lift . mkVar) (zip ats (concat [map Just ns | Names ns <- opts] ++ repeat Nothing))
                       local (\env -> env{envMap = foldr (uncurry M.insert) (envMap env) sArgs}) (go body)

        -- Main symbolic evaluator:
        go :: CoreExpr -> ReaderT Env S.Symbolic Val

        -- go e | trace ("--> " ++ show (sh e)) False = undefined

        go e@(Var v) = do Env{envMap, coreMap, specMap} <- ask
                          k <- getBaseType (exprType e)
                          case (v, k) `M.lookup` envMap of
                            Just b  -> return b
                            Nothing -> case v `M.lookup` coreMap of
                                          Just b  -> go b
                                          Nothing -> case v `M.lookup` specMap of
                                                        Just b  -> return b
                                                        Nothing -> uninterpret e

        go (Lit (LitInteger i t))
           = do k <- getBaseType t
                return $ Base $ S.svInteger k i

        go (Lit (MachFloat i))
           = return $ Base $ S.svFloat (fromRational i)

        go (Lit (MachDouble i))
           = return $ Base $ S.svDouble (fromRational i)

        go e@(Lit _)
           = tbd "Unsupported literal" [sh e]

        go e@(App (App (Var v) (Type t)) (Var dict))
           | isReallyADictionary dict = do Env{envMap} <- ask
                                           k <- getBaseType t
                                           case (v, k) `M.lookup` envMap of
                                              Just b -> return b
                                              _      -> uninterpret e
        go (App a (Type _))
           = go a

        go (App f e)
           = do func <- go f
                arg  <- go e
                case (func, arg) of
                  (Func (k, _) sf, Base sv) | S.kindOf sv == k     -> sf sv
                  (Base fv,        Base _)  | S.isUninterpreted fv -> tbd "Uninterpreted function application" [sh f, sh e]
                  (_,              Func{})                         -> tbd "Unsupported higher-order application" [sh f, sh e]
                  _                                                -> error $ "[SBV] Impossible happened. Got an application with mismatched types: " ++ show [sh f, sh e]

        go (Lam b body) = do
            k <- getBaseType (varType b)
            return $ Func (k, Just (sh b)) $ \s -> local (\env -> env{envMap = M.insert (b, k) (Base s) (envMap env)}) (go body)

        go e@(Let _ _)
           = tbd "Unsupported let-binding" [sh e]

        -- Case expressions. We take advantage of the core-invariant that each case alternative
        -- is exhaustive; and DEFAULT (if present) is the first alternative. We turn it into a
        -- simple if-then-else chain with the last element on the DEFAULT, or whatever comes last.
        go e@(Case ce _b _t alts)
           = do sce <- go ce
                let isDefault (DEFAULT, _, _) = True
                    isDefault _               = False
                    (nonDefs, defs) = partition isDefault alts
                    walk [(_, _, rhs)]        = go rhs
                    walk ((p, _, rhs) : rest) = case sce of
                                                   Base a -> do mr <- match a p
                                                                case mr of
                                                                  Just m  -> choose m (go rhs) (walk rest)
                                                                  Nothing -> caseTooComplicated "with-complicated-match" ["MATCH " ++ sh (ce, p), " --> " ++ sh rhs]
                                                   _      -> caseTooComplicated "with-non-base-scrutinee" []
                    walk []                     = caseTooComplicated "with-non-exhaustive-match" []  -- can't really happen
                walk (nonDefs ++ defs)
           where caseTooComplicated w [] = tbd ("Unsupported case-expression (" ++ w ++ ")") [sh e]
                 caseTooComplicated w xs = tbd ("Unsupported case-expression (" ++ w ++ ")") $ [sh e, "While Analyzing:"] ++ xs
                 choose t tb fb = case S.svAsBool t of
                                     Nothing    -> do stb <- tb
                                                      sfb <- fb
                                                      case (stb, sfb) of
                                                        (Base a, Base b) -> return $ Base $ S.svIte t a b
                                                        _                -> caseTooComplicated "with-non-base-alternatives" []
                                     Just True  -> tb
                                     Just False -> fb
                 match :: S.SVal -> AltCon -> Eval (Maybe S.SVal)
                 match a c = case c of
                               DEFAULT    -> return $ Just S.svTrue
                               LitAlt  l  -> do le <- go (Lit l)
                                                case le of
                                                  Base b -> return $ Just $ a `S.svEqual` b
                                                  Func{} -> return Nothing
                               DataAlt dc -> do Env{specMap} <- ask
                                                case dataConWorkId dc `M.lookup` specMap of
                                                  Just (Base b) -> return $ Just $ a `S.svEqual` b
                                                  _             -> return Nothing

        go (Cast e _)
           = go e

        go (Tick t e)
           = local (\envMap -> envMap{curLoc = tickSpan t (curLoc envMap)}) $ go e

        go e@(Type{})
           = tbd "Unsupported type-expression" [sh e]

        go e@(Coercion{})
           = tbd "Unsupported coercion-expression" [sh e]

-- | Uninterpret an expression.
-- TODO: Only supports base values. Need to extend to functions.
uninterpret :: CoreExpr -> Eval Val
uninterpret expr = do Env{flags} <- ask
                      k <- getBaseType (exprType expr)
                      return $ Base $ S.svUninterpreted k (showSDoc flags (ppr expr))  Nothing []


-- | Is this variable really a dictionary?
isReallyADictionary :: Var -> Bool
isReallyADictionary v = case classifyPredType (varType v) of
                          ClassPred{} -> True
                          EqPred{}    -> True
                          TuplePred{} -> True
                          IrredPred{} -> False

-- | Convert a Core type to an SBV kind, if known
-- Otherwise, create an uninterpreted kind, and return that.
getBaseType :: Type -> Eval S.Kind
getBaseType t = do Env{baseTCs, flags} <- ask
                   let uninterpreted = S.KUserSort (showSDoc flags (ppr t)) (Left "originating from sbvPlugin")
                   case splitTyConApp_maybe t of
                     Just (tc, []) -> case tc `M.lookup` baseTCs of
                                         Just k  -> return k
                                         Nothing -> return uninterpreted
                     _             -> return uninterpreted

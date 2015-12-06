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

import Data.IORef

import Data.Char  (isAlpha, isAlphaNum)
import Data.List  (intercalate, partition, nub, sortBy)
import Data.Maybe (isJust, listToMaybe)
import Data.Ord   (comparing)

import qualified Data.Map as M

import qualified Data.SBV           as S hiding (proveWith, proveWithAny)
import qualified Data.SBV.Dynamic   as S
import qualified Data.SBV.Internals as S

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
                 | Just s <- hasSkip opts  = liftIO $ putStrLn $ "[SBV] " ++ showSpan cfg b topLoc ++ " Skipping " ++ show (showSDoc (dflags cfg) (ppr b)) ++ ": " ++ s
                 | Safety `elem` opts      = error "SBV: Safety pragma is not implemented yet"
                 | Uninterpret `elem` opts = return ()
                 | True                    = liftIO $ prove cfg opts b topLoc e
                hasSkip opts = listToMaybe [s | Skip s <- opts]
                topLoc       = bindSpan b

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
proveIt cfg@Config{sbvAnnotation} opts (topLoc, topBind) topExpr = do
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
        rUninterps     <- newIORef []
        finalResult    <- runProver (res rUninterps)
        finalUninterps <- readIORef rUninterps
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

                -- If proof failed and there are uninterpreted functions, print a warning:
                let unintFuns = [p | p@(_, t) <- nub $ sortBy (comparing fst) finalUninterps, isJust (splitFunTy_maybe t)]
                unless (success || null unintFuns) $ do
                        let plu | length finalUninterps > 1 = "s:"
                                | True                      = ":"
                            shUI (e, t) = (showSDoc (dflags cfg) (ppr (getSrcSpan e)), sh e, sh t)
                            ls   = map shUI unintFuns
                            len1 = maximum (0 : [length s | (s, _, _) <- ls])
                            len2 = maximum (0 : [length s | (_, s, _) <- ls])
                            pad n s = take n (s ++ repeat ' ')
                            put (a, b, c) = putStrLn $ "  [" ++ pad len1 a ++ "] " ++ pad len2 b ++ " :: " ++ c
                        putStrLn $ "[SBV] Counter-example might be bogus due to uninterpreted constant" ++ plu
                        mapM_ put ls

                return success
          Left success -> return success

  where res uis = do v <- runReaderT (symEval topExpr) Env{ curLoc        = topLoc
                                                          , flags         = dflags        cfg
                                                          , uninterpreted = uis
                                                          , machWordSize  = wordSize      cfg
                                                          , envMap        = knownFuns     cfg
                                                          , baseTCs       = knownTCs      cfg
                                                          , specMap       = knownSpecials cfg
                                                          , coreMap       = allBinds      cfg
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
        symEval :: CoreExpr -> Eval Val
        symEval e = do let (bs, body) = collectBinders e
                       ats <- mapM (\b -> getBaseType (getSrcSpan b) (varType b) >>= \bt -> return (b, bt)) bs
                       let mkVar ((b, k), mbN) = do v <- S.svMkSymVar Nothing k (mbN `mplus` Just (sh b))
                                                    return ((b, k), Base v)
                       sArgs <- mapM (lift . mkVar) (zip ats (concat [map Just ns | Names ns <- opts] ++ repeat Nothing))
                       local (\env -> env{envMap = foldr (uncurry M.insert) (envMap env) sArgs}) (go body)

        isUninterpretedBinding :: Var -> Bool
        isUninterpretedBinding v = any (Uninterpret `elem`) [opt | SBV opt <- sbvAnnotation v]

        go :: CoreExpr -> Eval Val
        go e = tgo (exprType e) e

        -- Main symbolic evaluator:
        tgo :: Type -> CoreExpr -> Eval Val

        -- tgo t e | trace ("--> " ++ show (sh (e, t))) False = undefined

        tgo t (Var v) = do Env{envMap, coreMap, specMap} <- ask
                           k <- getBaseType (getSrcSpan v) t
                           case (v, k) `M.lookup` envMap of
                             Just b  -> return b
                             Nothing -> case v `M.lookup` coreMap of
                                           Just b  -> if isUninterpretedBinding v
                                                      then uninterpret t v
                                                      else go b
                                           Nothing -> case v `M.lookup` specMap of
                                                       Just b  -> return b
                                                       Nothing -> uninterpret t v

        tgo t e@(Lit l) = do Env{machWordSize} <- ask
                             case l of
                               MachChar{}        -> unint
                               MachStr{}         -> unint
                               MachNullAddr      -> unint
                               MachLabel{}       -> unint
                               MachInt      i    -> return $ Base $ S.svInteger (S.KBounded True  machWordSize) i
                               MachInt64    i    -> return $ Base $ S.svInteger (S.KBounded True  64          ) i
                               MachWord     i    -> return $ Base $ S.svInteger (S.KBounded False machWordSize) i
                               MachWord64   i    -> return $ Base $ S.svInteger (S.KBounded False 64          ) i
                               MachFloat    f    -> return $ Base $ S.svFloat   (fromRational f)
                               MachDouble   d    -> return $ Base $ S.svDouble  (fromRational d)
                               LitInteger   i it -> do k <- getBaseType noSrcSpan it
                                                       return $ Base $ S.svInteger k i
                  where unint = do Env{flags} <- ask
                                   k <- getBaseType noSrcSpan t
                                   return $ Base $ S.svUninterpreted k (mkValidName "lit" (showSDoc flags (ppr e))) Nothing []

        tgo tFun (App (App (Var v) (Type t)) (Var dict))
           | isReallyADictionary dict = do Env{envMap} <- ask
                                           k <- getBaseType (getSrcSpan v) t
                                           case (v, k) `M.lookup` envMap of
                                              Just b -> return b
                                              _      -> uninterpret tFun v
        tgo t (App a (Type _))
           = tgo t a

        tgo _ (App f e)
           = do func <- go f
                arg  <- go e
                let ok (S.KUserSort s1 _) (S.KUserSort s2 _) = s1 == s2
                    ok k1                 k2                 = k1 == k2
                case (func, arg) of
                  (Func (k, _) sf, Base sv) | S.kindOf sv `ok` k -> sf sv
                  (_,              Func{})                       -> tbd "Unsupported higher-order application" [sh f, sh e]
                  _                                              -> error $ "[SBV] Impossible happened. Got an application with mismatched types: "
                                                                            ++ sh [(f, func), (e, arg)]

        tgo _ (Lam b body) = do
            k <- getBaseType (getSrcSpan b) (varType b)
            return $ Func (k, Just (sh b)) $ \s -> local (\env -> env{envMap = M.insert (b, k) (Base s) (envMap env)}) (go body)

        tgo _ e@(Let _ _)
           = tbd "Unsupported let-binding" [sh e]

        -- Case expressions. We take advantage of the core-invariant that each case alternative
        -- is exhaustive; and DEFAULT (if present) is the first alternative. We turn it into a
        -- simple if-then-else chain with the last element on the DEFAULT, or whatever comes last.
        tgo _ e@(Case ce _b _t alts)
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

        tgo t (Cast e _)
           = tgo t e

        tgo _ (Tick t e)
           = local (\envMap -> envMap{curLoc = tickSpan t (curLoc envMap)}) $ go e

        tgo _ e@(Type{})
           = tbd "Unsupported type-expression" [sh e]

        tgo _ e@(Coercion{})
           = tbd "Unsupported coercion-expression" [sh e]

-- | Uninterpret an expression
uninterpret :: Type -> Var -> Eval Val
uninterpret origT v = do
          let (args, res) = splitFunTys t
              sp          = getSrcSpan v
          argKs <- mapM (getBaseType sp) args
          resK  <- getBaseType sp res
          Env{flags, uninterpreted} <- ask
          let nm = mkValidName "expr" $ showSDoc flags (ppr v)
          liftIO $ modifyIORef uninterpreted ((v, t) :)
          return $ walk argKs (nm, resK) []
  where t = origT

        walk []     (nm, k) args = Base $ S.svUninterpreted k nm Nothing (reverse args)
        walk (a:as) nmk     args = Func (a, Nothing) $ \p -> return (walk as nmk (p:args))

-- not every name is good, sigh
mkValidName :: String -> String -> String
mkValidName origin nm
  | null nm || nm `elem` S.smtLibReservedNames = mkValidName origin ("sbvPlugin_" ++ origin ++ "_" ++ nm)
  | isAlpha (head nm) && all isGood (tail nm)  = nm
  | True                                       = "|" ++ map tr nm ++ "|"
  where isGood c = isAlphaNum c || c == '_'
        tr '|'   = '_'
        tr '\\'  = '_'
        tr c     = c

-- | Is this variable really a dictionary?
isReallyADictionary :: Var -> Bool
isReallyADictionary v = case classifyPredType (varType v) of
                          ClassPred{} -> True
                          EqPred{}    -> True
                          TuplePred{} -> True
                          IrredPred{} -> False

-- | Convert a Core type to an SBV kind, if known
-- Otherwise, create an uninterpreted kind, and return that.
getBaseType :: SrcSpan -> Type -> Eval S.Kind
getBaseType sp t = do
        Env{baseTCs, flags} <- ask
        let nm = mkValidName "type" $ showSDoc flags (ppr t)
            uninterpreted = S.KUserSort nm $ Left $ "originating from sbvPlugin: " ++ showSDoc flags (ppr sp)
        case grabTCs (splitTyConApp_maybe t) of
          Just k -> case k `M.lookup` baseTCs of
                      Just knd -> return knd
                      Nothing  -> return uninterpreted
          _        -> return uninterpreted
  where -- allow one level of nesting
        grabTCs Nothing          = Nothing
        grabTCs (Just (top, ts)) = do as <- walk ts []
                                      return (top, as)
        walk []     sofar = Just $ reverse sofar
        walk (a:as) sofar = case splitTyConApp_maybe a of
                               Just (ac, []) -> walk as (ac:sofar)
                               _             -> Nothing

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

{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.Plugin.Analyze (analyzeBind) where

import GhcPlugins

import Control.Monad.Reader
import System.Exit hiding (die)

import Data.IORef

import Data.Char     (isAlpha, isAlphaNum, toLower, isUpper)
import Data.List     (intercalate, partition, nub, sort, sortBy, isPrefixOf)
import Data.Maybe    (listToMaybe, fromMaybe)
import Data.Ord      (comparing)

import qualified Data.Map as M

import qualified Data.SBV           as S hiding (proveWith, proveWithAny)
import qualified Data.SBV.Dynamic   as S
import qualified Data.SBV.Internals as S

import qualified Control.Exception as C

import Data.SBV.Plugin.Common
import Data.SBV.Plugin.Data

-- | Dispatch the analyzer over bindings
analyzeBind :: Config -> CoreBind -> CoreM ()
analyzeBind cfg@Config{sbvAnnotation, cfgEnv} = go
  where go (NonRec b e) = bind (b, e)
        go (Rec binds)  = mapM_ bind binds

        bind (b, e) = mapM_ work (sbvAnnotation b)
          where work (SBV opts)
                   | Just s <- hasSkip opts 
                   = liftIO $ putStrLn $ "[SBV] " ++ showSpan cfg (pickSpan [varSpan b]) ++ " Skipping " ++ show (showSDoc (flags cfgEnv) (ppr b)) ++ ": " ++ s
                   | Uninterpret `elem` opts
                   = return ()
                   | True
                   = liftIO $ prove cfg opts b e
                hasSkip opts = listToMaybe [s | Skip s <- opts]

-- | Prove an SBVTheorem
prove :: Config -> [SBVOption] -> Var -> CoreExpr -> IO ()
prove cfg@Config{isGHCi} opts b e = do
        success <- safely $ proveIt cfg opts b e
        unless (success || isGHCi || IgnoreFailure `elem` opts) $ do
            putStrLn $ "[SBV] Failed. (Use option '" ++ show IgnoreFailure ++ "' to continue.)" 
            exitFailure

-- | Safely execute an action, catching the exceptions, printing and returning False if something goes wrong
safely :: IO Bool -> IO Bool
safely a = a `C.catch` bad
  where bad :: C.SomeException -> IO Bool
        bad e = do print e
                   return False

-- | Returns True if proof went thru
proveIt :: Config -> [SBVOption] -> Var -> CoreExpr -> IO Bool
proveIt cfg@Config{cfgEnv, sbvAnnotation} opts topBind topExpr = do
        solverConfigs <- pickSolvers opts
        let verbose = Verbose    `elem` opts
            qCheck  = QuickCheck `elem` opts
            runProver prop
              | qCheck = Left  `fmap` S.svQuickCheck prop
              | True   = Right `fmap` S.proveWithAny [s{S.verbose = verbose} | s <- solverConfigs] prop
            topLoc = varSpan topBind
            loc = "[SBV] " ++ showSpan cfg topLoc
            slvrTag = ", using " ++ tag ++ "."
              where tag = case solverConfigs of
                            []     -> "no solvers"  -- can't really happen
                            [x]    -> show x
                            [x, y] -> show x ++ " and " ++ show y
                            xs     -> intercalate ", " (map show (init xs)) ++ ", and " ++ show (last xs)
        putStrLn $ "\n" ++ loc ++ (if qCheck then " QuickChecking " else " Proving ") ++ show (sh topBind) ++ slvrTag
        (finalResult, finalUninterps) <- do
                        finalResult    <- runProver (res cfgEnv topLoc)
                        finalUninterps <- readIORef (rUninterpreted cfgEnv)
                        return (finalResult, finalUninterps)
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

                -- If proof failed and there are uninterpreted values, print a warning; except for "uninteresting" types
                let unintVals = filter ((`notElem` uninteresting cfgEnv) . snd) $ nub $ sortBy (comparing fst) [vt | (vt, _) <- finalUninterps]
                unless (success || null unintVals) $ do
                        let plu | length finalUninterps > 1 = "s:"
                                | True                      = ":"
                            shUI (e, t) = (showSDoc (flags cfgEnv) (ppr (getSrcSpan e)), sh e, sh t)
                            ls   = map shUI unintVals
                            len1 = maximum (0 : [length s | (s, _, _) <- ls])
                            len2 = maximum (0 : [length s | (_, s, _) <- ls])
                            pad n s = take n (s ++ repeat ' ')
                            put (a, b, c) = putStrLn $ "  [" ++ pad len1 a ++ "] " ++ pad len2 b ++ " :: " ++ c
                        putStrLn $ "[SBV] Counter-example might be bogus due to uninterpreted constant" ++ plu
                        mapM_ put ls

                return success
          Left success -> return success

  where debug = Debug `elem` opts

        res initEnv topLoc = do
               v <- runReaderT (symEval topExpr) initEnv{curLoc = [topLoc]}
               case v of
                 Base r -> return r
                 r      -> error $ "Impossible happened. Final result reduced to a non-base value: " ++ showSDocUnsafe (ppr r)

        die :: [SrcSpan] -> String -> [String] -> a
        die locs w es = error $ concatMap ("\n" ++) $ tag ("Skipping proof. " ++ w ++ ":") : map tab es
          where marker = "[SBV] " ++ showSpan cfg (pickSpan locs)
                tag s = marker ++ " " ++ s
                tab s = replicate (length marker) ' ' ++  "    " ++ s

        tbd :: String -> [String] -> Eval Val
        tbd w ws = do Env{curLoc} <- ask
                      die curLoc w ws

        sh o = showSDoc (flags cfgEnv) (ppr o)

        -- Given an alleged theorem, first establish it has the right type, and
        -- then go ahead and evaluate it symbolicly after applying it to sufficient
        -- number of symbolic arguments
        symEval :: CoreExpr -> Eval Val
        symEval e = do let (bs, body) = collectBinders (pushLetLambda e)
                       Env{curLoc} <- ask
                       let mbListSize = listToMaybe [n | ListSize n <- opts]
                       bodyType <- getType (pickSpan curLoc) (exprType body)

                       -- Figure out if there were some unmentioned variables; happens if the top
                       -- level wasn't fully saturated.
                       let (extraArgs, finalType) = walk bodyType []
                                where walk (KFun d c) sofar = walk c (d:sofar)
                                      walk k          sofar = (reverse sofar, k)

                       case finalType of
                         KBase S.KBool -> do -- First collect the named arguments:
                                             argKs <- mapM (\b -> getType (getSrcSpan b) (varType b) >>= \bt -> return (b, bt)) bs
                                             let mkVar ((b, k), mbN) = do sv <- mkSym mbListSize (varSpan b) (Just (idType b)) k (mbN `mplus` Just (sh b))
                                                                          return ((b, k), sv)
                                             bArgs <- mapM (lift . mkVar) (zip argKs (concat [map Just ns | Names ns <- opts] ++ repeat Nothing))

                                             -- Go ahead and run the body symbolically; on bArgs
                                             bRes <- local (\env -> env{envMap = foldr (uncurry M.insert) (envMap env) bArgs}) (go body)

                                             -- If there are extraArgs; then create symbolics and apply to the result:
                                             let feed []     sres       = return sres
                                                 feed (k:ks) (Func _ f) = do sv <- lift $ mkSym mbListSize (pickSpan curLoc) Nothing k Nothing
                                                                             f sv >>= feed ks
                                                 feed ks     v          = error $ "Impossible! Left with extra args to apply on a non-function: " ++ sh (ks, v)

                                             feed extraArgs bRes

                         _             -> die curLoc "Non-boolean property declaration" (concat [ ["Found    : " ++ sh (exprType e)]
                                                                                                , ["Returning: " ++ sh (exprType body) | not (null bs)]
                                                                                                , ["Expected : Bool" ++ if null bs then "" else " result"]
                                                                                                ])
          where -- Sometimes the core has a wrapper let, floated out on top. Float that in.
                pushLetLambda (Let b (Lam x bd)) = Lam x (pushLetLambda (Let b bd))
                pushLetLambda o                  = o

                -- Create a symbolic variable:
                mkSym :: Maybe Int -> SrcSpan -> Maybe Type -> SKind -> Maybe String -> S.Symbolic Val
                mkSym mbLs curLoc mbBType = sym
                 where tinfo k = case mbBType of
                                   Nothing -> "Kind: " ++ sh k
                                   Just t  -> "Type: " ++ sh t

                       sym (KBase k) nm  = do v <- S.svMkSymVar Nothing k nm
                                              return (Base v)

                       sym (KTup ks) nm = do let ns = map (\i -> (++ ("_" ++ show i)) `fmap` nm) [1 .. length ks]
                                             vs <- zipWithM sym ks ns
                                             return $ Tup vs

                       sym (KLst ks) nm = do let ls  = fromMaybe bad mbLs
                                                 bad = die [curLoc] "List-argument found, with no size info"
                                                                    [ "Name: " ++ fromMaybe "anonymous" nm
                                                                    , tinfo (KLst ks)
                                                                    , "Hint: Use the \"ListSize\" annotation"
                                                                    ]
                                                 ns = map (\i -> (++ ("_" ++ show i)) `fmap` nm) [1 .. ls]
                                             vs <- zipWithM sym (replicate ls ks) ns
                                             return (Lst vs)

                       sym k@KFun{}  nm = die [curLoc] "Unsupported higher-order symbolic input"
                                                       [ "Name: " ++ fromMaybe "<anonymous>" nm
                                                       , tinfo k
                                                       ]

        isUninterpretedBinding :: Var -> Bool
        isUninterpretedBinding v = any (Uninterpret `elem`) [opt | SBV opt <- sbvAnnotation v]

        go :: CoreExpr -> Eval Val
        go (Tick t e) = local (\envMap -> envMap{curLoc = tickSpan t : curLoc envMap}) $ go e
        go e          = tgo (exprType e) e

        debugTrace s w
          | debug = trace ("--> " ++ s) w
          | True  = w

        -- Main symbolic evaluator:
        tgo :: Type -> CoreExpr -> Eval Val

        tgo t e | debugTrace (sh (e, t)) False = undefined

        tgo t (Var v) = do Env{envMap, coreMap} <- ask
                           k <- getType (getSrcSpan v) t
                           case (v, k) `M.lookup` envMap of
                             Just b  -> return b
                             Nothing -> case v `M.lookup` coreMap of
                                           Just (l, b)  -> if isUninterpretedBinding v
                                                           then uninterpret t v
                                                           else local (\env -> env{curLoc = l : curLoc env}) $ go b
                                           Nothing      -> debugTrace ("Uninterpreting: " ++ sh (v, k, nub $ sort $ map (fst . fst) (M.toList envMap)))
                                                                      $ uninterpret t v

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
                               LitInteger   i it -> do k <- getType noSrcSpan it
                                                       case k of
                                                         KBase b -> return $ Base $ S.svInteger b i
                                                         _       -> error $ "Impossible: The type for literal resulted in non base kind: " ++ sh (e, k)
                  where unint = do Env{flags} <- ask
                                   k  <- getType noSrcSpan t
                                   nm <- mkValidName (showSDoc flags (ppr e))
                                   case k of
                                     KBase b -> return $ Base $ S.svUninterpreted b nm Nothing []
                                     _       -> error $ "Impossible: The type for literal resulted in non base kind: " ++ sh (e, k)

        tgo tFun orig@App{} = do
             reduced <- betaReduce orig

             Env{specials} <- ask

             -- handle specials: Equality, tuples, and lists
             let getVar (Var v)    = Just v
                 getVar (Tick _ e) = getVar e
                 getVar _          = Nothing

                 isEq (App (App ev (Type _)) dict) | Just v <- getVar ev, isReallyADictionary dict, Just f <- isEquality specials v = Just f
                 isEq _                                                                                                             = Nothing

                 isTup (Var v)          = isTuple specials v
                 isTup (App f (Type _)) = isTup f
                 isTup (Tick _ e)       = isTup e
                 isTup _                = Nothing

                 isLst (Var v)          = isList specials v
                 isLst (App f (Type _)) = isLst f
                 isLst (Tick _ e)       = isLst e
                 isLst _                = Nothing

                 isSpecial e = isEq e `mplus` isTup e `mplus` isLst e

             case isSpecial reduced of
               Just f  -> debugTrace ("Special located: " ++ sh (orig, f)) $ return f
               Nothing -> case reduced of
                            App (App (Var v) (Type t)) dict | isReallyADictionary dict -> do
                                Env{envMap} <- ask
                                k <- getType (getSrcSpan v) t
                                case (v, k) `M.lookup` envMap of
                                  Just b  -> return b
                                  Nothing -> do Env{coreMap} <- ask
                                                case v `M.lookup` coreMap of
                                                  Just (l, e) -> local (\env -> env{curLoc = l : curLoc env}) $ tgo tFun (App (App e (Type t)) dict)
                                                  Nothing     -> tgo tFun (Var v)

                            App (Var v) (Type t) -> do
                                Env{coreMap} <- ask
                                case v `M.lookup` coreMap of
                                  Just (l, e) -> local (\env -> env{curLoc = l : curLoc env}) $ tgo tFun (App e (Type t))
                                  Nothing     -> tgo tFun (Var v)

                            App (Let (Rec bs) f) a -> go (Let (Rec bs) (App f a))

                            App f e  -> do
                                func <- go f
                                arg  <- go e
                                case (func, arg) of
                                   (Func _ sf, sv) -> sf sv
                                   _               -> error $ "[SBV] Impossible happened. Got an application with mismatched types: " ++ sh [(f, func), (e, arg)]

                            e   -> go e

        tgo _ (Lam b body) = do
                k <- getType (getSrcSpan b) (varType b)
                Env{envMap} <- ask
                return $ Func (Just (sh b)) $ \s -> local (\env -> env{envMap = M.insert (b, k) s envMap}) (go body)

        tgo _ (Let (NonRec b e) body) = local (\env -> env{coreMap = M.insert b (varSpan b, e) (coreMap env)}) (go body)

        tgo _ (Let (Rec bs) body) = local (\env -> env{coreMap = foldr (\(b, e) m -> M.insert b (varSpan b, e) m) (coreMap env) bs}) (go body)

        -- Case expressions. We take advantage of the core-invariant that each case alternative
        -- is exhaustive; and DEFAULT (if present) is the first alternative. We turn it into a
        -- simple if-then-else chain with the last element on the DEFAULT, or whatever comes last.
        tgo _ e@(Case ce caseBinder caseType alts)
           = do sce <- go ce
                let caseTooComplicated l w [] = die l ("Unsupported case-expression (" ++ w ++ ")") [sh e]
                    caseTooComplicated l w xs = die l ("Unsupported case-expression (" ++ w ++ ")") $ [sh e, "While Analyzing:"] ++ xs
                    isDefault (DEFAULT, _, _) = True
                    isDefault _               = False
                    (defs, nonDefs)           = partition isDefault alts
                    walk ((p, bs, rhs) : rest) =
                         do -- try to get a "good" location for this alternative, if possible:
                            let eLoc = case (rhs, bs) of
                                        (Tick t _, _  ) -> tickSpan t
                                        (Var v,    _  ) -> varSpan v
                                        (_,        b:_) -> varSpan b
                                        _               -> varSpan caseBinder
                            mr <- match eLoc sce p bs
                            case mr of
                              Just (m, bs') -> do let result = local (\env -> env{curLoc = eLoc : curLoc env, envMap = foldr (uncurry M.insert) (envMap env) bs'}) $ go rhs
                                                  if null rest
                                                     then result
                                                     else do Env{curLoc} <- ask
                                                             choose (caseTooComplicated (eLoc : curLoc) "with-complicated-alternatives-during-merging") m result (walk rest)
                              Nothing -> do Env{curLoc} <- ask
                                            caseTooComplicated (eLoc : curLoc) "with-complicated-match" ["MATCH " ++ sh (ce, p), " --> " ++ sh rhs]
                    walk []                   = do Env{curLoc} <- ask
                                                   caseTooComplicated curLoc "with-non-exhaustive-match" []  -- can't really happen
                k <- getType (getSrcSpan caseBinder) caseType
                local (\env -> env{envMap = M.insert (caseBinder, k) sce (envMap env)}) $ walk (nonDefs ++ defs)
           where choose bailOut t tb fb = case S.svAsBool t of
                                            Nothing    -> do stb <- tb
                                                             sfb <- fb
                                                             return $ iteVal bailOut t stb sfb
                                            Just True  -> tb
                                            Just False -> fb
                 match :: SrcSpan -> Val -> AltCon -> [Var] -> Eval (Maybe (S.SVal, [((Var, SKind), Val)]))
                 match sp a c bs = case c of
                                     DEFAULT    -> return $ Just (S.svTrue, [])
                                     LitAlt  l  -> do b <- go (Lit l)
                                                      return $ Just (a `eqVal` b, [])
                                     DataAlt dc -> do Env{envMap, destMap} <- ask
                                                      k <- getType sp (dataConRepType dc)
                                                      let wid = dataConWorkId dc
                                                      -- The following lookup in env essentially gets True/False constructors (or other base-values if we add them)
                                                      case (wid, k) `M.lookup` envMap of
                                                        Just (Base b) -> return $ Just (a `eqVal` Base b, [])
                                                        _             -> case wid `M.lookup` destMap of
                                                                           Nothing -> return Nothing
                                                                           Just f  -> do bts <- mapM (\b -> getType (getSrcSpan b) (varType b) >>= \bt -> return (b, bt)) bs
                                                                                         return $ Just (f a bts)

        tgo t (Cast e c)
           = debugTrace ("Going thru a Cast: " ++ sh c) $ tgo t e

        tgo _ (Tick t e) = local (\envMap -> envMap{curLoc = tickSpan t : curLoc envMap}) $ go e

        tgo _ (Type t)
           = do Env{curLoc} <- ask
                k <- getType (pickSpan curLoc) t
                return (Typ k)

        tgo _ e@Coercion{}
           = tbd "Unsupported coercion-expression" [sh e]

        isBetaReducable (Type _) = True
        isBetaReducable e        = isReallyADictionary e

        betaReduce :: CoreExpr -> Eval CoreExpr
        betaReduce orig@(App f a) = do
                rf <- betaReduce f
                if not (isBetaReducable a)
                   then return (App rf a)
                   else do let chaseVars :: CoreExpr -> Eval CoreExpr
                               chaseVars (Var x)    = do Env{coreMap} <- ask
                                                         case x `M.lookup` coreMap of
                                                           Nothing     -> return (Var x)
                                                           Just (_, b) -> chaseVars b
                               chaseVars (Tick _ x) = chaseVars x
                               chaseVars x          = return x
                           func <- chaseVars rf
                           case func of
                             Lam x b -> do reduced <- betaReduce $ substExpr (ppr "SBV.betaReduce") (extendSubstList emptySubst [(x, a)]) b
                                           () <- debugTrace ("Beta reduce:\n" ++ sh (orig, reduced)) $ return ()
                                           return reduced
                             _       -> return (App rf a)
        betaReduce e = return e

-- | Is this really a dictionary in disguise? This is a terrible hack, and the ice is thin here. But it seems to work.
-- TODO: Figure out if there's a better way of doing this. Note that this function really does get applications, when
-- those dictionaries are parameterized by others. Think of the case where "Eq [a]" dictionary depends on "Eq a", for
-- instance. In these cases, GHC to produces applications.
isReallyADictionary :: CoreExpr -> Bool
isReallyADictionary (App f _) = isReallyADictionary f
isReallyADictionary (Var v)   = "$" `isPrefixOf` unpackFS (occNameFS (occName (varName v)))
isReallyADictionary _         = False

-- | Uninterpret an expression
uninterpret :: Type -> Var -> Eval Val
uninterpret t v = do
          Env{rUninterpreted, flags} <- ask
          prevUninterpreted <- liftIO $ readIORef rUninterpreted
          case (v, t) `lookup` prevUninterpreted of
             Just (_, val) -> return val
             Nothing       -> do let (tvs,  t')  = splitForAllTys t
                                     (args, res) = splitFunTys t'
                                     sp          = getSrcSpan v
                                 argKs <- mapM (getType sp) args
                                 resK  <- getType sp res
                                 nm <- mkValidName $ showSDoc flags (ppr v)
                                 let fVal = wrap tvs $ walk argKs (nm, resK) []
                                 liftIO $ modifyIORef rUninterpreted (((v, t), (nm, fVal)) :)
                                 return fVal
  where walk :: [SKind] -> (String, SKind) -> [S.SVal] -> Val
        walk []     (nm, k) args = case k of
                                     KBase b -> Base $ S.svUninterpreted b nm Nothing (reverse args)
                                     _       -> error $ "Not yet supported uninterpreted type with non-base type: " ++ showSDocUnsafe (ppr k)
        walk (_:as) nmk     args = Func Nothing $ \a -> case a of
                                                          Base p -> return (walk as nmk (p:args))
                                                          _      -> return (walk as nmk args)
        wrap []     f = f
        wrap (_:ts) f = Func Nothing $ \(Typ _) -> return (wrap ts f)

-- not every name is good, sigh
mkValidName :: String -> Eval String
mkValidName name =
        do Env{rUsedNames} <- ask
           usedNames <- liftIO $ readIORef rUsedNames
           let unm = unSMT $ genSym usedNames name
           liftIO $ modifyIORef rUsedNames (unm :)
           return $ escape unm
  where genSym bad nm
          | nm `elem` bad = head [nm' | i <- [(0::Int) ..], let nm' = nm ++ "_" ++ show i, nm' `notElem` bad]
          | True          = nm
        unSMT nm
          | map toLower nm `elem` S.smtLibReservedNames
          = if not (null nm) && isUpper (head nm)
            then "sbv"  ++ nm
            else "sbv_" ++ nm
          | True
          = nm
        escape nm | isAlpha (head nm) && all isGood (tail nm) = nm
                  | True                                      = "|" ++ map tr nm ++ "|"
        isGood c = isAlphaNum c || c == '_'
        tr '|'   = '_'
        tr '\\'  = '_'
        tr c     = c

-- | Convert a Core type to an SBV Type, retaining functions and tuples
getType :: SrcSpan -> Type -> Eval SKind
getType sp typ = do let (tvs, typ') = splitForAllTys typ
                        (args, res) = splitFunTys typ'
                    argKs <- mapM (getType sp) args
                    resK  <- getComposite res
                    return $ wrap tvs $ foldr KFun resK argKs
 where wrap ts f    = foldr (KFun . mkUserSort) f ts
       mkUserSort v = KBase (S.KUserSort (show (occNameFS (occName (varName v)))) (Left "sbvPlugin"))

       -- | Extract tuples, lists, or base kinds
       getComposite :: Type -> Eval SKind
       getComposite t = case splitTyConApp_maybe t of
                          Just (k, ts)  | isTupleTyCon k -> KTup  `fmap` mapM (getType sp) ts
                          Just (k, [a]) | listTyCon == k -> KLst  `fmap` getType sp a
                          _                              -> KBase `fmap` getBaseType t

       -- | Convert a Core type to an SBV kind, if known
       -- Otherwise, create an uninterpreted kind, and return that.
       getBaseType :: Type -> Eval S.Kind
       getBaseType bt = do
               Env{tcMap} <- ask
               case grabTCs (splitTyConApp_maybe bt) of
                 Just k -> case k `M.lookup` tcMap of
                             Just knd -> return knd
                             Nothing  -> unknown
                 _        -> unknown
         where -- allow one level of nesting, essentially to support Haskell's 'Ratio Integer' to map to 'SReal'
               grabTCs Nothing          = Nothing
               grabTCs (Just (top, ts)) = do as <- walk ts []
                                             return (top, as)
               walk []     sofar = Just $ reverse sofar
               walk (a:as) sofar = case splitTyConApp_maybe a of
                                      Just (ac, []) -> walk as (ac:sofar)
                                      _             -> Nothing
               -- Check if we uninterpreted this before; if so, return it, otherwise create a new one
               unknown = do Env{flags, rUITypes} <- ask
                            uiTypes <- liftIO $ readIORef rUITypes
                            case bt `lookup` uiTypes of
                              Just k  -> return k
                              Nothing -> do nm <- mkValidName $ showSDoc flags (ppr bt)
                                            let k = S.KUserSort nm $ Left $ "originating from sbvPlugin: " ++ showSDoc flags (ppr sp)
                                            liftIO $ modifyIORef rUITypes ((bt, k) :)
                                            return k

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

{-# LANGUAGE NamedFieldPuns  #-}
module Data.SBV.Plugin.Analyze (analyzeBind) where

import GhcPlugins

import Control.Monad.Reader
import System.Exit hiding (die)

import Data.List  (intercalate)
import qualified Data.Map as M

import qualified Data.SBV         as S hiding (proveWith, proveWithAny)
import qualified Data.SBV.Dynamic as S

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
prove cfg@Config{isGHCi} opts b topLoc e
  | isProvable (exprType e) = do success <- safely $ proveIt cfg opts (topLoc, b) e
                                 unless (isGHCi || success) $
                                        if WarnIfFails `elem` opts
                                           then    putStrLn "[SBV] Failed. Continuing due to the 'WarnIfFails' flag."
                                           else do putStrLn "[SBV] Failed. (Use option 'WarnIfFails' to continue.)"
                                                   exitFailure
  | True                    = error $ "SBV: " ++ showSpan cfg b topLoc ++ " does not have a provable type!"

-- | Is this a provable type?
-- TODO: Currently we always say yes!
isProvable :: Type -> Bool
isProvable _ = True

-- | Safely execute an action, catching the exceptions, printing and returning False if something goes wrong
safely :: IO Bool -> IO Bool
safely a = a `C.catch` bad
  where bad :: C.SomeException -> IO Bool
        bad e = do print e
                   return False

-- | Interpreter environment
data Env = Env { curLoc  :: SrcSpan
               , baseTCs :: M.Map TyCon         S.Kind
               , envMap  :: M.Map (Var, S.Kind) Val
               , specMap :: M.Map Var           Val
               }

-- | The interpreter monad
type Eval a = ReaderT Env S.Symbolic a

-- Returns True if proof went thru
proveIt :: Config -> [SBVOption] -> (SrcSpan, Var) -> CoreExpr -> IO Bool
proveIt cfg@Config{isGHCi} opts (topLoc, topBind) topExpr = do
        solverConfigs <- pickSolvers opts
        let verbose = Debug `elem` opts
            runProver = S.proveWithAny [s{S.verbose = verbose} | s <- solverConfigs]
            loc = "[SBV] " ++ showSpan cfg topBind topLoc
            slvrTag | isGHCi && not verbose = ".. "
                    | True                  = ", using " ++ tag ++ "."
                    where tag = case solverConfigs of
                                  []     -> "no solvers"  -- can't really happen
                                  [x]    -> show x
                                  [x, y] -> show x ++ " and " ++ show y
                                  xs     -> intercalate ", " (map show (init xs)) ++ ", and " ++ show (last xs)
        putStr $ "\n" ++ loc ++ " Proving " ++ show (sh topBind) ++ slvrTag
        unless isGHCi $ putStrLn ""
        (solver, sres@(S.ThmResult smtRes)) <- runProver res
        let success = case smtRes of
                        S.Unsatisfiable{} -> True
                        S.Satisfiable{}   -> False
                        S.Unknown{}       -> False   -- conservative
                        S.ProofError{}    -> False   -- conservative
                        S.TimeOut{}       -> False   -- conservative
        putStr $ "[" ++ show solver ++ "] "
        print sres
        return success
  where res :: S.Symbolic S.SVal
        res = do v <- runReaderT (go topExpr)
                                 Env{ curLoc  = topLoc
                                    , envMap  = knownFuns     cfg
                                    , baseTCs = knownTCs      cfg
                                    , specMap = knownSpecials cfg
                                    }
                 case v of
                   Base r -> return r
                   Func _ -> die topLoc "Expression too complicated for SBV" [sh topExpr]

        die :: SrcSpan -> String -> [String] -> a
        die loc w es = error $ concatMap ("\n" ++) $ tag ("Skipping proof. " ++ w ++ ":") : map tab es
          where marker = "[SBV] " ++ showSpan cfg topBind loc
                tag s = marker ++ " " ++ s
                tab s = replicate (length marker) ' ' ++  "    " ++ s

        tbd :: String -> [String] -> Eval Val
        tbd w ws = do Env{curLoc} <- ask
                      die curLoc w ws

        sh o = showSDoc (dflags cfg) (ppr o)

        go :: CoreExpr -> ReaderT Env S.Symbolic Val

        -- go e | trace ("--> " ++ show (sh e)) False = undefined

        go e@(Var v) = do mbF <- findSymbolic e
                          case mbF of
                            Just sv -> return sv
                            Nothing -> tbd "Not-yet-supported expression" [sh e ++ " :: " ++ sh (varType v)]

        go (Lit (LitInteger i t))
           = do mbK <- getBaseType t
                case mbK of
                  Nothing -> tbd "Expression has a constant with a complicated type" [show i ++ " :: " ++ sh t]
                  Just k  -> return $ Base $ S.svInteger k i

        go (Lit (MachFloat i))
           = return $ Base $ S.svFloat (fromRational i)

        go (Lit (MachDouble i))
           = return $ Base $ S.svDouble (fromRational i)

        go e@(Lit _)
           = tbd "Unsupported literal" [sh e]

        go (App a (Type _))
           = go a

        go (App f e)
           = do fv <- do mbSF <- findSymbolic f
                         case mbSF of
                           Nothing -> go f
                           Just sf -> return sf
                ev <- go e
                case fv of
                  Base _  -> tbd "Unsupported application" [sh f, sh e]   -- shouldn't happen
                  Func sf -> return $ sf ev

        -- NB: We do *not* have to worry about shadowing when we enter the body
        -- of a lambda, as Core variables are guaranteed unique
        go e@(Lam b body) = do
            let t = varType b
            mbK <- getBaseType t
            case mbK of
              Nothing -> tbd "Abstraction with a non-basic binder" [sh e, sh t]
              Just k  -> do s <- lift $ S.svMkSymVar Nothing k (Just (sh b))
                            local (\env -> env{envMap = M.insert (b, k) (Base s) (envMap env)}) $ go body

        go e@(Let _ _)
           = tbd "Unsupported let-binding" [sh e]

        go e@(Case{})
           = tbd "Unsupported case-expression" [sh e]

        go e@(Cast{})
           = tbd "Unsupported cast-expression" [sh e]

        go (Tick t e)
           = local (\envMap -> envMap{curLoc = tickSpan t (curLoc envMap)}) $ go e

        go e@(Type{})
           = tbd "Unsupported type-expression" [sh e]

        go e@(Coercion{})
           = tbd "Unsupported coercion-expression" [sh e]

-- | Return, if known, whatever symbolic function we can for an expression
findSymbolic :: CoreExpr -> Eval (Maybe Val)
findSymbolic e = do mb <- getSymFun e
                    case mb of
                      Just _  -> return mb
                      Nothing -> getSpecialFun e
  where getSymFun :: CoreExpr -> Eval (Maybe Val)
        getSymFun (Var v) = do Env{envMap} <- ask
                               let t = exprType e
                               mbK <- getBaseType t
                               case mbK of
                                 Nothing -> return Nothing
                                 Just k  -> return $ (v, k) `M.lookup` envMap
        getSymFun (App (App (Var v) (Type t)) (Var dict))
          | isReallyADictionary dict = do Env{envMap} <- ask
                                          mbK <- getBaseType t
                                          case mbK of
                                            Nothing -> return Nothing
                                            Just k  -> return $ (v, k) `M.lookup` envMap
        getSymFun _ = return Nothing

        getSpecialFun :: CoreExpr -> Eval (Maybe Val)
        getSpecialFun (Var v) = do Env{specMap} <- ask
                                   return $ v `M.lookup` specMap
        getSpecialFun _       = return Nothing

        isReallyADictionary :: Var -> Bool
        isReallyADictionary v = case classifyPredType (varType v) of
                                  ClassPred{} -> True
                                  EqPred{}    -> True
                                  TuplePred{} -> True
                                  IrredPred{} -> False

-- | Convert a Core type to an SBV kind, if known
getBaseType :: Type -> Eval (Maybe S.Kind)
getBaseType t = do Env{baseTCs} <- ask
                   case splitTyConApp_maybe t of
                     Just (tc, []) -> return $ tc `M.lookup` baseTCs
                     _             -> return Nothing

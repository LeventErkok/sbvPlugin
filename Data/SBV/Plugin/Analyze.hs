{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.Plugin.Analyze (prove) where

import GhcPlugins

import Control.Monad.Reader
import Data.Maybe

import qualified Data.Map as M

import qualified Data.SBV         as S hiding (proveWith, proveWithAny)
import qualified Data.SBV.Dynamic as S

import qualified Control.Exception as C

import System.Exit hiding (die)

import Data.SBV.Plugin.Data


-- | Prove an SBVTheorem
prove :: Config -> Var -> SrcSpan -> CoreExpr -> IO ()
prove cfg@Config{opts} b topLoc e
  | isProvable (exprType e) = do success <- safely $ checkTheorem cfg (topLoc, b) e
                                 unless success $ if WarnIfFails `elem` opts
                                                  then    putStrLn "[SBV] Failed. Continuing due to the 'WarnIfFails' flag."
                                                  else do putStrLn "[SBV] Failed. (Use option 'WarnIfFails' to continue.)"
                                                          exitFailure
  | True                    = error $ "SBV: " ++ showSpan cfg b topLoc ++ " does not have a provable type!"

-- | Is this a provable type?
-- TODO: Currently we always say yes!
isProvable :: Type -> Bool
isProvable _ = True

safely :: IO Bool -> IO Bool
safely a = a `C.catch` bad
  where bad :: C.SomeException -> IO Bool
        bad e = do print e
                   return False

data Env = Env { curLoc  :: SrcSpan
               , baseTCs :: M.Map TyCon S.Kind
               , envMap  :: M.Map (Var, S.Kind) Val
               }

type Eval a = ReaderT Env S.Symbolic a

proveIt :: [SBVAttribute] -> S.Symbolic S.SVal -> IO (S.Solver, S.ThmResult)
proveIt opts thm = case chosen of
                     []  -> do r <- S.proveWith S.defaultSMTCfg{S.verbose = verbose} thm
                               return (S.name (S.solver S.defaultSMTCfg), r)
                     [s] -> do r <- S.proveWith s thm
                               return (S.name (S.solver s), r)
                     _   -> S.proveWithAny chosen thm
  where verbose = Debug `elem` opts
        chosen
          | AnySolver `elem` opts = map snd solvers
          | True                  = mapMaybe (`lookup` solvers) opts
        solvers = [ (n, s{S.verbose = verbose})
                  | (n, s) <- [ (Z3,        S.z3)
                              , (Yices,     S.yices)
                              , (Boolector, S.boolector)
                              , (CVC4,      S.cvc4)
                              , (MathSAT,   S.mathSAT)
                              , (ABC,       S.abc)
                              ]
                  ]

-- Returns True if proof went thru
checkTheorem :: Config -> (SrcSpan, Var) -> CoreExpr -> IO Bool
checkTheorem cfg@Config{opts} (topLoc, topBind) topExpr = do
        let loc = "[SBV] " ++ showSpan cfg topBind topLoc
            verbose = Debug `elem` opts
        putStr $ "\n" ++ loc ++ " Proving " ++ show (sh topBind) ++ ".. "
        when verbose $ putStrLn ""
        (solver, sres@(S.ThmResult smtRes)) <- proveIt opts res
        unless verbose $ putStr $ "[" ++ show solver ++ "] "
        print sres
        return $ case smtRes of
                   S.Unsatisfiable{} -> True
                   S.Satisfiable{}   -> False
                   S.Unknown{}       -> False   -- conservative
                   S.ProofError{}    -> False   -- conservative
                   S.TimeOut{}       -> False   -- conservative

  where res :: S.Symbolic S.SVal
        res = do v <- runReaderT (go topExpr) Env{curLoc = topLoc, envMap = knownFuns cfg, baseTCs = knownTCs cfg}
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
        go e@(Var v) = do Env{envMap} <- ask
                          let t = exprType e
                          mbK <- getBaseType t
                          case mbK of
                            Nothing -> tbd "Expression refers to non-local variable with complicated type" [sh e ++ " :: " ++ sh t]
                            Just k  -> case (v, k) `M.lookup` envMap of
                                          Just s  -> return s
                                          Nothing -> tbd "Expression refers to non-local variable" [sh e ++ " :: " ++ sh t]

        go e@(Lit _)
           = tbd "Unsupported literal" [sh e]

        go (App a (Type _))
           = go a

        go (App f e)
           = do fv <- do mbSF <- getSymFun f
                         case mbSF of
                           Nothing -> go f
                           Just sf -> return sf
                ev <- go e
                case fv of
                  Base _  -> tbd "Unsupported application" [sh f, sh e]
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

getSymFun :: CoreExpr -> Eval (Maybe Val)
getSymFun (App (App (Var v) (Type t)) (Var dict))
  | isReallyADictionary dict = do Env{envMap} <- ask
                                  mbK <- getBaseType t
                                  case mbK of
                                    Nothing -> return Nothing
                                    Just k  -> return $ (v, k) `M.lookup` envMap
getSymFun _ = return Nothing

isReallyADictionary :: Var -> Bool
isReallyADictionary v = case classifyPredType (varType v) of
                          ClassPred{} -> True
                          EqPred{}    -> True
                          TuplePred{} -> True
                          IrredPred{} -> False

getBaseType :: Type -> Eval (Maybe S.Kind)
getBaseType t = do Env{baseTCs} <- ask
                   case splitTyConApp_maybe t of
                     Just (tc, []) -> return $ tc `M.lookup` baseTCs
                     _             -> return Nothing

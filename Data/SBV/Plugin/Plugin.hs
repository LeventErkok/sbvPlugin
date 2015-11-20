{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.SBV.Plugin.Plugin(plugin) where

import GhcPlugins
import System.Exit

import qualified Data.Map as M

import qualified Language.Haskell.TH as TH

import Data.Int
import Data.Word

import qualified Data.SBV.Internals as S
import qualified Data.SBV.Dynamic   as S

import Data.SBV.Plugin.Data
import Data.SBV.Plugin.Analyze (prove)

-- | Entry point to the plugin
plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install}
 where install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
       install []   todos = reinitializeGlobals >> return (sbvPass : todos)
       install opts _     = do liftIO $ putStrLn $ "[SBV] Unexpected command line options: " ++ show opts
                               liftIO exitFailure

       sbvPass = CoreDoPluginPass "SBV based analysis" pass

       pass :: ModGuts -> CoreM ModGuts
       pass guts@(ModGuts {mg_binds}) = do

          df <- getDynFlags

          let grabTyCon (k, x) = do Just fn <- thNameToGhcName x
                                    tc <- lookupTyCon fn
                                    return (tc, k)

          baseTCs <- M.fromList `fmap` mapM grabTyCon [ (S.KBool,             ''Bool)
                                                      , (S.KUnbounded,        ''Integer)
                                                      , (S.KFloat,            ''Float)
                                                      , (S.KDouble,           ''Double)
                                                      , (S.KBounded True   8, ''Int8)
                                                      , (S.KBounded True  16, ''Int16)
                                                      , (S.KBounded True  32, ''Int32)
                                                      , (S.KBounded True  64, ''Int64)
                                                      , (S.KBounded False  8, ''Word8)
                                                      , (S.KBounded False 16, ''Word16)
                                                      , (S.KBounded False 32, ''Word32)
                                                      , (S.KBounded False 64, ''Word64)
                                                      ]

          let grabVar (n, k, sfn) = do Just fn <- thNameToGhcName n
                                       f <- lookupId fn
                                       return ((f, k), sfn)

          baseEnv <- M.fromList `fmap` mapM grabVar binOps

          anns <- getAnnotations deserializeWithData guts

          let cfg = Config { dflags        = df
                           , opts          = []
                           , knownTCs      = baseTCs
                           , knownFuns     = baseEnv
                           , sbvAnnotation = lookupWithDefaultUFM anns [] . varUnique
                           }

          mapM_ (analyzeBind cfg) mg_binds

          return guts

-- | Dispatch the analyzer recursively over subexpressions.
analyzeBind :: Config -> CoreBind -> CoreM ()
analyzeBind cfg@Config{sbvAnnotation} = go
  where go (NonRec b e) = bind (b, e)
        go (Rec binds)  = mapM_ bind binds
        bind (b, e) = mapM_ work (sbvAnnotation b)
          where work (SBVTheorem opts) = liftIO $ prove cfg opts b (bindSpan b) e
                work (SBVSafe{})       = return ()
                work SBVUninterpret    = return ()

binOps :: [(TH.Name, S.Kind, Val)]
binOps =  -- equality is for all kinds
          [('(==), k, lift2 S.svEqual) | k <- allKinds]

          -- arithmetic
       ++ [(op,    k, lift2 sOp)       | k <- arithKinds, (op, sOp) <- arithOps]

          -- comparisons
       ++ [(op,    k, lift2 sOp)       | k <- arithKinds, (op, sOp) <- compOps ]
 where
       -- Bit-vectors
       bvKinds    = [S.KBounded s sz | s <- [False, True], sz <- [8, 16, 32, 64]]

       -- Arithmetic kinds
       arithKinds = [S.KUnbounded, S.KFloat, S.KDouble] ++ bvKinds

       -- Everything
       allKinds   = S.KBool : arithKinds

       -- Binary arithmetic UOPs
       arithOps   = [ ('(+), S.svPlus)
                    , ('(-), S.svMinus)
                    , ('(*), S.svTimes)
                    ]

       -- Comparisons
       compOps    = [ ('(<),  S.svLessThan)
                    , ('(>),  S.svGreaterThan)
                    , ('(<=), S.svLessEq)
                    , ('(>=), S.svGreaterEq)
                    ]

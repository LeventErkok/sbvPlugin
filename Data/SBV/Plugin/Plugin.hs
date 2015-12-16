---------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Analyze
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Main entry point to the SBV Plugin
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

module Data.SBV.Plugin.Plugin(plugin) where

import GhcPlugins
import System.Exit

import Data.Maybe (fromJust)
import Data.List  (sortBy)
import Data.Ord   (comparing)
import Data.Bits  (bitSizeMaybe)

import Data.IORef

import qualified Data.Map as M

import Data.SBV.Plugin.Common
import Data.SBV.Plugin.Env
import Data.SBV.Plugin.Analyze (analyzeBind)

-- | Entry point to the plugin
plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install}
 where install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
       install []          todos = reinitializeGlobals >> return (sbvPass : todos)
       install ["runLast"] todos = reinitializeGlobals >> return (todos ++ [sbvPass])
       install opts        _     = do liftIO $ putStrLn $ "[SBV] Unexpected command line options: " ++ show opts
                                      liftIO $ putStrLn   ""
                                      liftIO $ putStrLn   "Options:"
                                      liftIO $ putStrLn   "  runLast     (run the SBV analyzer last)"
                                      liftIO exitFailure

       sbvPass = CoreDoPluginPass "SBV based analysis" pass

       pass :: ModGuts -> CoreM ModGuts
       pass guts@ModGuts{mg_binds} = do

          df   <- getDynFlags
          anns <- getAnnotations deserializeWithData guts

          let wsz = fromJust (bitSizeMaybe (0::Int))

          baseTCs       <- buildTCEnv  wsz
          baseEnv       <- buildFunEnv wsz
          baseDests     <- buildDests  wsz
          uninteresting <- uninterestingTypes
          isEquality    <- buildEquality

          rUninterpreted <- liftIO $ newIORef []
          rUsedNames     <- liftIO $ newIORef []
          rUITypes       <- liftIO $ newIORef []

          let cfg = Config { isGHCi        = hscTarget df == HscInterpreted
                           , opts          = []
                           , sbvAnnotation = lookupWithDefaultUFM anns [] . varUnique
                           , cfgEnv        = Env { curLoc         = noSrcSpan
                                                 , flags          = df
                                                 , machWordSize   = wsz
                                                 , uninteresting  = uninteresting
                                                 , rUninterpreted = rUninterpreted
                                                 , rUsedNames     = rUsedNames
                                                 , rUITypes       = rUITypes
                                                 , isEquality     = isEquality
                                                 , tcMap          = baseTCs
                                                 , envMap         = baseEnv
                                                 , destMap        = baseDests
                                                 , coreMap        = M.fromList (flattenBinds mg_binds)
                                                 }
                           }

          let bindLoc (NonRec b _)     = bindSpan b
              bindLoc (Rec [])         = noSrcSpan
              bindLoc (Rec ((b, _):_)) = bindSpan b

          mapM_ (analyzeBind cfg) $ sortBy (comparing bindLoc) mg_binds

          return guts

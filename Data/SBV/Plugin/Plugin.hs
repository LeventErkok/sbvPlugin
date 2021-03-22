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

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.SBV.Plugin.Plugin(plugin) where

import GHC.Plugins
import System.Exit

import Data.Maybe (fromJust)
import Data.List  (sortBy)
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
       install []          todos = return (sbvPass : todos)
       install ["skip"]    todos = return todos
       install ["runLast"] todos = return (todos ++ [sbvPass])
       install opts        _     = do liftIO $ putStrLn $ "[SBV] Unexpected command line options: " ++ show opts
                                      liftIO $ putStrLn   ""
                                      liftIO $ putStrLn   "Options:"
                                      liftIO $ putStrLn   "  skip        (does not run the plugin)"
                                      liftIO $ putStrLn   "  runLast     (run the SBVPlugin last in the pipeline)"
                                      liftIO exitFailure

       sbvPass = CoreDoPluginPass "SBV based analysis" pass

       pass :: ModGuts -> CoreM ModGuts
       pass guts@ModGuts{mg_binds} = do

          df   <- getDynFlags
          anns <- snd <$> getAnnotations deserializeWithData guts

          let wsz = fromJust (bitSizeMaybe (0::Int))

          baseTCs       <- buildTCEnv  wsz
          baseEnv       <- buildFunEnv wsz
          baseDests     <- buildDests
          uninteresting <- uninterestingTypes
          specials      <- buildSpecials

          rUninterpreted <- liftIO $ newIORef []
          rUsedNames     <- liftIO $ newIORef []
          rUITypes       <- liftIO $ newIORef []

          let cfg = Config { isGHCi        = hscTarget df == HscInterpreted
                           , opts          = []
                           , sbvAnnotation = lookupWithDefaultUFM anns [] . varName
                           , cfgEnv        = Env { curLoc         = []
                                                 , flags          = df
                                                 , machWordSize   = wsz
                                                 , mbListSize     = Nothing
                                                 , uninteresting  = uninteresting
                                                 , rUninterpreted = rUninterpreted
                                                 , rUsedNames     = rUsedNames
                                                 , rUITypes       = rUITypes
                                                 , specials       = specials
                                                 , tcMap          = baseTCs
                                                 , envMap         = baseEnv
                                                 , destMap        = baseDests
                                                 , bailOut        = \s ss -> error $ unlines (s:ss)
                                                 , coreMap        = M.fromList [(b, (varSpan b, e)) | (b, e) <- flattenBinds mg_binds]
                                                 }
                           }

          let bindLoc (NonRec b _)     = varSpan b
              bindLoc (Rec [])         = noSrcSpan
              bindLoc (Rec ((b, _):_)) = varSpan b

              cmp a b = bindLoc a `leftmost_smallest` bindLoc b

          mapM_ (analyzeBind cfg) $ sortBy cmp mg_binds

          return guts

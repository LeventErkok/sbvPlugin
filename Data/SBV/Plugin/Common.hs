---------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Common
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Common data-structures/utilities
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Data.SBV.Plugin.Common where

import Control.Monad (zipWithM, mplus)
import Control.Monad.Reader

import GHC.Plugins
import qualified GHC.Data.Strict as GDS (Maybe(Nothing))

import GHC.Types.Tickish
import GHC.Types.CostCentre
import GHC.Types.Unique (nonDetCmpUnique)

import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Data.IORef

import qualified Data.SBV         as S
import qualified Data.SBV.Dynamic as S

import Data.SBV.Plugin.Data

-- | Certain "very-polymorphic" things are just special
data Specials = Specials { isEquality :: Var -> Maybe Val
                         , isTuple    :: Var -> Maybe Val
                         , isList     :: Var -> Maybe Val
                         }

-- | TyCon's are no longer Ord in GHC 8.2.1; so we make a newtype
newtype TCKey = TCKey (TyCon, [TyCon])

-- | Extract the unique "key"
tcKeyToUList :: TCKey -> [Unique]
tcKeyToUList (TCKey (a, as)) = map getUnique (a:as)

-- | Make a rudimentary Eq instance for TCKey
instance Eq TCKey where
  k1 == k2 = tcKeyToUList k1 == tcKeyToUList k2

-- | Make a rudimentary Ord instance for TCKey
instance Ord TCKey where
  k1 `compare` k2 = tcKeyToUList k1 `cmp` tcKeyToUList k2
    where []     `cmp` []     = EQ
          []     `cmp` _      = LT
          _      `cmp` []     = GT
          (a:as) `cmp` (b:bs) = case a `nonDetCmpUnique` b of
                                   EQ -> as `cmp` bs
                                   r  -> r

-- | Interpreter environment
data Env = Env { curLoc         :: [SrcSpan]
               , flags          :: DynFlags
               , machWordSize   :: Int
               , mbListSize     :: Maybe Int
               , uninteresting  :: [Type]
               , rUninterpreted :: IORef [((Var, Type), (Bool, String, Val))]
               , rUsedNames     :: IORef [String]
               , rUITypes       :: IORef [(Type, S.Kind)]
               , specials       :: Specials
               , tcMap          :: M.Map TCKey S.Kind
               , envMap         :: M.Map (Var, SKind) Val
               , destMap        :: M.Map Var          (Val -> [(Var, SKind)] -> (S.SVal, [((Var, SKind), Val)]))
               , coreMap        :: M.Map Var          (SrcSpan, CoreExpr)
               , bailOut        :: forall a. String -> [String] -> Eval a
               }


-- | The interpreter monad
type Eval a = ReaderT Env S.Symbolic a

-- | Configuration info as we run the plugin
data Config = Config { isGHCi        :: Bool
                     , opts          :: [SBVAnnotation]
                     , sbvAnnotation :: Var -> [SBVAnnotation]
                     , cfgEnv        :: Env
                     }

-- | Given the user options, determine which solver(s) to use
pickSolvers :: [SBVOption] -> IO [S.SMTConfig]
pickSolvers slvrs
  | AnySolver `elem` slvrs = S.getAvailableSolvers
  | True                   = case mapMaybe (`lookup` solvers) slvrs of
                                [] -> return [S.defaultSMTCfg]
                                xs -> return xs
  where solvers = [ (Z3,        S.z3)
                  , (Yices,     S.yices)
                  , (Boolector, S.boolector)
                  , (CVC4,      S.cvc4)
                  , (MathSAT,   S.mathSAT)
                  , (ABC,       S.abc)
                  ]

-- | The kinds used by the plugin
data SKind = KBase S.Kind
           | KTup  [SKind]
           | KLst  SKind
           | KFun  SKind SKind
           deriving (Eq, Ord)

-- | The values kept track of by the interpreter
data Val = Base S.SVal
         | Typ  SKind
         | Tup  [Val]
         | Lst  [Val]
         | Func (Maybe String) (Val -> Eval Val)

-- | Outputable instance for SKind
instance Outputable SKind where
   ppr (KBase k)   = text (show k)
   ppr (KTup  ks)  = parens $ sep (punctuate (text ",") (map ppr ks))
   ppr (KLst  k)   = brackets $ ppr k
   ppr (KFun  k r) = parens (ppr k) <+> text "->" <+> ppr r

-- | Outputable instance for S.Kind
instance Outputable S.Kind where
   ppr = text . show

-- | Outputable instance for Val
instance Outputable Val where
   ppr (Base s)   = text (show s)
   ppr (Typ  k)   = ppr k
   ppr (Tup  vs)  = parens   $ sep $ punctuate (text ",") (map ppr vs)
   ppr (Lst  vs)  = brackets $ sep $ punctuate (text ",") (map ppr vs)
   ppr (Func k _) = text ("Func<" ++ show k ++ ">")

-- | Structural lifting of a boolean function (eq/neq) over Val
liftEqVal :: (S.SVal -> S.SVal -> S.SVal) -> Val -> Val -> S.SVal
liftEqVal baseEq v1 v2 = k v1 v2
  where k (Base a) (Base b)                          = a `baseEq` b
        k (Tup as) (Tup bs) | length as == length bs = foldr S.svAnd S.svTrue                            (zipWith k as bs)
        k (Lst as) (Lst bs)                          = foldr S.svAnd (S.svBool (length as == length bs)) (zipWith k as bs)
        k _ _                                        = error  $ "Impossible happened: liftEq received unexpected arguments: " ++ showSDocUnsafe (ppr (v1, v2))

-- | Symbolic equality over values
eqVal :: Val -> Val -> S.SVal
eqVal = liftEqVal S.svEqual

-- | Symbolic if-then-else over values.
iteVal :: ([String] -> Eval Val) -> S.SVal -> Val -> Val -> Eval Val
iteVal die t v1 v2 = k v1 v2
  where k (Base a) (Base b)                          = return $ Base $ S.svIte t a b
        k (Tup as) (Tup bs) | length as == length bs = Tup `fmap` zipWithM k as bs
        k (Lst as) (Lst bs) | length as == length bs = Lst `fmap` zipWithM k as bs
                            | True                   = die [ "Alternatives are producing lists of differing sizes:"
                                                           , "   Length " ++ show (length as) ++ ": " ++ showSDocUnsafe (ppr (Lst as))
                                                           , "vs Length " ++ show (length bs) ++ ": " ++ showSDocUnsafe (ppr (Lst bs))
                                                           ]
        k (Func n1 f) (Func n2 g)                    = return $ Func (n1 `mplus` n2) $ \a -> f a >>= \fa -> g a >>= \ga -> k fa ga
        k _ _                                        = die [ "Unsupported if-then-else/case with alternatives:"
                                                           , "    Value:" ++ showSDocUnsafe (ppr v1)
                                                           , "       vs:" ++ showSDocUnsafe (ppr v2)
                                                           ]

-- | Compute the span given a Tick. Returns the old-span if the tick span useless.
tickSpan :: GenTickish t -> SrcSpan
tickSpan (ProfNote cc _ _) = cc_loc cc
tickSpan (SourceNote s _)  = RealSrcSpan s GDS.Nothing
tickSpan _                 = noSrcSpan

-- | Compute the span for a binding.
varSpan :: Var -> SrcSpan
varSpan = nameSrcSpan . varName

-- | Pick the first "good" span, hopefully corresponding to
-- the closest location to where we are in the code
-- when we issue an error message.
pickSpan :: [SrcSpan] -> SrcSpan
pickSpan ss = case filter isGoodSrcSpan ss of
                (s:_) -> s
                []    -> noSrcSpan

-- | Show a GHC span in user-friendly form
showSpan :: DynFlags -> SrcSpan -> String
showSpan fs s = showSDoc fs (ppr s)

-- | This comes mighty handy! Wonder why GHC doesn't have it already:
instance Show CoreExpr where
  show = go
    where sh x = showSDocUnsafe (ppr x)
          go (Var   i)      = "(Var "  ++ sh i ++ ")"
          go (Lit   l)      = "(Lit "  ++ sh l ++ ")"
          go (App f a)      = "(App "  ++ go f ++ " " ++ go a ++ ")"
          go (Lam b e)      = "(Lam "  ++ sh b ++ " " ++ go e ++ ")"
          go (Let b e)      = "(Let "  ++ sh b ++ " " ++ go e ++ ")"
          go (Case e b t _) = "(Case " ++ go e ++ " " ++ sh b ++ " " ++ sh t ++ "...)"
          go (Cast e _)     = "(Cast " ++ go e ++ " ...)"
          go (Tick _ e)     = "(Tick " ++ go e ++ ")"
          go (Type t)       = "(Type " ++ sh t ++ ")"
          go (Coercion _)   = "(Coercion ...)"

---------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Env
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- The environment for mapping concrete functions/types to symbolic ones.
-----------------------------------------------------------------------------

{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.SBV.Plugin.Env (buildTCEnv, buildFunEnv, buildDests, buildSpecials, uninterestingTypes) where

import GhcPlugins
import GHC.Prim
import GHC.Types

import qualified Data.Map            as M
import qualified Language.Haskell.TH as TH

import Data.Int
import Data.Word
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Ratio

import qualified Data.SBV         as S hiding (proveWith, proveWithAny)
import qualified Data.SBV.Dynamic as S

import Data.SBV.Plugin.Common

-- | Build the initial environment containing types
buildTCEnv :: Int -> CoreM (M.Map (TyCon, [TyCon]) S.Kind)
buildTCEnv wsz = do xs <- mapM grabTyCon basics
                    ys <- mapM grabTyApp apps
                    return $ M.fromList $ xs ++ ys

  where grab = grabTH lookupTyCon

        grabTyCon (x, k) = grabTyApp (x, [], k)

        grabTyApp (x, as, k) = do fn   <- grab x
                                  args <- mapM grab as
                                  return ((fn, args), k)

        basics = concat [ [(t, S.KBool)              | t <- [''Bool              ]]
                        , [(t, S.KUnbounded)         | t <- [''Integer           ]]
                        , [(t, S.KFloat)             | t <- [''Float,   ''Float# ]]
                        , [(t, S.KDouble)            | t <- [''Double,  ''Double#]]
                        , [(t, S.KBounded True  wsz) | t <- [''Int,     ''Int#   ]]
                        , [(t, S.KBounded True    8) | t <- [''Int8              ]]
                        , [(t, S.KBounded True   16) | t <- [''Int16             ]]
                        , [(t, S.KBounded True   32) | t <- [''Int32,   ''Int32# ]]
                        , [(t, S.KBounded True   64) | t <- [''Int64,   ''Int64# ]]
                        , [(t, S.KBounded False wsz) | t <- [''Word,    ''Word#  ]]
                        , [(t, S.KBounded False   8) | t <- [''Word8             ]]
                        , [(t, S.KBounded False  16) | t <- [''Word16            ]]
                        , [(t, S.KBounded False  32) | t <- [''Word32,  ''Word32#]]
                        , [(t, S.KBounded False  64) | t <- [''Word64,  ''Word64#]]
                        ]

        apps =  [ (''Ratio, [''Integer], S.KReal) ]

-- | Build the initial environment containing functions
buildFunEnv :: Int -> CoreM (M.Map (Id, SKind) Val)
buildFunEnv wsz = M.fromList `fmap` mapM thToGHC (basicFuncs wsz ++ symFuncs wsz)

-- | Basic conversions, only on one kind
basicFuncs :: Int -> [(TH.Name, SKind, Val)]
basicFuncs wsz = [ ('F#,    tlift1 S.KFloat,               Func  Nothing return)
                 , ('D#,    tlift1 S.KDouble,              Func  Nothing return)
                 , ('I#,    tlift1 $ S.KBounded True  wsz, Func  Nothing return)
                 , ('W#,    tlift1 $ S.KBounded False wsz, Func  Nothing return)
                 , ('True,  KBase S.KBool,                 Base  S.svTrue)
                 , ('False, KBase S.KBool,                 Base  S.svFalse)
                 , ('(&&),  tlift2 S.KBool,                lift2 S.svAnd)
                 , ('(||),  tlift2 S.KBool,                lift2 S.svOr)
                 , ('not,   tlift1 S.KBool,                lift1 S.svNot)
                 ]

-- | Symbolic functions supported by the plugin; those from a class.
symFuncs :: Int -> [(TH.Name, SKind, Val)]
symFuncs wsz =  -- equality is for all kinds
          [(op, tlift2Bool k, lift2 sOp) | k <- allKinds, (op, sOp) <- [('(==), S.svEqual), ('(/=), S.svNotEqual)]]

          -- arithmetic
       ++ [(op, tlift1 k, lift1 sOp) | k <- arithKinds, (op, sOp) <- unaryOps]
       ++ [(op, tlift2 k, lift2 sOp) | k <- arithKinds, (op, sOp) <- binaryOps]

          -- literal conversions from Integer
       ++ [(op, KFun S.KUnbounded (KBase k), lift1Int sOp) | k <- integerKinds, (op, sOp) <- [('fromInteger, S.svInteger k)]]

          -- comparisons
       ++ [(op, tlift2Bool k, lift2 sOp) | k <- arithKinds, (op, sOp) <- compOps ]

          -- integer div/rem
      ++ [(op, tlift2 k, lift2 sOp) | k <- integralKinds, (op, sOp) <- [('div, S.svDivide), ('quot, S.svQuot), ('rem, S.svRem)]]

         -- bit-vector
      ++ [ (op, tlift2 k,          lift2 sOp) | k <- bvKinds, (op, sOp) <- bvBinOps   ]
      ++ [ (op, tlift2ShRot wsz k, lift2 sOp) | k <- bvKinds, (op, sOp) <- bvShiftRots]

 where
       -- Bit-vectors
       bvKinds    = [S.KBounded s sz | s <- [False, True], sz <- [8, 16, 32, 64]]

       -- Those that are "integral"ish
       integralKinds = S.KUnbounded : bvKinds

       -- Those that can be converted from an Integer
       integerKinds = S.KReal : integralKinds

       -- Float kinds
       floatKinds = [S.KFloat, S.KDouble]

       -- All arithmetic kinds
       arithKinds = floatKinds ++ integerKinds

       -- Everything
       allKinds   = S.KBool : arithKinds

       -- Unary arithmetic ops
       unaryOps   = [ ('abs,        S.svAbs)
                    , ('negate,     S.svUNeg)
                    , ('complement, S.svNot)
                    ]

       -- Binary arithmetic ops
       binaryOps  = [ ('(+),        S.svPlus)
                    , ('(-),        S.svMinus)
                    , ('(*),        S.svTimes)
                    , ('(/),        S.svDivide)
                    , ('quot,       S.svQuot)
                    , ('rem,        S.svRem)
                    ]

       -- Comparisons
       compOps = [ ('(<),  S.svLessThan)
                 , ('(>),  S.svGreaterThan)
                 , ('(<=), S.svLessEq)
                 , ('(>=), S.svGreaterEq)
                 ]

       -- Binary bit-vector ops
       bvBinOps = [ ('(.&.),   S.svAnd)
                  , ('(.|.),   S.svOr)
                  , ('xor,     S.svXOr)
                  ]

       -- Shift/rotates, where second argument is an int
       bvShiftRots = [ ('shiftL,  S.svShiftLeft)
                     , ('shiftR,  S.svShiftRight)
                     , ('rotateL, S.svRotateLeft)
                     , ('rotateR, S.svRotateRight)
                     ]


-- | Destructors
buildDests :: Int -> CoreM (M.Map (Var, SKind) (S.SVal -> [Var] -> (S.SVal, [((Var, SKind), Val)])))
buildDests wsz = M.fromList `fmap` mapM thToGHC dests
  where dests = [ unbox 'W# (S.KBounded False wsz)
                , unbox 'I# (S.KBounded True  wsz)
                , unbox 'F# S.KFloat
                , unbox 'D# S.KDouble
                ]

        unbox a k     = (a, tlift1 k, dest1 k)
        dest1 k a [b] = (S.svTrue, [((b, KBase k), Base a)])
        dest1 _ a bs  = error $ "Impossible happened: Mistmatched arity case-binder for: " ++ show a ++ ". Expected 1, got: " ++ show (length bs) ++ " arguments."

-- | These types show up during uninterpretation, but are not really "interesting" as they
-- are singly inhabited.
uninterestingTypes :: CoreM [Type]
uninterestingTypes = map varType `fmap` mapM (grabTH lookupId) ['void#]

-- | Certain things are just too special, as they uniformly apply to uninterpreted types.
buildSpecials :: CoreM Specials
buildSpecials = do isEq  <- do eq  <- grabTH lookupId '(==)
                               neq <- grabTH lookupId '(/=)
                               let choose = [(eq, liftEq S.svEqual), (neq, liftEq S.svNotEqual)]
                               return (`lookup` choose)
                   isTup <- do let supported = [2 .. 7]
                                   mkMany n  = Func Nothing g
                                     where g (Typ _) = return $ Func Nothing g
                                           g v       = h (n-1) [v]
                                           h 0 sofar = return $ Tup (reverse sofar)
                                           h i sofar = return $ Func Nothing $ \v -> h (i-1) (v:sofar)
                               ts <- mapM (grabTH lookupId . TH.tupleDataName) supported
                               let choose = zip ts (map mkMany supported)
                               return (`lookup` choose)
                   return Specials{ isEquality = isEq
                                  , isTuple    = isTup
                                  }

-- | Lift a binary type, with result bool
tlift2Bool :: S.Kind -> SKind
tlift2Bool k = KFun k (KFun k (KBase S.KBool))

-- | Lift a binary type
tlift2 :: S.Kind -> SKind
tlift2 k = KFun k (KFun k (KBase k))

-- | Lift a binary type, where second argument is Int
tlift2ShRot :: Int -> S.Kind -> SKind
tlift2ShRot wsz k = KFun k (KFun (S.KBounded True wsz) (KBase k))

-- | Lift a unary type
tlift1 :: S.Kind -> SKind
tlift1 k = KFun k (KBase k)

-- | Lift a unary SBV function that via kind/integer
lift1Int :: (Integer -> S.SVal) -> Val
lift1Int f = Func Nothing g
   where g (Base i) = return $ Base $ f (fromMaybe (error ("Cannot extract an integer from value: " ++ show i)) (S.svAsInteger i))
         g _        = error "Impossible happened: lift1Int received non-base argument!"

-- | Lift a unary SBV function to the plugin value space
lift1 :: (S.SVal -> S.SVal) -> Val
lift1 f = Func Nothing g
  where g (Typ _)  = return $ Func Nothing h
        g v        = h v
        h (Base a) = return $ Base $ f a
        h v        = error  $ "Impossible happened: lift1 received non-base argument: " ++ showSDocUnsafe (ppr v)

-- | Lift a two argument SBV function to our the plugin value space
lift2 :: (S.SVal -> S.SVal -> S.SVal) -> Val
lift2 f = Func Nothing g
   where g (Typ  _)   = return $ Func Nothing h
         g v          = h v
         h (Base a)   = return $ Func Nothing (k a)
         h v          = error  $ "Impossible happened: lift2 received non-base argument (h): " ++ showSDocUnsafe (ppr v)
         k a (Base b) = return $ Base $ f a b
         k _ v        = error  $ "Impossible happened: lift2 received non-base argument (k): " ++ showSDocUnsafe (ppr v)

-- | Lifting an equality is special; since it acts uniformly over tuples.
liftEq :: (S.SVal -> S.SVal -> S.SVal) -> Val
liftEq baseEq = Func Nothing g
   where g (Typ  _) = return $ Func Nothing g
         g v1       = return $ Func Nothing $ \v2 -> return $ Base $ liftEqVal baseEq v1 v2

thToGHC :: (TH.Name, a, b) -> CoreM ((Id, a), b)
thToGHC (n, k, sfn) = do f <- grabTH lookupId n
                         return ((f, k), sfn)

grabTH :: (Name -> CoreM b) -> TH.Name -> CoreM b
grabTH f n = do mbN <- thNameToGhcName n
                case mbN of
                  Just gn -> f gn
                  Nothing -> error $ "[SBV] Impossible happened, while trying to locate GHC name for: " ++ show n

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Plugin.Examples.AES
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- An implementation of AES (Advanced Encryption Standard), using SBVPlugin to prove various
-- correctness properties on it. For details on AES, see FIPS-197: <http://csrc.nist.gov/publications/fips/fips197/fips-197.pdf>.
--
-- This is a straightforward implementation of AES, with no attempt to optimize it in any way. We merely focus on correctness.
-- To see how the SBV library can be used to also generate fast-C code from Haskell, see: <http://hackage.haskell.org/package/sbv/docs/Data-SBV-Examples-Crypto-AES.html>
--
-- All 3 valid key sizes (128, 192, and 256) as required by the FIPS-197 standard
-- are supported.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fplugin=Data.SBV.Plugin #-}
{-# LANGUAGE ParallelListComp            #-}

module Data.SBV.Plugin.Examples.AES where

import Data.SBV  (split, (#))
import Data.SBV.Plugin

import Data.List (transpose)
import Data.Bits
import Data.Word

import Prelude hiding (map, foldr)

-----------------------------------------------------------------------------
-- * Formalizing GF(2^8)
-----------------------------------------------------------------------------

-- | An element of the Galois Field 2^8, which are essentially polynomials with
-- maximum degree 7. They are conveniently represented as values between 0 and 255.
type GF28 = Word8

-- | All elements of GF28. Unfortunately we can't quite use [0 .. 255] here as SBVPlugin doesn't
-- understand 'enumFromTo' and its friends yet. Note that such constructs are typically not
-- symbolically computable since we can't compute the length concretely if the bounds are symbolic.
-- But we should be able to do this when they are constant. Hopefully we'll remedy this in due time 
allGF28 :: [Word8]
allGF28 = go 0
  where go 255 = [255]
        go i   = i : go (i+1)

-- | Multiplication in GF(2^8). We more or less implement the algorithm given here: <https://en.wikipedia.org/wiki/Finite_field_arithmetic#C_programming_example>
-- verbatim, with the exception that we add a recursion counter to "stop" the loop after 8 iterations to ensure symbolic termination is not an issue.
gf28Mult :: GF28 -> GF28 -> GF28
gf28Mult x y = go x y 0 (8::Int)
  where go :: GF28 -> GF28 -> GF28 -> Int -> GF28
        go _ _ p 0 = p
        go _ 0 p _ = p
        go a b p c = go a' b' p' (c-1)
           where p' | b .&. 1 /= 0 = p `xor` a
                    | True         = p

                 a' | a .&. 0x80 /= 0  = (a `shiftL` 1) `xor` 0x1b
                    | True             =  a `shiftL` 1

                 b' = b `shiftR` 1

-- | Exponentiation by a constant in GF(2^8). The implementation uses the usual
-- square-and-multiply trick to speed up the computation.
gf28Pow :: GF28 -> Int -> GF28
gf28Pow n k = pow k
  where sq :: GF28 -> GF28
        sq x = x `gf28Mult` x

        pow :: Int -> GF28
        pow 0           = 1
        pow i
         | i .&. 1 /= 0 = n `gf28Mult` sq (pow (i `shiftR` 1))
         | True         = sq (pow (i `shiftR` 1))

-- | Computing inverses in GF(2^8). By the mathematical properties of GF(2^8)
-- and the particular irreducible polynomial used @x^8+x^5+x^3+x^1+1@, it
-- turns out that raising to the 254 power gives us the multiplicative inverse.
gf28Inverse :: GF28 -> GF28
gf28Inverse x = x `gf28Pow` 254

-- | Prove that 'gf28Inverse' and 'gf28Pow' are really inverses of each other.
{-# ANN gf28PowInverse theorem #-}
gf28PowInverse :: GF28 -> Bool
gf28PowInverse x
   | x == 0 = True   -- 0 does not have an inverse
   | True   = x `gf28Mult` gf28Inverse x == 1

-----------------------------------------------------------------------------
-- * Implementing AES
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- ** Types and basic operations
-----------------------------------------------------------------------------
-- | AES state. The state consists of four 32-bit words, each of which is in turn treated
-- as four GF28's, i.e., 4 bytes. The T-Box implementation keeps the four-bytes together
-- for efficient representation.
type State = [Word32]

-- | The key, which can be 128, 192, or 256 bits. Represented as a sequence of 32-bit words.
type Key = [Word32]

-- | The key schedule. AES executes in rounds, and it treats first and last round keys slightly
-- differently than the middle ones. We reflect that choice by being explicit about it in our type.
-- The length of the middle list of keys depends on the key-size, which in turn determines
-- the number of rounds.
type KS = (Key, [Key], Key)

-- | Conversion from 32-bit words to 4 constituent bytes.
toBytes :: Word32 -> [GF28]
toBytes x = [x1, x2, x3, x4]
  where (h,  l)  = split x
        (x1, x2) = split h
        (x3, x4) = split l

-- | Conversion from 4 constituent bytes to a 32-bit word.
fromBytes :: [GF28] -> Word32
fromBytes [b1, b2, b3, b4] = (b1 # b2) # (b3 # b4)
fromBytes xs               = error $ "fromBytes: Unexpected input: " ++ show xs

-- | Prove that 'toBytes' and 'fromBytes' are inverses of each other.
{-# ANN toFromBytes theorem #-}
toFromBytes :: Word32 -> Bool
toFromBytes x = fromBytes (toBytes x) == x

-- | Prove that 'toBytes' and 'fromBytes' are inverses of each other.
{-# ANN fromToBytes theorem {options = [ListSize 4]} #-}
fromToBytes :: [GF28] -> Bool
fromToBytes xs = toBytes (fromBytes xs) == xs

-- | Rotating a state row by a fixed amount to the right.
rotR :: [GF28] -> Int -> [GF28]
rotR [a, b, c, d] 1 = [d, a, b, c]
rotR [a, b, c, d] 2 = [c, d, a, b]
rotR [a, b, c, d] 3 = [b, c, d, a]
rotR xs           i = error $ "rotR: Unexpected input: " ++ show (xs, i)

-----------------------------------------------------------------------------
-- ** The key schedule
-----------------------------------------------------------------------------

-- | Definition of round-constants, as specified in Section 5.2 of the AES standard.
roundConstants :: [GF28]
roundConstants = 0 : [ gf28Pow 2 (k-1) | k <- [1 .. ] ]

-- | Prelude's version of !!, except it returns 0 if the index is not found:
nth :: (Eq b, Num a, Num b) => [a] -> b -> a
nth []     _ = 0
nth (x:_)  0 = x
nth (_:xs) i = nth xs (i-1)

-- | The @InvMixColumns@ transformation, as described in Section 5.3.3 of the standard. Note
-- that this transformation is only used explicitly during key-expansion in the T-Box implementation
-- of AES.
invMixColumns :: State -> State
invMixColumns state = map fromBytes $ transpose $ mmult (map toBytes state)
 where dot f   = foldr1 xor . zipWith ($) f
       mmult n = [map (dot r) n | r <- [ [mE, mB, mD, m9]
                                       , [m9, mE, mB, mD]
                                       , [mD, m9, mE, mB]
                                       , [mB, mD, m9, mE]
                                       ]]
       -- table-lookup versions of gf28Mult with the constants used in invMixColumns
       mE = nth mETable
       mB = nth mBTable
       mD = nth mDTable
       m9 = nth m9Table
       mETable = map (gf28Mult 0xE) allGF28
       mBTable = map (gf28Mult 0xB) allGF28
       mDTable = map (gf28Mult 0xD) allGF28
       m9Table = map (gf28Mult 0x9) allGF28

-- | Key expansion. Starting with the given key, returns an infinite sequence of
-- words, as described by the AES standard, Section 5.2, Figure 11.
keyExpansion :: Int -> Key -> [Key]
keyExpansion nk key = chop4 keys
   where keys :: [Word32]
         keys = key ++ [nextWord i prev old | i <- [nk ..] | prev <- drop (nk-1) keys | old <- keys]
         chop4 :: [a] -> [[a]]
         chop4 xs = let (f, r) = splitAt 4 xs in f : chop4 r
         nextWord :: Int -> Word32 -> Word32 -> Word32
         nextWord i prev old
           | i `mod` nk == 0           = old `xor` subWordRcon (prev `rotateL` 8) (roundConstants !! (i `div` nk))
           | i `mod` nk == 4 && nk > 6 = old `xor` subWordRcon prev 0
           | True                      = old `xor` prev
         subWordRcon :: Word32 -> GF28 -> Word32
         subWordRcon w rc = fromBytes [a `xor` rc, b, c, d]
            where [a, b, c, d] = map sbox $ toBytes w

-----------------------------------------------------------------------------
-- ** The S-box transformation
-----------------------------------------------------------------------------

-- | The values of the AES S-box table. Note that we describe the S-box programmatically
-- using the mathematical construction given in Section 5.1.1 of the standard. However,
-- the code-generation will turn this into a mere look-up table, as it is just a
-- constant table, all computation being done at \"compile-time\".
sboxTable :: [GF28]
sboxTable = [xformByte (gf28Inverse b) | b <- allGF28]
  where xformByte :: GF28 -> GF28
        xformByte b = foldr xor 0x63 [b `rotateR` i | i <- [0, 4, 5, 6, 7]]

-- | The sbox transformation. We simply use @nth@ to index into the sbox table.
sbox :: GF28 -> GF28
sbox = nth sboxTable

-----------------------------------------------------------------------------
-- ** The inverse S-box transformation
-----------------------------------------------------------------------------

-- | The values of the inverse S-box table. Again, the construction is programmatic.
unSBoxTable :: [GF28]
unSBoxTable = [gf28Inverse (xformByte b) | b <- allGF28]
  where xformByte :: GF28 -> GF28
        xformByte b = foldr xor 0x05 [b `rotateR` i | i <- [2, 5, 7]]

-- | The inverse s-box transformation.
unSBox :: GF28 -> GF28
unSBox = nth unSBoxTable

-- | Prove that the 'sbox' and 'unSBox' are inverses.
{-# ANN sboxInverseCorrect theorem #-}
sboxInverseCorrect :: GF28 -> Bool
sboxInverseCorrect x = unSBox (sbox x) == x && sbox (unSBox x) == x

-----------------------------------------------------------------------------
-- ** AddRoundKey transformation
-----------------------------------------------------------------------------

-- | Adding the round-key to the current state. We simply exploit the fact
-- that addition is just xor in implementing this transformation.
addRoundKey :: Key -> State -> State
addRoundKey = zipWith xor

-----------------------------------------------------------------------------
-- ** Tables for T-Box encryption
-----------------------------------------------------------------------------

-- | T-box table generation function for encryption
t0Func :: GF28 -> [GF28]
t0Func a = [s `gf28Mult` 2, s, s, s `gf28Mult` 3] where s = sbox a

-- | First look-up table used in encryption
t0 :: GF28 -> Word32
t0 = nth t0Table where t0Table = [fromBytes (t0Func a)          | a <- allGF28]

-- | Second look-up table used in encryption
t1 :: GF28 -> Word32
t1 = nth t1Table where t1Table = [fromBytes (t0Func a `rotR` 1) | a <- allGF28]

-- | Third look-up table used in encryption
t2 :: GF28 -> Word32
t2 = nth t2Table where t2Table = [fromBytes (t0Func a `rotR` 2) | a <- allGF28]

-- | Fourth look-up table used in encryption
t3 :: GF28 -> Word32
t3 = nth t3Table where t3Table = [fromBytes (t0Func a `rotR` 3) | a <- allGF28]

-----------------------------------------------------------------------------
-- ** Tables for T-Box decryption
-----------------------------------------------------------------------------

-- | T-box table generating function for decryption
u0Func :: GF28 -> [GF28]
u0Func a = [s `gf28Mult` 0xE, s `gf28Mult` 0x9, s `gf28Mult` 0xD, s `gf28Mult` 0xB] where s = unSBox a

-- | First look-up table used in decryption
u0 :: GF28 -> Word32
u0 = nth t0Table where t0Table = [fromBytes (u0Func a)          | a <- allGF28]

-- | Second look-up table used in decryption
u1 :: GF28 -> Word32
u1 = nth t1Table where t1Table = [fromBytes (u0Func a `rotR` 1) | a <- allGF28]

-- | Third look-up table used in decryption
u2 :: GF28 -> Word32
u2 = nth t2Table where t2Table = [fromBytes (u0Func a `rotR` 2) | a <- allGF28]

-- | Fourth look-up table used in decryption
u3 :: GF28 -> Word32
u3 = nth t3Table where t3Table = [fromBytes (u0Func a `rotR` 3) | a <- allGF28]

-----------------------------------------------------------------------------
-- ** AES rounds
-----------------------------------------------------------------------------

-- | Generic round function. Given the function to perform one round, a key-schedule,
-- and a starting state, it performs the AES rounds.
doRounds :: (Bool -> State -> Key -> State) -> KS -> State -> State
doRounds rnd (ikey, rkeys, fkey) sIn = rnd True (last rs) fkey
  where s0 = ikey `addRoundKey` sIn
        rs = s0 : [rnd False s k | s <- rs | k <- rkeys ]

-- | One encryption round. The first argument indicates whether this is the final round
-- or not, in which case the construction is slightly different.
aesRound :: Bool -> State -> Key -> State
aesRound isFinal s key = d `addRoundKey` key
  where d = map (f isFinal) [0,1,2,3]
        a = map toBytes s
        f True j = fromBytes [ sbox (a !! ((j+0) `mod` 4) !! 0)
                             , sbox (a !! ((j+1) `mod` 4) !! 1)
                             , sbox (a !! ((j+2) `mod` 4) !! 2)
                             , sbox (a !! ((j+3) `mod` 4) !! 3)
                             ]
        f False j = e0 `xor` e1 `xor` e2 `xor` e3
              where e0 = t0 (a !! ((j+0) `mod` 4) !! 0)
                    e1 = t1 (a !! ((j+1) `mod` 4) !! 1)
                    e2 = t2 (a !! ((j+2) `mod` 4) !! 2)
                    e3 = t3 (a !! ((j+3) `mod` 4) !! 3)

-- | One decryption round. Similar to the encryption round, the first argument
-- indicates whether this is the final round or not.
aesInvRound :: Bool -> State -> Key -> State
aesInvRound isFinal s key = d `addRoundKey` key
  where d = map (f isFinal) [0..3]
        a = map toBytes s
        f True j = fromBytes [ unSBox (a !! ((j+0) `mod` 4) !! 0)
                             , unSBox (a !! ((j+3) `mod` 4) !! 1)
                             , unSBox (a !! ((j+2) `mod` 4) !! 2)
                             , unSBox (a !! ((j+1) `mod` 4) !! 3)
                             ]
        f False j = e0 `xor` e1 `xor` e2 `xor` e3
              where e0 = u0 (a !! ((j+0) `mod` 4) !! 0)
                    e1 = u1 (a !! ((j+3) `mod` 4) !! 1)
                    e2 = u2 (a !! ((j+2) `mod` 4) !! 2)
                    e3 = u3 (a !! ((j+1) `mod` 4) !! 3)

-----------------------------------------------------------------------------
-- * AES API
-----------------------------------------------------------------------------

-- | Key schedule. Given a 128, 192, or 256 bit key, expand it to get key-schedules
-- for encryption and decryption. The key is given as a sequence of 32-bit words.
-- (4 elements for 128-bits, 6 for 192, and 8 for 256.)
aesKeySchedule :: Key -> (KS, KS)
aesKeySchedule key = (encKS, decKS)
  where nk = length key
        nr = nk + 6
        encKS@(f, m, l) = (head rKeys, take (nr-1) (tail rKeys), rKeys !! nr)
        decKS = (l, map invMixColumns (reverse m), f)
        rKeys = keyExpansion nk key

-- | Block encryption. The first argument is the plain-text, which must have
-- precisely 4 elements, for a total of 128-bits of input. The second
-- argument is the key-schedule to be used, obtained by a call to 'aesKeySchedule'.
-- The output will always have 4 32-bit words, which is the cipher-text.
aesEncrypt :: [Word32] -> KS -> [Word32]
aesEncrypt pt encKS = doRounds aesRound encKS pt

-- | Block decryption. The arguments are the same as in 'aesEncrypt', except
-- the first argument is the cipher-text and the output is the corresponding
-- plain-text.
aesDecrypt :: [Word32] -> KS -> [Word32]
aesDecrypt ct decKS = doRounds aesInvRound decKS ct

-----------------------------------------------------------------------------
-- * Test vectors
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- ** 128-bit enc/dec test
-----------------------------------------------------------------------------

-- | 128-bit encryption test, expressed as a theorem. From Appendix C.1 of the AES standard.
{-# ANN t128Enc theorem #-}
t128Enc :: Bool
t128Enc = aesEncrypt pt ks == [0x69c4e0d8, 0x6a7b0430, 0xd8cdb780, 0x70b4c55a]
  where pt  = [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff]
        key = [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f]
        (ks, _) = aesKeySchedule key

-- | 128-bit decryption test, expressed as a theorem. From Appendix C.1 of the AES standard.
{-# ANN t128Dec theorem #-}
t128Dec :: Bool
t128Dec = aesDecrypt ct ks == [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff]
  where ct  = [0x69c4e0d8, 0x6a7b0430, 0xd8cdb780, 0x70b4c55a]
        key = [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f]
        (_, ks) = aesKeySchedule key

-----------------------------------------------------------------------------
-- ** 192-bit enc/dec test
-----------------------------------------------------------------------------

-- | 192-bit encryption test, expressed as a theorem. From Appendix C.1 of the AES standard.
{-# ANN t192Enc theorem #-}
t192Enc :: Bool
t192Enc = aesEncrypt pt ks == [0xdda97ca4, 0x864cdfe0, 0x6eaf70a0, 0xec0d7191]
  where pt  = [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff]
        key = [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f, 0x10111213, 0x14151617]
        (ks, _) = aesKeySchedule key

-- | 192-bit decryption test, expressed as a theorem. From Appendix C.2 of the AES standard.
{-# ANN t192Dec theorem #-}
t192Dec :: Bool
t192Dec = aesDecrypt ct ks == [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff]
  where ct  = [0xdda97ca4, 0x864cdfe0, 0x6eaf70a0, 0xec0d7191]
        key = [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f, 0x10111213, 0x14151617]
        (_, ks) = aesKeySchedule key

-----------------------------------------------------------------------------
-- ** 256-bit enc/dec test
-----------------------------------------------------------------------------

-- | 256-bit encryption test, expressed as a theorem. From Appendix C.3 of the AES standard:
{-# ANN t256Enc theorem #-}
t256Enc :: Bool
t256Enc = aesEncrypt pt ks == [0x8ea2b7ca, 0x516745bf, 0xeafc4990, 0x4b496089]
  where pt  = [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff]
        key = [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f, 0x10111213, 0x14151617, 0x18191a1b, 0x1c1d1e1f]
        (ks, _) = aesKeySchedule key

-- | 256-bit decryption test, expressed as a theorem. From Appendix C.3 of the AES standard:
{-# ANN t256Enc theorem #-}
t256Dec :: Bool
t256Dec = aesDecrypt ct ks == [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff]
  where ct  = [0x8ea2b7ca, 0x516745bf, 0xeafc4990, 0x4b496089]
        key = [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f, 0x10111213, 0x14151617, 0x18191a1b, 0x1c1d1e1f]
        (_, ks) = aesKeySchedule key

-----------------------------------------------------------------------------
-- * Verification
-- ${verifIntro}
-----------------------------------------------------------------------------
{- $verifIntro
  While SMT based technologies can prove correct many small properties fairly quickly, it would
  be naive for them to automatically verify that our AES implementation is correct. (By correct,
  we mean decryption follewed by encryption yielding the same result.) However, we can state
  this property precisely using SBV, and use quick-check to gain some confidence.
-}

-- | Correctness theorem for 128-bit AES. Ideally, we would run:
--
-- @
--   prove aes128IsCorrect
-- @
--
-- to get a proof automatically. Unfortunately, while SBV will successfully generate the proof
-- obligation for this theorem and ship it to the SMT solver, it would be naive to expect the SMT-solver
-- to finish that proof in any reasonable time with the currently available SMT solving technologies.
-- Instead, we simply use the @QuickCheck@ option of the plugin to get some confidence.
{-# ANN aes128IsCorrect theorem {options = [QuickCheck]} #-}
aes128IsCorrect :: (Word32, Word32, Word32, Word32)  -- ^ plain-text words
                -> (Word32, Word32, Word32, Word32)  -- ^ key-words
                -> Bool                              -- ^ True if round-trip gives us plain-text back
aes128IsCorrect (i0, i1, i2, i3) (k0, k1, k2, k3) = pt == pt'
   where pt  = [i0, i1, i2, i3]
         key = [k0, k1, k2, k3]
         (encKS, decKS) = aesKeySchedule key
         ct  = aesEncrypt pt encKS
         pt' = aesDecrypt ct decKS

-----------------------------------------------------------------------------
-- Plugin needs all used definitions to be present in the current module. We
-- should really remove this restriction. Here' we redefine a few Prelude
-- functions we use to avoid the issue.
-----------------------------------------------------------------------------

-- | Redefine map locally, so it doesn't get uninterpreted.
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- | Redefine foldr locally, so it doesn't get uninterpreted.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b []     = b
foldr f b (x:xs) = x `f` foldr f b xs

{-# ANN aesRound    ("HLint: ignore Use head" :: String) #-}
{-# ANN aesInvRound ("HLint: ignore Use head" :: String) #-}
{-# ANN gf28Pow     ("HLint: ignore Eta reduce" :: String) #-}

{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ScopedTypeVariables
           , CPP
           , MagicHash
           , UnboxedTuples
  #-}

#include "ieee-flpt.h"


module MyFloat (piFloat, piDouble)
where

import GHC.Base
import GHC.Integer.Logarithms.Internals hiding ( roundingMode# )
import Data.Bits
import GHC.Real

piFloat :: Float
piFloat = let (n:%d) = 3.141592653589793238 in rationalToFloat n d

piDouble :: Double
piDouble = let (n:%d) = 3.141592653589793238 in rationalToDouble n d

rationalToFloat :: Integer -> Integer -> Float
{-# NOINLINE [1] rationalToFloat #-}
rationalToFloat n 0
    | n == 0        = 0/0
    | n < 0         = (-1)/0
    | otherwise     = 1/0
rationalToFloat n d
    | n == 0        = encodeFloat 0 0
    | n < 0         = -(fromRat'' minEx mantDigs (-n) d)
    | otherwise     = fromRat'' minEx mantDigs n d
      where
        minEx       = FLT_MIN_EXP
        mantDigs    = FLT_MANT_DIG


rationalToDouble :: Integer -> Integer -> Double
{-# NOINLINE [1] rationalToDouble #-}
rationalToDouble n 0
    | n == 0        = 0/0
    | n < 0         = (-1)/0
    | otherwise     = 1/0
rationalToDouble n d
    | n == 0        = encodeFloat 0 0
    | n < 0         = -(fromRat'' minEx mantDigs (-n) d)
    | otherwise     = fromRat'' minEx mantDigs n d
      where
        minEx       = DBL_MIN_EXP
        mantDigs    = DBL_MANT_DIG


{-# SPECIALISE fromRat'' :: Int -> Int -> Integer -> Integer -> Float,
                            Int -> Int -> Integer -> Integer -> Double #-}
fromRat'' :: RealFloat a => Int -> Int -> Integer -> Integer -> a
-- Invariant: n and d strictly positive
fromRat'' minEx@(I# me#) mantDigs@(I# md#) n d =
    case integerLog2IsPowerOf2# d of
      (# ld#, pw# #)
        | isTrue# (pw# ==# 0#) ->
          case integerLog2# n of
            ln# | isTrue# (ln# >=# (ld# +# me# -# 1#)) ->
                  -- this means n/d >= 2^(minEx-1), i.e. we are guaranteed to get
                  -- a normalised number, round to mantDigs bits
                  if isTrue# (ln# <# md#)
                    then encodeFloat n (I# (negateInt# ld#))
                    else let n'  = n `shiftR` (I# (ln# +# 1# -# md#))
                             n'' = case roundingMode# n (ln# -# md#) of
                                    0# -> n'
                                    2# -> n' + 1
                                    _  -> case fromInteger n' .&. (1 :: Int) of
                                            0 -> n'
                                            _ -> n' + 1
                         in encodeFloat n'' (I# (ln# -# ld# +# 1# -# md#))
                | otherwise ->
                  -- n/d < 2^(minEx-1), a denorm or rounded to 2^(minEx-1)
                  -- the exponent for encoding is always minEx-mantDigs
                  -- so we must shift right by (minEx-mantDigs) - (-ld)
                  case ld# +# (me# -# md#) of
                    ld'# | isTrue# (ld'# <=# 0#) -> -- we would shift left, so we don't shift
                           encodeFloat n (I# ((me# -# md#) -# ld'#))
                         | isTrue# (ld'# <=# ln#) ->
                           let n' = n `shiftR` (I# ld'#)
                           in case roundingMode# n (ld'# -# 1#) of
                                0# -> encodeFloat n' (minEx - mantDigs)
                                1# -> if fromInteger n' .&. (1 :: Int) == 0
                                        then encodeFloat n' (minEx-mantDigs)
                                        else encodeFloat (n' + 1) (minEx-mantDigs)
                                _  -> encodeFloat (n' + 1) (minEx-mantDigs)
                         | isTrue# (ld'# ># (ln# +# 1#)) -> encodeFloat 0 0 -- result of shift < 0.5
                         | otherwise ->  -- first bit of n shifted to 0.5 place
                           case integerLog2IsPowerOf2# n of
                            (# _, 0# #) -> encodeFloat 0 0  -- round to even
                            (# _, _ #)  -> encodeFloat 1 (minEx - mantDigs)
        | otherwise ->
          let ln = I# (integerLog2# n)
              ld = I# ld#
              -- 2^(ln-ld-1) < n/d < 2^(ln-ld+1)
              p0 = max minEx (ln - ld)
              (n', d')
                | p0 < mantDigs = (n `shiftL` (mantDigs - p0), d)
                | p0 == mantDigs = (n, d)
                | otherwise     = (n, d `shiftL` (p0 - mantDigs))
              -- if ln-ld < minEx, then n'/d' < 2^mantDigs, else
              -- 2^(mantDigs-1) < n'/d' < 2^(mantDigs+1) and we
              -- may need one scaling step
              scale p a b
                | (b `shiftL` mantDigs) <= a = (p+1, a, b `shiftL` 1)
                | otherwise = (p, a, b)
              (p', n'', d'') = scale (p0-mantDigs) n' d'
              -- n''/d'' < 2^mantDigs and p' == minEx-mantDigs or n''/d'' >= 2^(mantDigs-1)
              {-
              rdq = case n'' `quotRem` d'' of
                     (q,r) -> case compare (r `shiftL` 1) d'' of
                                LT -> q
                                EQ -> if fromInteger q .&. (1 :: Int) == 0
                                        then q else q+1
                                GT -> q+1
              -}
              rdq = n'' `quot` d''
          in  encodeFloat rdq p'

{-
-- Assumption: Integer and Int# are strictly positive, Int# is less
-- than logBase 2 of Integer, otherwise havoc ensues.
-- Used only for the numerator in fromRational when the denominator
-- is a power of 2.
-- The Int# argument is log2 n minus the number of bits in the mantissa
-- of the target type, i.e. the index of the first non-integral bit in
-- the quotient.
--
-- 0# means round down (towards zero)
-- 1# means we have a half-integer, round to even
-- 2# means round up (away from zero)
roundingMode# :: Integer -> Int# -> Int#
roundingMode# (IS i#) t =
   let
      k = int2Word# i# `and#` ((uncheckedShiftL# 2## t) `minusWord#` 1##)
      c = uncheckedShiftL# 1## t
   in if isTrue# (c `gtWord#` k)
         then 0#
         else if isTrue# (c `ltWord#` k)
                 then 2#
                 else 1#

roundingMode# (IN _)  _ = errorWithoutStackTrace "roundingMode#: IN" -- See the Assumption
roundingMode# (IP bn) t =
   let
      j = word2Int# (int2Word# t `and#` MMASK##) -- index of relevant bit in word
      k = uncheckedIShiftRA# t WSHIFT#           -- index of relevant word
      r = bigNatIndex# bn k `and#` ((uncheckedShiftL# 2## j) `minusWord#` 1##)
      c = uncheckedShiftL# 1## j
      test i = if isTrue# (i <# 0#)
                  then 1#
                  else case bigNatIndex# bn i of
                          0## -> test (i -# 1#)
                          _   -> 2#
   in if isTrue# (c `gtWord#` r)
         then 0#
         else if isTrue# (c `ltWord#` r)
                 then 2#
                 else test (k -# 1#)

-}

roundingMode# :: Integer -> Int# -> Int#
roundingMode# _ _ = 0#
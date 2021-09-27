{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module MyFloat (asinh, atanh, copySign)
where

import           Prelude       hiding (asinh, atanh)
import           Numeric              (log1p)


{- ASINH

The plan is to fix mingw-w64 (https://github.com/mirror/mingw-w64/blob/master/mingw-w64-crt/math/x86/asinh.c, and float, long double, etc).
This is a (fixed) Haskell reimplementation of that code.
It fixes https://sourceforge.net/p/mingw-w64/bugs/916/ (fails for large values) 
and https://sourceforge.net/p/mingw-w64/bugs/515/ (asinh 0 gives -0.0).
-}

asinh :: RealFloat a => a -> a
asinh x | isInfinite x = x
        | otherwise =
  let y = abs x
      z | y >= sqrt floatSuccLim  =  log 2 + log y
        | otherwise               =  log1p (y + y*y/(sqrt (y*y + 1) + 1))
  in copySign z x

{-
when y*y+1 == y*y:
    log1p (y + sqrt (y * y + 1.0) - 1.0)
=   log1p (y + sqrt (y * y      ) - 1.0)
=   log1p (y +       y            - 1.0)
=   log   (y +       y                 )
=   log   (2 *       y                 )
=   log    2 + log   y                    FIXME: At y = sqrt floatSuccLim, this is slightly out (and causes temp descending behaviour).
-}


{- ATANH
Fix of https://github.com/mirror/mingw-w64/blob/master/mingw-w64-crt/math/x86/atanh.c
Which only has the sign problem for zero (atanh (-0.0) gives 0.0).
-}

atanh :: RealFloat a => a -> a
atanh x = atanh' $ abs x
  where
    atanh' y | isNaN x = x
             | y == 1 = if x > 0 then 1/0 else -1/0
             | y > 1 = 0/0
             | otherwise =
      let z = 0.5 * log1p ((y + y) / (1 - y));
      in copySign z x

{-
copySign x y returns the value of x, but with the sign of y.
returns NaN if x is NaN. Per IEEE spec, "result is undefined" if y is NaN.
-}

copySign :: RealFloat a => a -> a -> a
copySign x y | makePos   = abs x
             | otherwise = negate $ abs x
  where
    makePos | isNegativeZero y = False
            | y < 0            = False
            | otherwise        = True

--at and beyond x=floatSuccLim, x+1 == x
floatSuccLim :: forall a. RealFloat a => a
floatSuccLim = encodeFloat (floatRadix @a undefined ^ floatDigits @a undefined) 0

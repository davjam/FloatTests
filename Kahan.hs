{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}


-- see https://people.freebsd.org/~das/kahan86branch.pdf

module Kahan (
    cAbs, cArg,
    cSqrt, cLog,  
    absTests, argTests
  ) where

import Data.Complex
import Numeric
--import Debug.Trace

--https://www.cplusplus.com/reference/cmath/copysign/?kw=copysign
--foreign import ccall unsafe "math.h copysignf"       copysign_f       :: Float  -> Float  -> Float      --XXXX IO? CFloat?
--foreign import ccall unsafe "math.h copysign"        copysign_d       :: Double -> Double -> Double
--foreign import ccall unsafe "math.h hypotf"          hypot_f          :: Float  -> Float  -> Float
--foreign import ccall unsafe "math.h hypot"           hypot_d          :: Double -> Double -> Double
--foreign import ccall unsafe "math.h atan2"           atan2_d          :: Double -> Double -> Double      --XXXX no float version. (Should match haskell).
--foreign import ccall unsafe "math.h asinhf"          asinh_f          :: Float  -> Float
--foreign import ccall unsafe "math.h asinh"           asinh_d          :: Double -> Double

--foreign import ccall "exp" c_exp :: Double -> Double
--foreign import ccall "asinh" c_asinh :: Double -> Double

{-
This is a direct implementation of cAbs, except for exception flag handling
should give same answer as magnitude
-}

_cAbsK :: RealFloat a => Complex a -> a         -- _fnK is a "direct reflection" of Kahan's imperative code
_cAbsK (x0:+y0) =
  let r2   = sqrt(2)
      r2p1 = 1 + r2
      t2p1 = 1 + r2 - r2p1
      x1 = abs x0
      y1 = abs y0
      s0 = 0
      (x2,y2) = if x1 < y1 then (y1,x1) else (x1,y1)
      x3 = if isInfinite y2 then y2 else x2
      t0 = x3 - y2
      s3 | not (isInfinite x3) && t0 /= x3 =
            let s2 = if t0 > y2 then let s1 = x3/y2
                                     in  s1 + sqrt(1+s1*s1)
                                else let s1 = t0/y2
                                         t1 = (2+s1)*s1
                                     in  ((t2p1 + t1/(r2+sqrt(2+t1))) + s1) + r2p1
            in y2 / s2
         | otherwise = s0
  in x3 + s3

_cAbsH :: RealFloat a => Complex a -> a                     -- _fnH is a better functional version
_cAbsH (x0:+y0) = uncurry cAbs' $ sort2 (abs x0) (abs y0)
  where
    cAbs' x y   | isInfinite x = x
                | isInfinite y = y
                | t == x       = x
                | t > y        = x + y/((x/y)+sqrt(1+(x/y)*(x/y)))
                | otherwise    = x + y/(((t2p1 + u/(r2+sqrt(2+u)))+s)+r2p1)
      where
        t    = x-y
        s    = t/y
        u    = (2+s)*s
        r2   = sqrt(2)
        r2p1 = 1 + r2
        t2p1 = 1 + r2 - r2p1
    sort2 x y | x < y     = (y,x)
              | otherwise = (x,y)

cAbs :: RealFloat a => Complex a -> a
cAbs z = magnitude z

absTests :: RealFloat a => (Complex a -> a) -> Bool
absTests f = and [
  f (3:+4) == 5,
  f (4:+3) == 5,
  f (5:+12) == 13,
  f (12:+5) == 13,
  f (0:+0) == 0,
  f (0:+(-0)) == 0,
  f ((-0):+0) == 0,
  f ((-0):+(-0)) == 0,
  f ((5/0):+5) == 5/0,
  f (5:+5/0) == 5/0,
  f (5/0:+5/0) == 5/0,
  f (huge:+tiny) == huge,
  f (tiny:+huge) == huge
  ]
  where
    huge = 123456789012345678901234567890
    tiny = 4

{-
Should match phase, but phase fails for 0:+(-0), etc.
-}

cArg :: RealFloat a => Complex a -> a
cArg z = phase z

_cArgK :: RealFloat a => Complex a -> a
_cArgK (x0:+y0) = 
  let x1 = if x0 == 0 && y0 == 0 then copySign 1 x0 else x0
      _z0 = undefined -- z = CBOX???
      th0 | abs y0 > abs x1 = copySign (pi/2) y0 - atan (x1/y0)
          | x1 < 0          = copySign  pi    y0 + atan (y0/x1)
          | otherwise       = atan(y0/x1)
  in th0
   
argTests :: RealFloat a => (Complex a -> a) -> Bool
argTests f = and [
  f ((1):+(0)) == 0,
  f ((0):+(1)) == pi/2,
  f ((-0):+(1)) == pi/2,
  f ((-1):+(0)) == pi,
  f ((-1):+(-0)) == -pi,
  f ((-0):+(-1)) == -pi/2,
  f ((0):+(-1)) == -pi/2,
  f ((1):+(-0)) == -0,
  f ((0):+(0)) == 0,
  f ((0):+(-0)) == -0,
  f ((-0):+(0)) == pi,
  f ((-0):+(-0)) == -pi
  ]

{-
class RealFloat a => IEEE a where
  copySign :: a -> a -> a
  hypot :: a -> a -> a
  atan2C :: a -> a -> a
--  asinhC :: a -> a

instance IEEE Double where
  copySign = copysign_d
  hypot    = hypot_d   
  atan2C   = atan2_d   
--  asinhC   = asinh_d
                              
instance IEEE Float where     
  copySign = copysign_f
  hypot    = hypot_f   
  atan2C   = undefined
--  asinhC   = asinh_f
-}


{-
minPositiveFloat :: forall a. RealFloat a => a
minPositiveFloat = encodeFloat 1 $ fst (floatRange a) - floatDigits a
  where a = undefined :: a
-}


_cLog :: RealFloat a => Complex a -> Complex a
_cLog z = cLogS z 0

cLogS :: forall a. RealFloat a => Complex a -> a -> Complex a
cLogS z@(x:+y) j =
  let
    (rh,k) = cSSqs z :: (a, Int)
    b  = max (abs x) (abs y)  :: a
    th = min (abs x) (abs y)
    rh' | k == 0 && t0 < b && (b <= t1 ||rh < t2) = log1p((b-1)*(b*1)+th*th) / 2
        | otherwise = log rh / 2 + (fromIntegral k + j) * log 2
    th' = _cArgK z
  in rh' :+ th'
  
  where
    t0 = 1/sqrt 2 :: a
    t1 = 5/4
    t2 = 3
    
cSSqs :: RealFloat a
      => Complex a   --z@(x:+y)
      -> (a, Int)    --(r,k) s.t. r=abs(z/2^k)^2
cSSqs (x:+y) = (r1,k1) where
  k0 = 0
  r0 = x*x+y*y
  
  (r1,k1) | (r0 /= r0 || isInfinite r0) && (isInfinite x || isInfinite y) = (1/0, k0)
          | isInfinite r0 {-|| something-} = let k = floor(logBase 2 (max (abs x) (abs y)))
                                                 r = sq(scaleFloat (-k) x) + sq(scaleFloat (-k) y)
                                             in (r,k)
          | otherwise = (r0, k0)

sq :: RealFloat a => a -> a
sq z = z * z

cLog :: RealFloat a => Complex a -> Complex a
cLog z@(x:+y) = (p2 :+ phase z)
  where
    t0 = 1/(sqrt 2)
    t1 = 1.25
    t2 = 3.0
    (p,k) = cSSqs z
    
    b = max (abs x) (abs y)
    th = min (abs x) (abs y)
    p2 | k == 0 && t0 < b && (b <= t1 || p < t2) = log1p((b-1)*(b+1)+th*th)
       | otherwise = log(p)/2 + fromIntegral k*log 2
  



{-
Initial copy of Kahan.
cSSqs :: RealFloat a
      => Complex a   --z@(x:+y)
      -> (a, Int)    --(r,k) s.t. r=abs(z/2^k)^2
cSSqs (x:+y) = (r1,k1) where
  k0 = 0
  r0 = x*x+y*y
  
  (r1,k1) | (r0 /= r0 || isInfinite r0) && (isInfinite x || isInfinite y) = (1/0, k0)
          | isInfinite r0 {-|| something-} = let k = floor(logBase 2 (max (abs x) (abs y)))
                                                 r = sq(scaleFloat (-k) x) + sq(scaleFloat (-k) y)
                                             in (r,k)
          | otherwise = (r0, k0)
  sq z = z * z



cSqrt :: RealFloat a => Complex a -> Complex a
cSqrt z@(x:+y) = w1:+n1
  where
    (r,k) = cSSqs z
    r1 | x == x = scaleFloat (-k) (abs x) + sqrt r
       | otherwise = r
    (k1, r2) | odd k = ((k - 1) `div` 2, r1)
             | otherwise = (k `div` 2-1, r1+r1)
    r3 = scaleFloat k1 (sqrt r2)
    (w,n) = (r3,y)
    (w1,n1) | r3 == 0 = (w,n)
            | otherwise = let n2 = if not (isInfinite n) then (n/r3)/2
                                                         else n
                          in if x < 0 then (abs n2, copySign r3 y)
                                      else (w, n2)
-}



cSqrt :: RealFloat a => Complex a -> Complex a
cSqrt z@(x:+y) = w1:+n1
  where
    (r,k) = cSSqs z
    r1 | x == x = scaleFloat (-k) (abs x) + sqrt r
       | otherwise = r
    (k1, r2) | odd k = ((k - 1) `div` 2, r1)
             | otherwise = (k `div` 2-1, r1+r1)
    r3 = scaleFloat k1 (sqrt r2)
    (w,n) = (r3,y)
    (w1,n1) | r3 == 0 = (w,n)
            | otherwise = let n2 = if not (isInfinite n) then (n/r3)/2
                                                         else n
                          in if x < 0 then (abs n2, copySign r3 y)
                                      else (w, n2)

_cSquare :: forall a. RealFloat a => Complex a -> Complex a
_cSquare (w:+n) | isNaN x = if | isInfinite y -> copySign 0 w :+ y
                               | isInfinite n -> (-1/0) :+ y
                               | isInfinite w -> 1/0 :+ y
                               | otherwise    -> x :+ y
                | isNaN y && isInfinite x = x :+ copySign 0 y
                | otherwise = x:+y
  where
    x = (w-n)*(w+n)
    y = wn+wn
    wn = w*n

copySign :: RealFloat a => a -> a -> a
copySign x y | makePos   = abs x
             | otherwise = negate $ abs x
  where
    makePos | isNegativeZero y = False
            | y < 0            = False
            | otherwise        = True


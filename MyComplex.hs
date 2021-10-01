{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
This contains draft fixes to https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/Data/Complex.hs
-}

module MyComplex ((+:), (-:), (*:), (/:),
                  phase, iTimes,
                  sqrt, log, tan, asin, acos, tanh, atan, asinh, acosh, atanh)
where

import           Prelude       hiding (sqrt, log, tan, asin, acos, atan, tanh, asinh, acosh, atanh)
import qualified Prelude       as P   (sqrt, log, tan, atan)

import           Data.Complex  hiding (phase)
import           Numeric              (log1p)

import qualified MyFloat       as F   (asinh)
import           MyFloat              (copySign)


{-
Note:

  (3 :+ (-0.0)) +: 1 == 4 :+ (-0.0)

whereas

  (3 :+ (-0.0)) +  1 == 4 :+   0.0
-}

(+:), (-:), (*:), (/:) :: RealFloat a => Complex a -> a -> Complex a

infixl 6 +:
(x:+y) +: z    =  (x+z) :+  y

infixl 6 -:
(x:+y) -: z    =  (x-z) :+  y

infixl 7 *:
(x:+y) *: z   =  (x*z) :+ (y*z)

infixl 7 /:
(x:+y) /: z   =  x/z :+ y/z


{- PHASE
Same as Prelude, but without zero special case that messes up negative zeros)
-}

phase :: RealFloat a => Complex a -> a
phase (x:+y) = atan2 y x

sqrt, log, tan, asin, acos, atan, tanh, asinh, acosh, atanh :: RealFloat a => Complex a -> Complex a

{- SQRT
This is not Kahan's formula.
It is a tweak from Prelude, fixing for neg zeros.
-}

sqrt z@(0:+0)  =  z --consistancy with sqrt(-0). Ensure correct +/-0 for imag.
sqrt z@(x:+y)  =  u :+ (if isNeg y then -v else v)
  where (u,v)   = if isNeg x then (v',u') else (u',v')
        v'      = abs y / (u'*2)
        u'      = P.sqrt ((magnitude z + abs x) / 2)
        isNeg r = isNegativeZero r || r < 0

{- LOG
This is not Kahan's formula.
This is the same as Prelude, but using fixed phase, hence:
              P.log (0 :+ (-0)) = (-Infinity) :+   0.0
but we have     log (0 :+ (-0)) = (-Infinity) :+ (-0.0)
-}

log z = P.log (magnitude z) :+ phase z

tan z = -iTimes(tanh(iTimes z))

{-
ASIN
-}

asin z@(x:+_) = P.atan(x/r) :+ F.asinh s
  where
    r = realPart (sqrt(-z+:1)*sqrt(z+:1))
    s = imagPart (sqrt(conjugate(-z+:1))*sqrt(z+:1))

{- asin conjugate
> asin $ (-1) :+ 0.00001
(-1.567634051769965) :+ 3.162280295393111e-3
> asin $ (-1) :+ 0
(-1.5707963267948966) :+ (-0.0) --IDEALLY WOULD BE +ve 0.0. The formula above can't deliver this.
> asin $ (-1) :+ (-0)
(-1.5707963267948966) :+ (-0.0)
> asin $ (-1) :+ (-0.00001)
(-1.567634051769965) :+ (-3.162280295393183e-3)
-}

acos z =  2 * P.atan(realPart(sqrt(-z+:1))/realPart(sqrt(z+:1)))
       :+ F.asinh (imagPart(sqrt(conjugate(z+:1))*sqrt(-z+:1)))

atan z = -iTimes(atanh(iTimes z))

tanh (z:+n) | z > F.asinh om / 4 = copySign 1 z :+ copySign 0 n --in http://cpp.sh/ asinh(1.7976931348623157e308) = 710.475860, not Infinity
            | isInfinite t = p/s :+ 1/t
            | otherwise = (b*p*s :+ t) /: (1+b*s*s)
  where
    t = P.tan n
    b = 1 + t*t
    s = sinh z
    p = P.sqrt(1+s*s)
    om = maxNonInfiniteFloat

asinh z = -iTimes(asin(iTimes z))

acosh z = F.asinh(realPart(conjugate(sqrt(z-:1)) * sqrt(z+:1)))
        :+ 2 * P.atan (imagPart (sqrt (z-:1)) / realPart (sqrt (z+:1)))

iTimes :: RealFloat a => Complex a -> Complex a
iTimes (x:+y) = (-y) :+ x

atanh (1:+y@0) = 1/0 :+ y
atanh ((-1):+y@0) = (-1/0) :+ y
atanh z@(x:+y) | x > th || abs y > th = realPart(1/z) :+ copySign (pi/2) y
               | x == 1               = P.log(P.sqrt(P.sqrt(4+y*y))/P.sqrt(abs y + rh)) :+ copySign (pi/2 + P.atan((abs y + rh)/2)) y / 2
               | otherwise            = ln1p(4*x/(sq(1-x)+sq(abs y + rh)))/4 :+ phase(((1-x)*(1+x) - sq(abs y + rh)) :+ 2*y)/2
  where
    --b = copySign 1 x
    th = P.sqrt maxNonInfiniteFloat / 4
    rh = 1 / th
    ln1p = log1p

--attempt a more simple formula: ("corrected" from initial Complex.hs):
--atanh z        =  0.5 * (log (z+:1) - log ((-z)+:1))
--doesn't (and can't) give correct answer for (-0) :+ 2. (should be (-0):+1.10).
--since log (z+:1) = 0.804:+1.1,  log ((-z)+:1)) = 0.804:+(-1.1), and the diff can't have real -0.0.

sq :: Num a => a -> a
sq x = x * x

{-
z                              P.atanh z                                          atanh z
(-9247365704582822.0) :+ (-3)  (-2.2204460492503136e-16) :+ (-1.5707963267948966) (-2.220446049250313e-16) :+ (-1.5707963267948966)
(-9247365704582823.0) :+ (-3)    0.0                     :+   1.5707963267948966    0.0                    :+ (-1.5707963267948966)
-}

maxNonInfiniteFloat :: forall a. RealFloat a => a
maxNonInfiniteFloat = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e
    a = undefined :: a

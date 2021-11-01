{-# OPTIONS -Wall #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Complex
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Complex numbers.
--
-----------------------------------------------------------------------------

module MyComplex
        (
        -- * Rectangular form
          Complex((:+))

        , realPart
        , imagPart
        -- * Polar form
        , mkPolar
        , cis
        , polar
        , magnitude
        , phase
        -- * Other operations
        , conjugate
        , iTimes
        -- * Operations with complex and real numbers
        -- $ComplexRealOps
        , (+:), (-:), (*:), (/:)

        -- * Branch Cuts and Principal Values
        -- $BranchCuts

        -- ** Explanation
        -- $BranchCutsExp
        )  where

import qualified MyFloat as F

import GHC.Base (Applicative (..))
import GHC.Generics (Generic, Generic1)
import GHC.Float (Floating(..))
import Data.Data (Data)
import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
                alignment)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))

infix  6  :+

-- -----------------------------------------------------------------------------
-- The Complex type

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@
-- has the phase of @z@, but unit magnitude.
--
-- The 'Foldable' and 'Traversable' instances traverse the real part first.
--
-- Note that `Complex`'s instances inherit the deficiencies from the type
-- parameter's. For example, @Complex Float@'s 'Ord' instance has similar
-- problems to `Float`'s.
--
-- A number of functions have [branch cuts and principal values](#BranchCuts).
data Complex a
  = !a :+ !a    -- ^ forms a complex number from its real and imaginary
                -- rectangular components.
        deriving ( Eq          -- ^ @since 2.01
                 , Show        -- ^ @since 2.01
                 , Read        -- ^ @since 2.01
                 , Data        -- ^ @since 2.01
                 , Generic     -- ^ @since 4.9.0.0
                 , Generic1    -- ^ @since 4.9.0.0
                 , Functor     -- ^ @since 4.9.0.0
                 , Foldable    -- ^ @since 4.9.0.0
                 , Traversable -- ^ @since 4.9.0.0
                 )

-- -----------------------------------------------------------------------------
-- Functions over Complex

-- | Extracts the real part of a complex number.
realPart :: Complex a -> a
realPart (x :+ _) =  x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (_ :+ y) =  y

-- | The conjugate of a complex number.
{-# SPECIALISE conjugate :: Complex Double -> Complex Double #-}
conjugate        :: Num a => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)

-- | @iTimes z == z * (0:+1)@, but without any rounding issues or loss of negative zero. E.g.
-- @iTimes((-0):+0) == (-0):+(-0)@
-- whereas
-- @((-0):+0)*(0:+1) == (-0):+0@
iTimes :: (RealFloat a) => Complex a -> Complex a
iTimes (x:+y) = (-y) :+ x

-- | Form a complex number from polar components of magnitude and phase.
{-# SPECIALISE mkPolar :: Double -> Double -> Complex Double #-}
mkPolar          :: Floating a => a -> a -> Complex a
mkPolar r theta  =  r * cos theta :+ r * sin theta

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
{-# SPECIALISE cis :: Double -> Complex Double #-}
cis              :: Floating a => a -> Complex a
cis theta        =  cos theta :+ sin theta

-- | The function 'polar' takes a complex number and
-- returns a (magnitude, phase) pair:
-- the magnitude is nonnegative, and the phase in the range @[-'pi', 'pi']@.
{-# SPECIALISE polar :: Complex Double -> (Double,Double) #-}
polar            :: (RealFloat a) => Complex a -> (a,a)
polar z          =  (magnitude z, phase z)

-- | The nonnegative magnitude of a complex number.
-- See Note [magnitude implementation]
{-# SPECIALISE magnitude :: Complex Double -> Double #-}
magnitude :: (RealFloat a) => Complex a -> a
magnitude z =  scaleFloat k m where (m,k) = magScale z


-- | The phase of a complex number, in the range @[-'pi', 'pi']@.
-- If the number has a negative real part, and +/- 0 imaginary part
-- the result will be +/- pi.
-- The phase of 0:+0 is 0; of 0:+(-0) is -0; of (-0):+0 is pi; of (-0):+(-0) is -pi.
{-# SPECIALISE phase :: Complex Double -> Double #-}
phase :: (RealFloat a) => Complex a -> a
-- ensure phase ((-0):+(-0)) is -pi, etc.
-- this also ensures log (0 :+ (-0)) is (-Infinity) :+ (-0.0)
phase (x:+y)     = atan2 y x

-- $ComplexRealOps
-- These operators combine complex and real numbers, without converting the real numbers to complex first. E.g.
-- @(3:+(-0)) +: 1 == 4:+(-0)@
-- whereas
-- @(3:+(-0)) + 1 == (3:+(-0)) + (1:+0) == 4:+0@

(+:), (-:), (*:), (/:) :: RealFloat a => Complex a -> a -> Complex a

(x:+y) +: r  =  (x+r) :+  y
(x:+y) -: r  =  (x-r) :+  y
(x:+y) *: r  =   x*r  :+  y*r
(x:+y) /: r  =   x/r  :+  y/r

infixl 6 +:, -:
infixl 7 *:, /:

-- -----------------------------------------------------------------------------
-- Instances of Complex

-- | @since 2.01
instance  (RealFloat a) => Num (Complex a)  where
    {-# SPECIALISE instance Num (Complex Float) #-}
    {-# SPECIALISE instance Num (Complex Double) #-}
    (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
    (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
    (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)       =  negate x :+ negate y
    abs z               =  magnitude z :+ 0

    signum (x@0:+y)     = x :+ signum y --including 0:+NaN
    signum (x:+y@0)     = signum x :+ y --ditto NaN:+0
    signum z@(x:+y)
      | otherwise       = case (isInfinite x, isInfinite y) of
                            (True,  True ) -> signum' $ (copySign 1 x) :+ (copySign 1 y)
                            (True,  False) -> --y not zero or infinite, but might be NaN
                                              signum' $ (copySign y x) :+ (copySign 0 y)
                            (False, True ) -> --ditto x
                                              signum' $ (copySign 0 x) :+ (copySign x y)
                            (False, False) -> signum' z
      where
        signum' w@(u:+v)     = scaleFloat (-k) u / m
                            :+ scaleFloat (-k) v / m
          where (m,k) = magScale w

    fromInteger n       =  fromInteger n :+ 0

-- | @since 2.01
instance  (RealFloat a) => Fractional (Complex a)  where
    {-# SPECIALISE instance Fractional (Complex Float) #-}
    {-# SPECIALISE instance Fractional (Complex Double) #-}
    (x:+y) / (x':+y')   =  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
                           where x'' = scaleFloat k x'
                                 y'' = scaleFloat k y'
                                 k   = - exponent (max (abs x') (abs y'))
                                 --not max (exponent x') (exponent y')
                                 --which can fail when x' or y' is 0
                                 d   = x'*x'' + y'*y''

    fromRational a      =  fromRational a :+ 0

-- | @since 2.01
instance  (RealFloat a) => Floating (Complex a) where
    {-# SPECIALISE instance Floating (Complex Float) #-}
    {-# SPECIALISE instance Floating (Complex Double) #-}
    pi             =  pi :+ 0

    exp (x:+y)     =  expx * cos y :+ expx * sin y
                      where expx = exp x

    -- See Note [log implementation]
    log z          = (k' * log r + log m) :+ phase z  --see note on phase
                      where (m,k) = magScale z
                            k' = fromIntegral k
                            r  = fromIntegral $ floatRadix m

    x ** y = case (x,y) of
      (_ , (0:+0))  -> 1 :+ 0
      ((0:+0), (exp_re:+_)) -> case compare exp_re 0 of
                 GT -> 0 :+ 0
                 LT -> inf :+ 0
                 EQ -> nan :+ nan
      ((re:+im), (exp_re:+_))
        | (isInfinite re || isInfinite im) -> case compare exp_re 0 of
                 GT -> inf :+ 0
                 LT -> 0 :+ 0
                 EQ -> nan :+ nan
        | otherwise -> exp (log x * y)
      where
        inf = 1/0
        nan = 0/0

    -- See Note [sqrt implementation]
    -- sqrt((-0):+0) is unlike sqrt(-0) which has slighly odd IEEE 754 requirement to be (-0).
    sqrt (0:+y@0)                 =      0 :+          y
    sqrt z@(x:+y) | isInfinite y  =  abs y :+          y
                  | x >= 0        =      r :+          s
                  | otherwise     =  abs s :+ copySign r y
      where
        r | isInfinite x      = 1/0
          | floatRadix x /= 2 = sqrt(scaleFloat k q / 2)
          -- other radix unlikely. We'll still get overflow for sqrt(huge :+ huge),
          -- but we can't used the even/odd logic below
          | even k            = scaleFloat (k `div` 2 - 1) (sqrt (q+q))
          | otherwise         = scaleFloat ((k-1) `div` 2) (sqrt q)
          where
            (m,k) = magScale z
            q = m + scaleFloat (-k) (abs x)
        s = y/r/2

    --all of these have the same 0 assumption as exp
    sin  (x  :+y  )  =  sin x * cosh y :+    cos x * sinh y
    cos  (x  :+y  )  =  cos x * cosh y :+ (- sin x * sinh y)
    sinh (x  :+y  )  =  cos y * sinh x :+    sin y * cosh x
    cosh (x  :+y  )  =  cos y * cosh x :+    sin y * sinh x

    -- See Note [Kahan implementations] for a number of the following functions.
    tan z          = -iTimes(tanh(iTimes z))
    tanh (x:+y)    | abs(x) > cutover = copySign 1 x :+ copySign 0 y
                   | isInfinite t = p/s :+ 1/t
                   | otherwise = (b*p*s :+ t) /: (1+b*s*s)
                      where t = tan y
                            b = 1 + t*t
                            s = sinh x
                            p = sqrt(1+s*s)
                            cutover = F.asinh maxNonInfiniteFloat / 4 --NB fails in Windows, at time of writing

    -- See Note [asin, acos, acosh implementation]
    asin z@(x:+_)  =  atan(x/realPart (sqrt(-z+:1)*sqrt(z+:1)))
                   :+ asinImag z

    acos z         =  2 * atan(realPart(sqrt(-z+:1))/realPart(sqrt(z+:1)))
                   :+ asinImag (-z)

    acosh z        =  acoshReal z
                   :+ 2 * atan (imagPart(sqrt(z-:1)) / realPart(sqrt(z+:1)))

    atan z         =  -iTimes(atanh(iTimes z))

    asinh z        =  -iTimes(asin(iTimes z))

    atanh w@(u:+_) = conjugate(atanh'(conjugate w *: b)) *: b
      where
      b = copySign 1 u
      atanh' (1:+y@0) = 1/0 :+ y  --although excluded from domain, make it match real fn (Infinity +/-0i)
      atanh' z@(x:+y) | x > th || abs y > th =  realPart(1/z)
                                             :+ copySign (pi/2) y
                      | x == 1               =  log(sqrt(sqrt(4+y*y))/sqrt(abs y))
                                             :+ copySign (pi/2 + atan((abs y)/2)) y / 2
                      | otherwise            =  log1p(4*x/(sqr(1-x)+sqr(abs y)))/4
                                             :+ phase(((1-x)*(1+x) - sqr(abs y)) :+ 2*y)/2
      th = sqrt maxNonInfiniteFloat / 4

    log1p x@(a :+ b)
      | abs a < 0.5 && abs b < 0.5
      , u <- 2*a + a*a + b*b = log1p (u/(1 + sqrt(u+1))) :+ atan2 (1 + a) b
      | otherwise = log (1 + x)
    {-# INLINE log1p #-}

    expm1 x@(a :+ b)
      | a*a + b*b < 1
      , u <- expm1 a
      , v <- sin (b/2)
      , w <- -2*v*v = (u*w + u + w) :+ (u+1)*sin b
      | otherwise = exp x - 1
    {-# INLINE expm1 #-}

asinImag, acoshReal :: RealFloat a => Complex a -> a

--NB, conjugate(sqrt w)) /= sqrt(conjugate w) when w doesn't support -0.0: (e.g. (-1):+0).
--(Kahan's formula sqrt(z-1)* is ambiguous, but like this works correctly for IEEE754 and not floats).
asinImag z@(_:+y) = maybe (F.asinh $ imagPart $ conjugate (sqrt(-z+:1)) * sqrt (z+:1))
                          (`copySign` y)
                          (asinhQ z)

acoshReal z       = maybe (F.asinh $ realPart $ conjugate (sqrt(z-:1)) * sqrt (z+:1))
                          id
                          (asinhQ z)

asinhQ :: RealFloat a => Complex a -> Maybe a
asinhQ z@(x:+_) | abs x > 1 && x+1 == x  =  Just (log 2 + log m + k*log b)
                | otherwise              =  Nothing
  where (m,k')  =  magScale z
        k       =  fromIntegral k'
        b       =  fromIntegral $ floatRadix x

{-
Note [Kahan implementations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A simple implementation of the functions based on their mathematical formulae
will often result in incorrect branch cut location (especially when dealing
with -0) and poor precision,due to intermediate rounding / cancellation / etc.

The document "Branch Cuts for Complex Elementary Functions
or Much Ado About Nothing's Sign Bit" by W. Kahan
(currently available at https://people.freebsd.org/~das/kahan86branch.pdf)
explains the rational behind the branch cut locations when -0 is allowed.

Kahan also provides procedures (section 9) to implement these the elementary functions
in a computationally accurate way and with correct branch cuts.
Some of the functions here are based on these.

Not all functions are based on Kahan's procedures, since
some seem to work correctly already.


Note on [magnitude implementation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  magnitude z@(x:+y)
  = sqrt (x^2 + y^2)                       but may overflow, even for reasonable result.
  = b^(k) * sqrt (x^2 + y^2) * b^(-k)      where b is the base (radix), for some suitable k
  = b^(k) * sqrt ((x^2 + y^2) * (b^(-k))^2)
  = b^(k) * sqrt (x^2*(b^(-k))^2 + y^2*(b^(-k))^2)
  = b^(k) * sqrt ((x*b^(-k))^2 + (y*b^(-k))^2)
  = b^(k) * m
    where (m,k) = magScale z


Note on [log implementation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  log z = log (magnitude z) :+ phase z

    log (magnitude z)
  = log (b^(k) * m)
  = log (b^(k)) + log  m
  = k * log b + log m
    where (m,k) = magScale z

  Kahan's logic "if k=0 and T0 < beta and (beta <=T1 or rho<T2)" doesn't make sense to me:
    k = floor(logBase 2 (max (abs x) (abs y))) = 0
    => 0 <= logBase 2 (max (abs x) (abs y)) < 1
    => 1 <= max (abs x) (abs y) < 2
    => 1 <= beta < 2
        => T0 < beta
    => rho = sqrt (x^2 + y^2) < sqrt 8 < T2
    Hence the condition seems to be eqivalent to just k=0.

  Presumably, since we then use log1p((beta-1)(beta+1)+theta^2)/2
  we want (beta-1)(beta+1)+theta^2 close to zero (else we'd expect no benefit from log1p
    z@(x:+y) = 1.0000000001:+0 gives this:
    beta = 1.0000000001, th = 0, (beta-1)(beta+1)+theta^2 ~= 1e-10
    but we then get ~5e-11 as result, which is less accurate than the
      log rho / 2 + (fromIntegral k + j) * log 2
    formula.


Note [sqrt implementation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Per https://en.wikipedia.org/wiki/Square_root#Algebraic_formula

  sqrt z@(x:+y) = u :+ copySign v y where

            [ sqrt(x^2+y^2) + x]
    u = sqrt[ -----------------]
            [        2         ]

            [ sqrt(x^2+y^2) - x]
    v = sqrt[ -----------------]
            [        2         ]

    Notes:

    1. u * v = abs y/2

                  [ sqrt(x^2+y^2) + abs x]
    2. if r = sqrt[ ---------------------]
                  [           2          ]

       then r == u, when x >= 0
            r == v, when x <= 0

    3. We can scale to avoid some under/over flow.

       let q = [sqrt(x^2+y^2) + abs x]*b^(-k), where b=floatRadix, for some suitable scaling k
             = sqrt(x^2+y^2)*b^(-k) + (abs x)*b^(-k)
             = m + (abs x)*b^(-k)
               where (m,k) = magScale z
       then r = sqrt(q*b^k/2)

       if b == 2, we can get avoid more under/over flow by also doing:

       if k is even, let k = 2*j
            r = sqrt(q*2^(2*j)/2)
              = sqrt(q/2)  * sqrt(2^(2*j))
              = sqrt(2q/4) * sqrt((2^j)^2)
              = sqrt(2q)/2 * 2^j
              = sqrt(2q)   * 2^(j-1)
              = sqrt(2q)   * 2^(k `div` 2 - 1)

       if k is odd, let k = 2*j + 1
            r = sqrt(q*2^(2*j+1)/2)
              = sqrt(q*2^(2*j))
              = sqrt q * sqrt((2^j)^2)
              = sqrt q * 2^j
              = sqrt q * 2^((k-1) `div` 2)

Note [asin, acos, acosh implementation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  I can't (yet) derive Kahan's formulae (strangely given as comments)
  from the logrithmic definitions.
  But they seem to me to be correct, but are vulnerable to intermediate overflow.
  To solve the overflow:

  For large enough x (i.e. abs x > lim):
    x+1 == x
    sqrt(x^2+y^2) > abs x > lim

  Note: Re(sqrt(z*conjugate z))  = Re(sqrt((x+iy)(x-iy)))
                                 = Re(sqrt(x^2+y^2) :+ 0)
                                 = sqrt(x^2+y^2)
                                 = magnitude z
        Im(sqrt(-z*conjugate z)) = Im(sqrt(-x^2+y^2) :+ 0)
                                 = Im(-i * sqrt(x&2+y^2) :+ 0)
                                 = -magnitude z (IEEE types)
                                 =  magnitude z (non IEEE)

  let q = imagPart(conjugate(sqrt(z+:1))*sqrt(-z+:1))
        = imagPart(conjugate(sqrt z)*sqrt(-z))
        = +/- imagPart(sqrt(conjugate z)*sqrt(-z))
        = +/- imagPart(sqrt(-z*(conjugate z)))
        = +/- magnitude z
        = +/- m*b^k  where (m,k) = magScale z

  Then
  asinh q = log(q + sqrt (q^2+1))
          = log(q + sqrt (q^2))
          = log(2 * q)
          = log 2 + log q
          = log 2 + log m + k*log b

  Similar for acos, acosh.
-}

-- | @since 4.8.0.0
instance Storable a => Storable (Complex a) where
    sizeOf a       = 2 * sizeOf (realPart a)
    alignment a    = alignment (realPart a)
    peek p           = do
                        q <- return $ castPtr p
                        r <- peek q
                        i <- peekElemOff q 1
                        return (r :+ i)
    poke p (r :+ i)  = do
                        q <-return $  (castPtr p)
                        poke q r
                        pokeElemOff q 1 i

-- | @since 4.9.0.0
instance Applicative Complex where
  pure a = a :+ a
  f :+ g <*> a :+ b = f a :+ g b
  liftA2 f (x :+ y) (a :+ b) = f x a :+ f y b

-- | @since 4.9.0.0
instance Monad Complex where
  a :+ b >>= f = realPart (f a) :+ imagPart (f b)

-- | @since 4.15.0.0
instance MonadZip Complex where
  mzipWith = liftA2

-- | @since 4.15.0.0
instance MonadFix Complex where
  mfix f = (let a :+ _ = f a in a) :+ (let _ :+ a = f a in a)

-- -----------------------------------------------------------------------------
-- Rules on Complex

{-# RULES

"realToFrac/a->Complex Double"
  realToFrac = \x -> realToFrac x :+ (0 :: Double)

"realToFrac/a->Complex Float"
  realToFrac = \x -> realToFrac x :+ (0 :: Float)

  #-}

-- -----------------------------------------------------------------------------
-- Internal functions

sqr :: RealFloat a => a -> a  --not suitable for Complex. (See Kahan CSQUARE)
sqr x = x * x

-- copySign x y returns the value of x, but with the sign of y.
-- returns NaN if x is NaN. Per IEEE requirement says, "result is undefined"
-- if y is NaN, but we'll define it to keep x.
copySign :: RealFloat a => a -> a -> a
copySign x y | isNaN y    =  x
             | isNeg y    =  negate $ abs x
             | otherwise  =  abs x
  where isNeg r  =  isNegativeZero r || r < 0

magScale :: (RealFloat a) => Complex a -> (a, Int)  --magScale z = (magnitude (z/r^k), k). See note [magnitude implementation]
magScale (x:+y) = (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)), k)
  where k = exponent (max (abs x) (abs y))  --NOT max (exponent x) (exponent y), which fails with e.g. tiny:+0.
        mk = - k

-- maxNonInfiniteFloat is the largest non-infinite floating point value.
maxNonInfiniteFloat :: forall a. RealFloat a => a
maxNonInfiniteFloat = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e
    a = undefined :: a

-- $BranchCuts
-- #BranchCuts#
--
-- The "inverse" complex functions have branch cuts and principal values in ranges as follows:
--
-- +--------------------+----------------------------------------------------------+-----------------------------------------------+
-- + (inverse) function + branch cut(s)                                            + range x or (x:+y) where                       +
-- +====================+==========================================================+===============================================+
-- + 'phase'            + @[-&#x221E;, -0]@                                        + -&#x03C0; <= x <= &#x03C0;                    +
-- +--------------------+----------------------------------------------------------+-----------------------------------------------+
-- + 'sqrt'             + @[-&#x221E;, -0)@                                        + x >= +0.0                                     +
-- +--------------------+----------------------------------------------------------+-----------------------------------------------+
-- + 'log'              + @[-&#x221E;, -0]@                                        + -&#x03C0; <= y <= &#x03C0;                    +
-- +--------------------+----------------------------------------------------------+-----------------------------------------------+
-- + 'asin'             + @[-&#x221E;, -1)@ and @(1, &#x221E;]@                    + -&#x03C0;\/2 <= x <= &#x03C0;\/2              +
-- +--------------------+----------------------------------------------------------+-----------------------------------------------+
-- + 'acos'             + @[-&#x221E;, -1)@ and @(1, &#x221E;]@                    + +0.0 <= x <= &#x03C0;                         +
-- +--------------------+----------------------------------------------------------+-----------------------------------------------+
-- + 'atan'             + @[-&#x221E;/i/, -/i/)@ and @(/i/, &#x221E;/i/]@          + -&#x03C0;\/2 <= x <= &#x03C0;\/2              +
-- +--------------------+----------------------------------------------------------+-----------------------------------------------+
-- + 'asinh'            + @[-&#x221E;/i/, -/i/)@ and @(/i/, &#x221E;/i/]@          + -&#x03C0;\/2 <= y <= &#x03C0;\/2              +
-- +--------------------+----------------------------------------------------------+-----------------------------------------------+
-- + 'acosh'            + @[-&#x221E;, 1)@                                         + x >= +0.0 and -&#x03C0; <= y <= &#x03C0;      +
-- +--------------------+----------------------------------------------------------+-----------------------------------------------+
-- + 'atanh'            + @[-&#x221E;, -1)@ and @(1, &#x221E;]@                    + -&#x03C0;\/2 <= y <=  &#x03C0;\/2             +
-- +--------------------+----------------------------------------------------------+-----------------------------------------------+
--
-- Note that 'phase' maps complex numbers to real numbers
-- and the other functions map complex numbers to complex numbers.
-- All of the branch cuts fall on axes.
--
-- In this table, the expression @x >= +0.0@ and similar excludes @x@ where @isNegativeZero x@ is @True@.
--
-- The behaviour of types @Complex a@ for a value on a branch cut is different,
-- depending on whether floating type @a@ supports signed zeros.
--
-- For types that support signed zeros (i.e. IEEE 754 types),
-- the branch cuts can be considered to be "between" @0.0@ and @-0.0@.
-- The mappings of @0.0@ will be continuous with those of values greater than @0.0@, and
-- the mappings of @-0.0@ will be continuous with those of values less than @0.0@.
--
-- For types that don't support signed zeros, the mappings of
-- points on a branch cut are continuous with mappings of points in one of the adjacent quadrants
-- as indicated in the table below.
-- In addition, the range of the functions excludes certain values, also indicated in the table.
--
-- +--------------------+-----------------------------------+-----------------------------------------------------------------------------------------+
-- + (inverse) function + branch cut(s) continuous with     + range excludes                                                                          +
-- +====================+===================================+=========================================================================================+
-- + 'phase'            + QII                               + -&#x03C0;                                                                               +
-- +--------------------+-----------------------------------+-----------------------------------------------------------------------------------------+
-- + 'sqrt'             + QII                               + (0, -&#x221E;/i/]                                                                       +
-- +--------------------+-----------------------------------+-----------------------------------------------------------------------------------------+
-- + 'log'              + QII                               + [-&#x221E;-&#x03C0;/i/, &#x221E;-&#x03C0;/i/]                                           +
-- +--------------------+-----------------------------------+-----------------------------------------------------------------------------------------+
-- + 'asin'             + QII and QIV                       + (-&#x03C0;\/2, -&#x03C0;\/2-&#x221E;/i/], (&#x03C0;\/2, &#x03C0;\/2+&#x221E;/i/]        +
-- +--------------------+-----------------------------------+-----------------------------------------------------------------------------------------+
-- + 'acos'             + QII and QIV                       + (0, -&#x221E;/i/], (&#x03C0;, &#x03C0;+&#x221E;/i/]                                     +
-- +--------------------+-----------------------------------+-----------------------------------------------------------------------------------------+
-- + 'atan'             + QIII and QI                       + [-&#x03C0;\/2, -&#x03C0;\/2+&#x221E;/i/], [&#x03C0;\/2, &#x03C0;\/2-&#x221E;/i/]        +
-- +--------------------+-----------------------------------+-----------------------------------------------------------------------------------------+
-- + 'asinh'            + QIII and QI                       + (-&#x03C0;\/2/i/, &#x221E;-&#x03C0;\/2/i/], (&#x03C0;\/2/i/, -&#x221E;+&#x03C0;\/2/i/]  +
-- +--------------------+-----------------------------------+-----------------------------------------------------------------------------------------+
-- + 'acosh'            + QII (x\<0), QI (x\>0)             + (0, -&#x03C0;/i/], [-&#x03C0;/i/, &#x221E;-&#x03C0;/i/]                                 +
-- +--------------------+-----------------------------------+-----------------------------------------------------------------------------------------+
-- + 'atanh'            + QII and QIV                       + [-&#x03C0;\/2/i/, -&#x221E;-&#x03C0;\/2/i/], [&#x03C0;\/2/i/, &#x221E;+&#x03C0;\/2/i/]  +
-- +--------------------+-----------------------------------+-----------------------------------------------------------------------------------------+
--
-- where the quadrants are labelled:
--
-- +--------------------+---------------+---------------+
-- +                    + negative real + positive real +
-- +--------------------+---------------+---------------+
-- + positive imaginary + QII           + QI            +
-- +--------------------+---------------+---------------+
-- + negative imaginary + QIII          + QIV           +
-- +--------------------+---------------+---------------+
--
-- Some (inverse) functions also have branch points as follows:
--
-- +--------------------+-----------------+-------------------------+
-- + (inverse) function + branch point    + value at branch point   +
-- +====================+=================+=========================+
-- + 'log'              + @0.0 :+ 0.0@    + @(-Infinity) :+ 0.0@    +
-- +--------------------+-----------------+-------------------------+
-- + 'atan'             + @0.0 :+ 1.0@    + @0.0 :+ Infinity@       +
-- +--------------------+-----------------+-------------------------+
-- + 'atan'             + @0.0 :+ (-1.0)@ + @0.0 :+ (-Infinity)@    +
-- +--------------------+-----------------+-------------------------+
-- + 'atanh'            + @1.0 :+ 0.0@    + @Infinity :+ 0.0@       +
-- +--------------------+-----------------+-------------------------+
-- + 'atanh'            + @(-1.0) :+ 0.0@ + @(-Infinity) :+ 0.0@    +
-- +--------------------+-----------------+-------------------------+
--
-- For 'log', branch points with a negative zero real part are included in
-- the branch cut, giving different results for
-- branch points with positive and negative zero in the imaginary part.
-- For other branch points, a negative zero input will be reflected in the output.
--
-- Haskell's branch cuts, continuities, ranges and branch points follow the recommendations by Kahan
-- (Branch Cuts for Complex Elementary Functions or Much Ado About Nothing's Sign Bit, 1987),
-- and are consitant with those in some other languages such as Common Lisp.
--
-- $BranchCutsExp
--
-- ===Branch Cuts
--
-- This is an explanation of the branch cuts for the function 'sqrt'.
-- The same principles apply to the other functions.
--
-- Consider 'sqrt' of real numbers.
-- Both @2.0^2@ and @(-2.0)^2@ have the value @4.0@,
-- but @sqrt 4.0@ returns only @2.0@.
-- In general 'sqrt' returns only the non-negative square root,
-- a standard widely adopted.
-- In other words the range of 'sqrt', over real numbers, is [0, &#x221E;]:
-- half of the real number line.
--
-- @sqrt@ over complex numbers has analagous behaviour.
-- The square of both @1:+2@ and its negative @(-1):+(-2)@ is the same value @(-3):+4@.
-- 'sqrt' maps @(-3):+4@ back to only one of these: @1:+2@,
-- the /principal value/.
-- The standard principal values for @sqrt@ are those @x:+y@ where @x >= 0@:
-- half of the complex plane.
--
-- The table below shows the square of two sequences of complex numbers:
--
-- +-------------------------+------------------------+--------------------------+
-- +    @v@                  +    @w = -v@            + @z = v^2@ (@== w^2@)     +
-- +=========================+========================+==========================+
-- +  __@( 0.050):+2.0@__    +   @(-0.050):+(-2.0)@   + @(-4):+( 0.2)@           +
-- +-------------------------+------------------------+--------------------------+
-- +  __@( 0.025):+2.0@__    +   @(-0.025):+(-2.0)@   + @(-4):+( 0.1)@           +
-- +-------------------------+------------------------+--------------------------+
-- +    @( 0.000):+2.0@      +   @( 0.000):+(-2.0)@   + @(-4):+( 0.0)@           +
-- +-------------------------+------------------------+--------------------------+
-- +    @(-0.025):+2.0@      + __@( 0.025):+(-2.0)@__ + @(-4):+(-0.1)@           +
-- +-------------------------+------------------------+--------------------------+
-- +    @(-0.050):+2.0@      + __@( 0.050):+(-2.0)@__ + @(-4):+(-0.2)@           +
-- +-------------------------+------------------------+--------------------------+
--
-- Each corresponding @v@ and @w@ has the same square @z@.
-- @sqrt@ maps each of these @z@ values back to the principal value
-- (whichever of @v@ or @w@ has a non-negative real part, as highlighted in bold).
-- Hence, although the sequence @z@ is continuous, @sqrt z@ is discontinuous,
-- with the results jumping from a imaginary @2.0@ to imaginary @-2.0@
-- as the imaginary part of @z@ crosses the real axis.
--
-- The discontinuity in 'sqrt' illustrated here happens at -4 on the real axis.
-- There would be similar discontinuities at all other points on the negative real axis.
-- Hence the negative real axis @[-&#x221E;, -0)@ forms a line of discontinuity, called a /branch cut/.
-- The location of the branch cuts and the range of the function are related.
--
-- In the case of @sqrt $ (-4):+( 0.0)@, either @v@ or @w@ could be chosen, but:
--
-- * When dealing with floating point numbers that support negative zeros,
-- the result is chosen so that
-- @sqrt $ (-4):+( 0.0)@ is continuous with points @sqrt $ (-4):+y@ where @y>0@, hence @( 0.000):+2.0@, and
-- @sqrt $ (-4):+(-0.0)@ is continuous with points @sqrt $ (-4):+y@ where @y<0@, hence @( 0.000):(-2.0)@.
-- It is helpful to think of the branch cut as being "between" @0.0@ and @-0.0@.
--
-- * When dealing with floating point numbers that don't support negative zeros, an
-- arbitrary but standard choice is made.
-- The standard choice in this case is that @sqrt $ (-4):+( 0.0)@ is @( 0.000):+2.0@.
-- Hence 'sqrt' of points on the branch cut are continuous with 'sqrt' of points in QII on the complex plane.
-- In addition, 'sqrt' will not map anything to @( 0.000):+(-2.0)@, or any other point
-- on the negative imaginary axis (0, -&#x221E;/i/], which is excluded from the range.
--
--     Note that "continous with" doesn't mean "maps into" or "maps towards".
--     For example, the first branch cut listed for @cos@ is marked as "continuous with QII".
--     Hence both @(-2) :+ 0@ (a point on this branch cut) and
--     @(-2) :+ 0.1@ (a nearby point in QII) are both mapped by @cos@ to locations near each other,
--     but in QIV.
--
-- The result for (unsigned) @0.0@ (for types that don't support signed zeros)
-- is not necessarily the same as that for "positive" @0.0@ (for those that types that do).
-- For example:
--
-- >>> asin $ 2.0 :+ 0.0 :: Complex SomeNonIEEEFloatingType
-- 1.570 :+ (-1.316)
-- >>> asin $ 2.0 :+ 0.0 :: Complex SomeIEEEFloatingType
-- 1.570 :+ 1.316
--
-- ===Branch Points
--
-- This is an explanation of the branch point for the function 'log'.
-- The same principles apply to the other functions.
--
-- Mathematically \(e^x\to0\) as \(x\to-\infty\) and
-- \(\log 0\) is undefined.
-- Computationally it is usually helpful
-- (and is required by IEEE 754) to give @-Infinity@:
--
-- >>> log 0.0
-- -Infinity
--
-- For complex numbers, @exp (x:+y) == mkPolar (exp x) y@,
-- i.e. a point distance @exp x@ from the origin, at angle @y@.
-- As @x@ tends to @-Infinity@, this point tends to @0.0 :+ 0.0@,
-- but from different angles depending on @y@.
-- Hence @exp $ (-1/0.0) :+ y@ gives @0.0 :+ 0.0@ for any @y@.
--
-- The inverse function 'log' could map @0.0 :+ 0.0@ to /any/ @-Infinity :+ y@.
-- The point @0.0 :+ 0.0@ is called a /branch point/ of 'log'.
-- An arbirary, but standard, choice (which must be within the principle value range)
-- is made for the result, as shown in the table above:
--
-- >>> log $ 0.0 :+ 0.0
-- (-Infinity) :+ 0.0

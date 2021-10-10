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
        -- * operations with complex and real numbers
        , (+:), (-:), (*:), (/:)

        -- * Principal Values and Branch Cuts
        -- $BranchCuts
        
        -- ** Explanation
        -- $BranchCutsExp
        
        -- ** IEEE 754 (including negative zeros)
        -- $IEEE754
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
-- A number of functions have [principal values and branch cuts](#BranchCuts).
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
{-# SPECIALISE magnitude :: Complex Double -> Double #-}
magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
                     (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
                    where k  = max (exponent x) (exponent y)
                          mk = - k
                          sqr z = z * z

-- | The phase of a complex number, in the range @[-'pi', 'pi']@.
-- If the number has a negative real part, and +/- 0 imaginary part
-- the result will be +/- pi.
-- The phase of 0:+0 is 0; of 0:+(-0) is -0; of (-0):+0 is pi; of (-0):+(-0) is -pi.
{-# SPECIALISE phase :: Complex Double -> Double #-}
phase :: (RealFloat a) => Complex a -> a
-- ensure phase ((-0):+(-0)) is -pi, etc.
-- this also ensures log (0 :+ (-0)) is (-Infinity) :+ (-0.0)
phase (x:+y)     = atan2 y x


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
    signum (0:+0)       =  0
    signum z@(x:+y)     =  x/r :+ y/r  where r = magnitude z
    fromInteger n       =  fromInteger n :+ 0

-- | @since 2.01
instance  (RealFloat a) => Fractional (Complex a)  where
    {-# SPECIALISE instance Fractional (Complex Float) #-}
    {-# SPECIALISE instance Fractional (Complex Double) #-}
    (x:+y) / (x':+y')   =  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
                           where x'' = scaleFloat k x'
                                 y'' = scaleFloat k y'
                                 k   = - max (exponent x') (exponent y')
                                 d   = x'*x'' + y'*y''

    fromRational a      =  fromRational a :+ 0

-- | @since 2.01
instance  (RealFloat a) => Floating (Complex a) where
    {-# SPECIALISE instance Floating (Complex Float) #-}
    {-# SPECIALISE instance Floating (Complex Double) #-}
    pi             =  pi :+ 0
    exp (x:+y)     =  expx * cos y :+ expx * sin y
                      where expx = exp x
    log z          =  log (magnitude z) :+ phase z  --see note on phase

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

    -- Mirror sqrt(-0) == -0.0. Ensure sqrt(0:+(-0)) is 0:+(-0), etc.
    sqrt z@(0:+0)  =  z
    sqrt z@(x:+y)  =  u :+ (if neg y then -v else v)
                      where (u,v) = if neg x then (v',u') else (u',v')
                            v'    = abs y / (u'*2)
                            u'    = sqrt ((magnitude z + abs x) / 2)
                            neg r = isNegativeZero r || r < 0

    sin (x:+y)     =  sin x * cosh y :+ cos x * sinh y
    cos (x:+y)     =  cos x * cosh y :+ (- sin x * sinh y)

    -- See Note [Kahan implementations] for a number of the following functions.
    tan z          = -iTimes(tanh(iTimes z))

    sinh (x:+y)    =  cos y * sinh x :+ sin  y * cosh x
    cosh (x:+y)    =  cos y * cosh x :+ sin y * sinh x
    tanh (x:+y)    | x > cutover = copySign 1 x :+ copySign 0 y
                   | isInfinite t = p/s :+ 1/t
                   | otherwise = (b*p*s :+ t) /: (1+b*s*s)
                      where t = tan y
                            b = 1 + t*t
                            s = sinh x
                            p = sqrt(1+s*s)
                            cutover = F.asinh maxNonInfiniteFloat / 4 --NB fails in Windows, at time of writing

    asin z@(x:+_)  =  atan(x/r) :+ F.asinh s
                      where r = realPart (sqrt(-z+:1)*sqrt(z+:1))
                            s = imagPart (conjugate(sqrt(-z+:1))*sqrt(z+:1))
    acos z         =  x :+ y
                      where x = 2 * atan(realPart(sqrt(-z+:1))/realPart(sqrt(z+:1)))
                            y = F.asinh (imagPart(conjugate(sqrt(z+:1))*sqrt(-z+:1)))

    atan z         =  -iTimes(atanh(iTimes z))

    asinh z        =  -iTimes(asin(iTimes z))
    -- Take care to allow (-1)::Complex, fixing #8532
    acosh z       = x :+ y
                    where x = F.asinh(realPart(conjugate(sqrt(z-:1)) * sqrt(z+:1)))
                          y = 2 * atan (imagPart(sqrt(z-:1)) / realPart(sqrt(z+:1)))

    atanh w@(u:+_) = conjugate(atanh'(conjugate w *: b)) *: b
      where
      b = F.copySign 1 u
      atanh' (1:+y@0) = 1/0 :+ y
      atanh' z@(x:+y) | x > th || abs y > th = realPart(1/z) :+ copySign (pi/2) y
                      | x == 1               = log(sqrt(sqrt(4+y*y))/sqrt(abs y + rh)) :+ copySign (pi/2 + atan((abs y + rh)/2)) y / 2
                      | otherwise            = log1p(4*x/(sq(1-x)+sq(abs y + rh)))/4 :+ phase(((1-x)*(1+x) - sq(abs y + rh)) :+ 2*y)/2
      th = sqrt maxNonInfiniteFloat / 4
      rh = 1 / th
      sq z = z * z

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

Not all functions are based on Kahan's procedures, since:
1. Some seem to work correctly already; and
2. Some of his procedures access the floating point exception flags,
   which are not readily avaiable in Haskell.
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

-- copySign x y returns the value of x, but with the sign of y.
-- returns NaN if x is NaN. Per IEEE spec, "result is undefined" if y is NaN.
copySign :: RealFloat a => a -> a -> a
copySign x y | makePos   = abs x
             | otherwise = negate $ abs x
  where
    makePos | isNegativeZero y = False
            | y < 0            = False
            | otherwise        = True

-- maxNonInfiniteFloat is the largest non-infinite floating point value.
maxNonInfiniteFloat :: forall a. RealFloat a => a
maxNonInfiniteFloat = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e
    a = undefined :: a

-- iTimes z == z * (0:+1), but without any rounding issues.
iTimes :: (RealFloat a) => Complex a -> Complex a
iTimes (x:+y) = (-y) :+ x

-- These operators add integers to complex numbers, preserving negative zeros:
-- (3 :+ (-0.0)) +: 1 == 4 :+ (-0.0)
-- whereas:
-- (3 :+ (-0.0)) +  1 == 4 :+   0.0, since the 1 is converted to 1:+0, and (-0)+0 is 0.

(+:), (-:), (*:), (/:) :: RealFloat a => Complex a -> a -> Complex a

(x:+y) -: z  =  (x-z) :+  y
(x:+y) +: z  =  (x+z) :+  y
(x:+y) *: z  =   x*z  :+  y*z
(x:+y) /: z  =   x/z  :+  y/z

infixl 6 +:, -:
infixl 7 *:, /:

-- $BranchCuts
-- #BranchCuts#
-- 
-- The "inverse" complex functions have branch cuts and principal values in ranges as follows:
--
-- +--------------------+------------------------------------------------------+-----------------------------------------------+
-- + (inverse) function + branch cut(s)                                        + range (x:+y) where                            +
-- +====================+======================================================+===============================================+
-- + 'sqrt'             + @[-&#x221E;, -0)@                                    + x >= 0                                        +
-- +--------------------+------------------------------------------------------+-----------------------------------------------+
-- + 'log'              + @[-&#x221E;, -0)@                                    + -&#x03C0; <= y <= -&#x03C0;                   +
-- +--------------------+------------------------------------------------------+-----------------------------------------------+
-- + 'asin'             + @[-&#x221E;, -1)@ and @(1, &#x221E;]@                + -&#x03C0;\/2 <= x <= -&#x03C0;\/2             +
-- +--------------------+------------------------------------------------------+-----------------------------------------------+
-- + 'acos'             + @[-&#x221E;, -1)@ and @(1, &#x221E;]@                + 0 <= x <= -&#x03C0;                           +
-- +--------------------+------------------------------------------------------+-----------------------------------------------+
-- + 'atan'             + @[-&#x221E;/i/, -/i/)@ and @(/i/, &#x221E;/i/]@      + -&#x03C0;\/2 <= x <= -&#x03C0;\/2             +
-- +--------------------+------------------------------------------------------+-----------------------------------------------+
-- + 'asinh'            + @[-&#x221E;/i/, -/i/)@ and @(/i/, &#x221E;/i/]@      + -&#x03C0;\/2 <= y <= -&#x03C0;\/2             +
-- +--------------------+------------------------------------------------------+-----------------------------------------------+
-- + 'acosh'            + @[-&#x221E;, 1)@                                     + x >= 0 and -&#x03C0; <= y <= -&#x03C0;        +
-- +--------------------+------------------------------------------------------+-----------------------------------------------+
-- + 'atanh'            + @[-&#x221E;, -1)@ and @(1, &#x221E;]@                + -&#x03C0;\/2 <= y <= -&#x03C0;\/2             +
-- +--------------------+------------------------------------------------------+-----------------------------------------------+
--
-- The complex to real function 'phase' has branch cut @[-&#x221E;, -0)@ and range [-&#x03C0;, &#x03C0;].
--
-- Haskell's branch cuts and ranges are consitant with those in some other languages,
-- such as Common Lisp.
--
-- $BranchCutsExp
--
-- Both @(1:+2)^2@ and @((-1):+(-2))^2@ give @(-3):+4@.
-- However the "inverse" function 'sqrt' maps @(-3):+4@ back to only one of these: @1:+2@,
-- the /principal value/.
-- (This is analagous to 'sqrt' only returning the positive square root of a real number).
-- As a result, the range of these functions is only a limited subset of the complex plane.
-- ('sqrt' doesn't map anything to @(-1):+(-2)@).
-- 
-- `sqrt` also has a discontinuity, as shown here:
-- 
-- +----------------+-------------------+
-- |            @x@ | @sqrt x@ (approx) |
-- +================+===================+
-- | @(-4):+( 0.2)@ | @0:+( 2)@         |
-- +----------------+-------------------+
-- | @(-4):+( 0.1)@ | @0:+( 2)@         |
-- +----------------+-------------------+
-- | @(-4):+( 0.0)@ | @0:+( 2)@         |
-- +----------------+-------------------+
-- | @(-4):+(-0.1)@ | @0:+(-2)@         |
-- +----------------+-------------------+
-- | @(-4):+(-0.2)@ | @0:+(-2)@         |
-- +----------------+-------------------+
-- 
-- The discontinuity illustrated happens at -4 on the real axis.
-- There would be a similar discontinuity at all other points on the negative real axis
-- (as the imaginary part changes from positive to negative).
-- Hence the negative real axis forms a line of discontinuity, called a /branch cut/.
--
-- The location of the branch cuts and the range of the function are related.
-- In the case of 'sqrt', with the branch cut given above,
-- the range is those complex numbers with non-negative real parts.
--
-- $IEEE754
--
-- Haskell 'Double' and 'Float' types include IEEE 754 values, including negative zeros.
-- For these types, the branch cuts are such that @-0.0@ is continuous with other negative numbers.
-- In the case of 'sqrt', this means the branch cut is on the negative real axis,
-- "between" numbers with imaginary parts @0.0@ and @-0.0@.
-- Hence @sqrt((-4):+(-0))@ is @0:+(-2)@.
--
-- In addition, for functions that map (real or imaginary) zero to (real or imaginary) zero,
-- @0.0@ will map to @0.0@ and @-0.0@ will map to @-0.0@ (or, vice-versa, such as for 'negate').
-- Hence @sqrt(4:+0.0)@ gives @2:+0.@ and @sqrt (4:+(-0.0))@ gives @2:+(-0.0)@.

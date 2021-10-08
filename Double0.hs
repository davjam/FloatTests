{-# OPTIONS -Wall -Wpartial-fields #-}

module Double0 (D0, d0, unD0) where

import GHC.Float
import Text.Printf

newtype D0 = D0 { unD0 :: Double}
  deriving (Eq, Ord)

d0 :: Double -> D0
d0 = D0 . fix0 where
    fix0 0 = 0    --also maps -0.0 to 0.
    fix0 x = x

instance Show D0 where
  show (D0 x) = show x

instance PrintfArg D0 where
  formatArg = formatArg . unD0

instance  Enum D0  where
    succ      = d0 . succ . unD0
    pred      = d0 . pred . unD0
    toEnum    = d0 . toEnum
    fromEnum  = fromEnum . unD0
    enumFrom       (D0 x )                 = map d0 $ enumFrom       x      
    enumFromTo     (D0 x )         (D0 y)  = map d0 $ enumFromTo     x     y
    enumFromThen   (D0 x1) (D0 x2)         = map d0 $ enumFromThen   x1 x2  
    enumFromThenTo (D0 x1) (D0 x2) (D0 y)  = map d0 $ enumFromThenTo x1 x2 y

instance Num D0 where
    D0 x      + D0 y  = d0 $ x + y
    D0 x      - D0 y  = d0 $ x - y
    D0 x      * D0 y  = d0 $ x * y
    negate     (D0 x) = d0 $ negate      x
    abs        (D0 x) = d0 $ abs         x
    signum     (D0 x) = d0 $ signum      x
    fromInteger    n  = d0 $ fromInteger n

instance  Real D0  where
  toRational (D0 x) = toRational x

instance  Fractional D0  where
    D0 x / D0 y         =  d0 $ x / y
    fromRational x      =  d0 $ fromRational x
    recip (D0 x)        =  d0 $ recip x

instance  RealFrac D0  where
   properFraction (D0 x) = (n, d0 f) where (n, f) = properFraction x
   truncate       (D0 x) = truncate       x
   round          (D0 x) = round          x
   floor          (D0 x) = floor          x
   ceiling        (D0 x) = ceiling        x

instance  Floating D0  where
    pi                     =  d0 $ pi
    exp     (D0 x)         =  d0 $ exp x
    log     (D0 x)         =  d0 $ log x
    sqrt    (D0 x)         =  d0 $ sqrt x
    sin     (D0 x)         =  d0 $ sin x
    cos     (D0 x)         =  d0 $ cos x
    tan     (D0 x)         =  d0 $ tan x
    asin    (D0 x)         =  d0 $ asin x
    acos    (D0 x)         =  d0 $ acos x
    atan    (D0 x)         =  d0 $ atan x
    sinh    (D0 x)         =  d0 $ sinh x
    cosh    (D0 x)         =  d0 $ cosh x
    tanh    (D0 x)         =  d0 $ tanh x
    (**)    (D0 x) (D0 y)  =  d0 $ x ** y
    logBase (D0 x) (D0 y)  =  d0 $ logBase x y
    asinh   (D0 x)         =  d0 $ asinh x
    acosh   (D0 x)         =  d0 $ acosh x
    atanh   (D0 x)         =  d0 $ atanh x
    log1p   (D0 x)         =  d0 $ log1p x
    expm1   (D0 x)         =  d0 $ expm1 x
    log1mexp (D0 x)        =  d0 $ log1mexp x

instance  RealFloat D0  where
    floatRadix _        =  floatRadix  (0::Double)
    floatDigits _       =  floatDigits (0::Double)
    floatRange _        =  floatRange  (0::Double)

    decodeFloat (D0 x)  = decodeFloat x
    encodeFloat i e     = d0 $ encodeFloat i e
    exponent (D0 x)     = exponent x
    significand (D0 x)  = d0 $ significand x

    scaleFloat k (D0 x) = d0 $ scaleFloat k x
    isNaN          (D0 x) = isNaN          x
    isInfinite     (D0 x) = isInfinite     x
    isDenormalized (D0 x) = isDenormalized x
    isNegativeZero _      = False
    isIEEE _              = False

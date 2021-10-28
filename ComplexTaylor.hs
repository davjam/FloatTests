-- {-# OPTIONS -Wall -Wpartial-fields #-}

module ComplexTaylor (Complex(..), Function(..), fnT) where

data Function = Sin | Cos | Sinh | Cosh
  deriving (Eq, Show)
  
fnT :: Function -> Complex Rational -> Complex Rational
fnT Sin  = sinTay
fnT Cos  = cosTay
fnT Sinh = sinhTay
fnT Cosh = coshTay


--import Data.Complex
infix  6  :+
data Complex a
  = !a :+ !a    -- ^ forms a complex number from its real and imaginary
                -- rectangular components.
        deriving ( Eq          -- ^ @since 2.01
                 , Show        -- ^ @since 2.01
                 , Read        -- ^ @since 2.01
                 )


--Taylor series calcs per https://en.wikipedia.org/wiki/Taylor_series
sinTay, cosTay, sinhTay, coshTay, expTay :: Complex Rational -> Complex Rational
sinTay = taylor (cycle [1,-1]) [1,3..]
cosTay = taylor (cycle [1,-1]) [0,2..]
sinhTay = taylor (repeat 1) [1,3..]
coshTay = taylor (repeat 1) [0,2..]
expTay = taylor (repeat 1) [0..]

taylor :: [Complex Rational] -> [Integer] -> Complex Rational -> Complex Rational
taylor coefs expons x = sum $ take 20 $ taylorTerms coefs expons x

--only valid for some functions (i.e. those above)
taylorTerms :: [Complex Rational] -> [Integer] -> Complex Rational -> [Complex Rational]
taylorTerms coefs expons x = zipWith t coefs expons
  where
    t c e = c * x^e / fromInteger (fact e)

fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact (n - 1)

instance  (Num a) => Num (Complex a)  where
    (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
    (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
    (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)       =  negate x :+ negate y
    abs z               =  undefined --magnitude z :+ 0
    signum z            =  undefined -- 
--    signum z@(x:+y)     =  x/r :+ y/r  where r = magnitude z
    fromInteger n       =  fromInteger n :+ 0

instance  (Fractional a) => Fractional (Complex a)  where
    (x:+y) / (x':+y')   =  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
                           where x'' =  x'
                                 y'' =  y'
                                 d   = x'*x'' + y'*y''

    fromRational a      =  fromRational a :+ 0

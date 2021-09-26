{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use tan" #-}

import Data.Foldable

data Fn a = Fn 
  String          --name
  (a -> a)        --function
  [Expected a]    --expected results

data Expected a = E a    --exactly
                | A a    --approximately (but to many sfs)
                | R      --a real number (not NaN or infinite)
  deriving Show

vals :: RealFloat a => [a]
vals =                                  [  nan,     -inf , -mx       ,     -1      ,     -0  ,      0  ,     1     ,     mx   ,     inf ]
                                                           
fns :: RealFloat a => [Fn a]                               
fns = [Fn "recip"         recip         [E nan, E $ -0   , A $  -0   , E $ -1      , E $ -inf, E $  inf, E $ 1     , A $ 0    , E $ 0   ]
      ,Fn "sqrt"          sqrt          [E nan, E $  nan , E $  nan  , E $  nan    , E $ -0  , E $  0  , E $ 1     , R        , E $ inf ]
      ,Fn "(^2)"          (^(2::Int))   [E nan, E $  inf , E $  inf  , E $  1      , E $  0  , E $  0  , E $ 1     , E $ inf  , E $ inf ]
      ,Fn "(^^2)"         (^^(2::Int))  [E nan, E $  inf , E $  inf  , E $  1      , E $  0  , E $  0  , E $ 1     , E $ inf  , E $ inf ]
      ,Fn "(**2)"         (**2)         [E nan, E $  inf , E $  inf  , E $  1      , E $  0  , E $  0  , E $ 1     , E $ inf  , E $ inf ]
      ,Fn "(2**)"         (2**)         [E nan, E $  0   , E $  0    , E $  0.5    , E $  1  , E $  1  , E $ 2     , E $ inf  , E $ inf ]
      ,Fn "exp"           exp           [E nan, E $  0   , E $  0    , A $  expN1  , E $  1  , E $  1  , A $ exp1  , E $ inf  , E $ inf ]
      ,Fn "log"           log           [E nan, E $  nan , E $  nan  , E $  nan    , E $ -inf, E $ -inf, E $ 0     , R        , E $ inf ]
      ,Fn "(logBase 2)"   (logBase 2)   [E nan, E $  nan , E $  nan  , E $  nan    , E $ -inf, E $ -inf, E $ 0     , R        , E $ inf ]
      ,Fn "(`logBase` 2)" (`logBase` 2) [E nan, E $  nan , E $  nan  , E $  nan    , E $ -0  , E $ -0  , E $ inf   , R        , E $ 0   ] --log base 0 and 1 should be undefined?
      ,Fn "sin"           sin           [E nan, E $  nan , R         , A $ -sin1   , E $ -0  , E $ 0   , A $ sin1  , R        , E $ nan ]
      ,Fn "cos"           cos           [E nan, E $  nan , R         , A $  cos1   , E $  1  , E $ 1   , A $ cos1  , R        , E $ nan ]
      ,Fn "tan"           tan           [E nan, E $  nan , A $ -tanMx, A $ -tan1   , E $ -0  , E $ 0   , A $ tan1  , A $ tanMx, E $ nan ]
      ,Fn "asin"          asin          [E nan, E $  nan , E $  nan  , A $ -pi/2   , E $ -0  , E $ 0   , A $ pi/2  , E $ nan  , E $ nan ]
      ,Fn "acos"          acos          [E nan, E $  nan , E $  nan  , A $  pi     , A $ pi/2, A $ pi/2, E $ 0     , E $ nan  , E $ nan ]
      ,Fn "atan"          atan          [E nan, A $ -pi/2, A $ -pi/2 , A $ -pi/4   , E $ -0  , E $ 0   , A $ pi/4  , A $ pi/2 , A $ pi/2]
      ,Fn "sinh"          sinh          [E nan, E $ -inf , E $ -inf  , A $ -sinh1  , E $ -0  , E $ 0   , A $ sinh1 , E $ inf  , E $ inf ]
      ,Fn "cosh"          cosh          [E nan, E $  inf , E $  inf  , A $  cosh1  , E $  1  , E $ 1   , A $ cosh1 , E $ inf  , E $ inf ]
      ,Fn "tanh"          tanh          [E nan, E $ -1   , E $ -1    , A $ -tanh1  , E $ -0  , E $ 0   , A $ tanh1 , E $ 1    , E $ 1   ]
      ,Fn "asinh"         asinh         [E nan, E $ -inf , R         , A $ -asinh1 , E $ -0  , E $ 0   , A $ asinh1, R        , E $ inf ]
      ,Fn "acosh"         acosh         [E nan, E $  nan , E $ nan   , E $  nan    , E $  nan, E $ nan , E $ 0     , R        , E $ inf ]
      ,Fn "atanh"         atanh         [E nan, E $  nan , E $ nan   , E $ -inf    , E $ -0  , E $ 0   , E $ inf   , E $ nan  , E $ nan ]
      ]
inf, nan, mx :: forall a. RealFloat a => a
inf = 1/0
nan = 0/0
mx = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e
    a = undefined :: a

exp1, expN1, sin1, cos1, tan1, sinh1, cosh1, tanh1, asinh1, tanMx :: RealFloat a => a
exp1  = fromRational $ expTay    1
expN1 = fromRational $ expTay $ -1
sin1  = fromRational $ sinTay 1
cos1  = fromRational $ cosTay 1
tan1  = sin1/cos1
sinh1 = fromRational $ sinhTay 1
cosh1 = fromRational $ coshTay 1
tanh1 = sinh1 / cosh1
asinh1 = asinhNewt 1

tanMx = sin mx / cos mx

main :: IO ()
main = do
  putStrLn "Double fails:"
  traverse_ (testFn (vals::[Double])) fns
  putStrLn "Float fails:"
  traverse_ (testFn (vals::[Float])) fns
  putStrLn "Large sin fails:"
  traverse_ print (sinLargeFails @Double)
  where
    testFn :: (Show a, RealFloat a) => [a] -> Fn a -> IO ()
    testFn xs (Fn name f expecteds) = traverse_ putFail $ zip xs expecteds
      where
        putFail (x,ey) | f x `hasVal` ey = return ()
                       | otherwise = putStrLn $ name ++ " " ++ show x ++ " sb " ++ show ey ++ " is " ++ show (f x)

hasVal :: RealFloat a => a -> Expected a -> Bool
x `hasVal` R     | isNaN x          = False
                 | isInfinite x     = False
                 | otherwise        = True
x `hasVal` (E y) | isNaN x          = isNaN y
                 | isNaN y          = False
                 | isNegativeZero x = isNegativeZero y
                 | isNegativeZero y = False
                 | otherwise        = x == y
x `hasVal` (A y) | isNaN x          = isNaN y
                 | isNaN y          = False
                 | abs y < err      = abs x < err
                 | signum x /= signum y = False
                 | isInfinite x     = isInfinite y
                 | otherwise        = abs (x/y - 1) < err
  where err = 0.000001

--Taylor series calcs per https://en.wikipedia.org/wiki/Taylor_series
sinTay, cosTay, sinhTay, coshTay, expTay :: Rational -> Rational
sinTay = taylor (cycle [1,-1]) [1,3..]
cosTay = taylor (cycle [1,-1]) [0,2..]
sinhTay = taylor (repeat 1) [1,3..]
coshTay = taylor (repeat 1) [0,2..]
expTay = taylor (repeat 1) [0..]

taylor :: [Rational] -> [Integer] -> Rational -> Rational
taylor coefs expons x = sum $ take 20 $ taylorTerms coefs expons x

taylorTerms :: [Rational] -> [Integer] -> Rational -> [Rational]
taylorTerms coefs expons x = zipWith t coefs expons
  where
    t c e = c * x^e / fromInteger (fact e)

--Newton method per https://en.wikipedia.org/wiki/Newton%27s_method
asinhNewt :: RealFloat a => a -> a
asinhNewt x = iterate (\z -> z - (sinh z - x)/cosh z) 1 !! 30

fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact (n - 1)

piRat :: Rational
piRat = 4 / foldr (\i f -> 2*i-1 + i^(2::Int)/f) (2*n - 1) [1..n]
  where n = 21  --gives similar accuracy as pi::Double

sinLarge :: RealFloat a => a -> a
sinLarge x = fromRational $ sinTay r
  where
    (n, _) = properFraction $ xRat / (2 * piRat)
    r = xRat - 2 * fromInteger n * piRat
    xRat = toRational x

sinLargeFails :: RealFloat a => [a]
sinLargeFails = filter (\x -> not $ sin x `hasVal` A (sinLarge x)) bigNums

bigNums :: RealFloat a => [a]
bigNums = take 10 $ iterate sqrt mx

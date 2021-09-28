{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use tan" #-}

import Control.Monad
import Data.Foldable
import qualified MyFloat as F

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
                                                           
--See IEEE 754 Standards 9.2.1 Special Values for some of these.
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
      ,Fn "tan"           tan           [E nan, E $  nan , R         , A $ -tan1   , E $ -0  , E $ 0   , A $ tan1  , R        , E $ nan ]
      ,Fn "asin"          asin          [E nan, E $  nan , E $  nan  , A $ -pi/2   , E $ -0  , E $ 0   , A $ pi/2  , E $ nan  , E $ nan ]
      ,Fn "acos"          acos          [E nan, E $  nan , E $  nan  , A $  pi     , A $ pi/2, A $ pi/2, E $ 0     , E $ nan  , E $ nan ]
      ,Fn "atan"          atan          [E nan, A $ -pi/2, A $ -pi/2 , A $ -pi/4   , E $ -0  , E $ 0   , A $ pi/4  , A $ pi/2 , A $ pi/2]
      ,Fn "sinh"          sinh          [E nan, E $ -inf , E $ -inf  , A $ -sinh1  , E $ -0  , E $ 0   , A $ sinh1 , E $ inf  , E $ inf ]
      ,Fn "cosh"          cosh          [E nan, E $  inf , E $  inf  , A $  cosh1  , E $  1  , E $ 1   , A $ cosh1 , E $ inf  , E $ inf ]
      ,Fn "tanh"          tanh          [E nan, E $ -1   , E $ -1    , A $ -tanh1  , E $ -0  , E $ 0   , A $ tanh1 , E $ 1    , E $ 1   ]
      ,Fn "asinh"         asinh         [E nan, E $ -inf , R         , A $ -asinh1 , E $ -0  , E $ 0   , A $ asinh1, R        , E $ inf ]
      ,Fn "F.asinh"       F.asinh       [E nan, E $ -inf , R         , A $ -asinh1 , E $ -0  , E $ 0   , A $ asinh1, R        , E $ inf ]
      ,Fn "acosh"         acosh         [E nan, E $  nan , E $ nan   , E $  nan    , E $  nan, E $ nan , E $ 0     , R        , E $ inf ]
      ,Fn "atanh"         atanh         [E nan, E $  nan , E $ nan   , E $ -inf    , E $ -0  , E $ 0   , E $ inf   , E $ nan  , E $ nan ]
      ,Fn "F.atanh"       F.atanh       [E nan, E $  nan , E $ nan   , E $ -inf    , E $ -0  , E $ 0   , E $ inf   , E $ nan  , E $ nan ]
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

exp1, expN1, sin1, cos1, tan1, sinh1, cosh1, tanh1, asinh1 :: RealFloat a => a
exp1  = fromRational $ expTay    1
expN1 = fromRational $ expTay $ -1
sin1  = fromRational $ sinTay 1
cos1  = fromRational $ cosTay 1
tan1  = sin1/cos1
sinh1 = fromRational $ sinhTay 1
cosh1 = fromRational $ coshTay 1
tanh1 = sinh1 / cosh1
asinh1 = asinhNewt 1

main :: IO ()
main = do
  putStrLn "Double fails:"
  traverse_ (testFn (vals::[Double])) fns
  putStrLn "Float fails:"
  traverse_ (testFn (vals::[Float])) fns

  unless (isIncreasingAt @Double F.asinh F.asinhCutover) $ putStrLn "asinh not increasing (Double)"
  unless (isIncreasingAt @Float  F.asinh F.asinhCutover) $ putStrLn "asinh not increasing (Float)"
  
  putStrLn "identity fails (Double):"
  itentityFails @Double
  putStrLn "identity fails (Float):"
  itentityFails @Float

  putStrLn "alg values fails (Double):"
  testAlgVals @Double
  putStrLn "alg values fails (Float):"
  testAlgVals @Float

  putStrLn "Large sin fails (Double):"
  traverse_ print (sinLargeFails @Double)
  putStrLn "Large sin fails (Float):"
  traverse_ print (sinLargeFails @Float)

  putStrLn "Large cos fails (Double):"
  traverse_ print (cosLargeFails @Double)
  putStrLn "Large cos fails (Float):"
  traverse_ print (cosLargeFails @Float)

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

{-
"CORRECT" CALCULATIONS USING TAYLOR SERIES ETC
-}

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

{-
TESTS FOR SIN ETC OF HUGE NUMBERS
-}

piRat :: Rational
piRat = 4 / foldr (\i f -> 2*i-1 + i^(2::Int)/f) (2*n - 1) [1..n]
  where n = 30  --21 gives similar accuracy as pi::Double

sinLarge :: RealFloat a => a -> a
sinLarge = fromRational . sinTay . mod2pi

cosLarge :: RealFloat a => a -> a
cosLarge = fromRational . cosTay . mod2pi

mod2pi :: RealFloat a => a -> Rational
mod2pi x = xRat - 2 * fromInteger n * piRat
  where
    xRat = toRational x
    (n, _) = properFraction $ xRat / (2 * piRat)


sinLargeFails :: RealFloat a => [a]
sinLargeFails = filter (\x -> not $ sin x `hasVal` A (sinLarge x)) bigNums

cosLargeFails :: RealFloat a => [a]
cosLargeFails = filter (\x -> not $ cos x `hasVal` A (cosLarge x)) bigNums

bigNums :: RealFloat a => [a]
bigNums = ns ++ map negate ns where
  ns = take 14 $ iterate (*10) 10 --at x=1e15, sin x/cos x = -1.672409893861645; tan x = -1.672414782127583. They differ by slightly more than err.

--See https://en.wikipedia.org/wiki/Trigonometric_functions#Algebraic_values
algVals :: RealFloat a => [(String, a,a,a)] --(name, x, sin x, cos x)
algVals = [("  pi/12",   pi/12, (sqrt 6 - sqrt 2)/4  , (sqrt 6 + sqrt 2)/4  )
          ,("  pi/10",   pi/10, (sqrt 5 - 1)/4       , sqrt(10 + 2*sqrt 5)/4)
          ,("  pi/8 ",   pi/8 , sqrt(2-sqrt 2) / 2   , sqrt(2 + sqrt 2)/2   )
          ,("  pi/6 ",   pi/6 , 1/2                  , sqrt 3 / 2           )
          ,("  pi/5 ",   pi/5 , sqrt(10-2*sqrt 5) / 4, (1 + sqrt 5) / 4     )
          ,("  pi/4 ",   pi/4 , sqrt 2 / 2           , sqrt 2 / 2           )
          ,("3*pi/10", 3*pi/10, (1+sqrt 5)/4         , sqrt(10 - 2*sqrt 5)/4)
          ,("  pi/3 ",   pi/3 , sqrt 3 / 2           , 1/2                  )
          ,("3*pi/8 ", 3*pi/8 , sqrt(2+sqrt 2) / 2   , sqrt(2 - sqrt 2)/2   )
          ,("2*pi/5 ", 2*pi/5 , sqrt(10+2*sqrt 5) / 4, (sqrt 5 - 1)/4       )
          ,("5*pi/12", 5*pi/12, (sqrt 6 + sqrt 2)/4  , (sqrt 6 - sqrt 2)/4  )
          ,("  pi/2 ",   pi/2 , 1                    , 0                    )
          ]

testAlgVals :: forall a. RealFloat a => IO ()
testAlgVals = traverse_ testAlg (algVals @a)
  where
    testAlg (name,x,siny,cosy) = do
      unless ((fromRational . sinTay . toRational) x `hasVal` A siny) $ putStrLn $ "sinTay error: " ++ name
      unless ((fromRational . cosTay . toRational) x `hasVal` A cosy) $ putStrLn $ "cosTay error: " ++ name
      unless (             sin x `hasVal` A siny       ) $ putStrLn $ "sin " ++ name
      unless (             cos x `hasVal` A cosy       ) $ putStrLn $ "cos " ++ name
      unless (cosy == 0 || tan x `hasVal` A (siny/cosy)) $ putStrLn $ "tan " ++ name
      

{-
TESTS FOR IDENTITIES
-}

--expect tan x = sin x / cos x

itentityFails :: forall a. (RealFloat a, Enum a, Show a) => IO ()
itentityFails = do
  traverse_ idFails identities
  where
    idFails (name, f1, f2) = traverse idFail (smallNums ++ bigNums :: [a])
      where
        idFail x | f1 x `hasVal` A (f2 x) = return ()
                 | otherwise = putStrLn $ name ++ " " ++ show x

identities :: RealFloat a => [(String, a -> a, a -> a)]
identities = [("sin -x == -sin x", sin . negate, negate . sin)
             ,("cos -x == cos x", cos . negate, cos)
             ,("tan -x == - tan x", tan . negate, negate . tan)
             ,("sin^2 + cos^2 = 1", \x -> (sin x)^(2::Int) + (cos x)^(2::Int), const 1)
             ,("tan=sin/cos", tan, \x -> sin x / cos x)
             ,("sinh -x == -sinh x", sinh . negate, negate . sinh)
             ,("cosh -x == cosh x", cosh . negate, cosh)
             ,("tanh -x == -tanh x", tanh . negate, negate . tanh)
             {-although mathematically true, these fail computationally even on small numbers:
             ,("cosh x + sinh x = exp x", \x -> cosh x + sinh x, exp)
             ,("cosh^2 - sinh^2 = 1", \x -> (cosh x)^(2::Int) - (sinh x)^(2::Int), const 1)
             -}
             ]

smallNums :: (Enum a, RealFloat a) => [a]
smallNums = ns ++ map negate ns where
  ns = [1/16,1/8..10]

{-
TESTS FOR MONOTONICITY
-}

isIncreasingAt :: RealFloat a => (a -> a) -> a -> Bool
isIncreasingAt f x0 = smallInc y0  y1
                   && smallInc ym1 y0
  where
    (y0, _, y1 ) = yStep nextUp   f x0
    (_ , _, ym1) = yStep nextDown f x0
    smallInc ya yb = yb > ya && yb - ya < 0.00001 --FIXME: This is good for asinh (which is only slightly increasing), but probably no good for other cases.

    --These simplistic definitions fail at zero, infinities, NaN, etc:
    nextUp   x = encodeFloat (m+1) e where (m,e) = decodeFloat x
    nextDown x = encodeFloat (m-1) e where (m,e) = decodeFloat x

yStep :: RealFloat a
      => (a -> a)     --a step function in x
      -> (a -> a)     --the function to test
      -> a            --x0
      -> (a, a, a)    --(f x0, x1, f x1) where x1 is the first x where f(x1) /= f(x0)
yStep xStep f x0 = (y0, x1, f x1) where
    y0 = f x0
    xs = iterate xStep (xStep x0)
    x1 = head $ dropWhile ((== y0) . f) xs

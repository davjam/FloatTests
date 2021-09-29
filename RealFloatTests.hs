{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use tan" #-}

import HasVal

--uncomment these to test revised functions.
-- import Prelude hiding (asinh, atanh)
-- import MyFloat (asinh, atanh)

main :: IO ()
main = do
  putFails "Special Value Double" (specValTests @Double)
  putFails "Special Value Float"  (specValTests @Float )

  putFails "Identity Double" (identityTests @Double)
  putFails "Identity Float"  (identityTests @Float )

  putFails "Algebraic Values Double" (algValTests @Double)
  putFails "Algebraic Values Float"  (algValTests @Float )

  putFails "Large Trig Double" (largeTrigTests @Double)
  putFails "Large Trig Float"  (largeTrigTests @Float )

  monotonTest @Double "Double"
  monotonTest @Float  "Float"

-----------------------------------------------------------------------------
-- SPECIAL VALUE TESTS
-- See IEEE 754 Standards 9.2.1 Special Values for some of these.

specValTests :: (RealFloat a, Show a) => [Test a (Expected a)]
specValTests = [Test "StdVal" name (show x) (f x) expected | Fn name f exps <- fns, (x,expected) <- zip vals exps]

data Fn a = Fn 
  String          --name
  (a -> a)        --function
  [Expected a]    --expected results

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
      ,Fn "tan"           tan           [E nan, E $  nan , R         , A $ -tan1   , E $ -0  , E $ 0   , A $ tan1  , R        , E $ nan ]
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

-----------------------------------------------------------------------------
-- TESTS FOR IDENTITIES.
-- https://en.wikipedia.org/wiki/Trigonometric_functions#Basic_identities

identityTests :: (RealFloat a, Enum a, Show a) => [Test a (Expected a)]
identityTests = [ Test "identity" name (show x) (f1 x) (A $ f2 x)
                | (name, f1, f2) <- identities
                , x <- smallNums ++ bigNums
                ]

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

-----------------------------------------------------------------------------
-- ALGEBRAIC IDENTITY TESTS
-- https://en.wikipedia.org/wiki/Trigonometric_functions#Simple_algebraic_values

algValTests :: (RealFloat a, Show a) => [Test a (Expected a)]
algValTests = concat [[Test "Alg value" "sin" xName (sin x) (A sinx)
                      ,Test "Alg value" "cos" xName (cos x) (A cosx)
                      ,Test "Taylor check" "sin" xName (fromRational $ sinTay $ toRational x) (A sinx)
                      ,Test "Taylor check" "cos" xName (fromRational $ cosTay $ toRational x) (A cosx)
                      ]
                      | (xName, x, sinx, cosx) <- algVals
                      ]
                ++   [Test "Alg value" "tan" xName (tan x) (A $ sinx/cosx) | (xName, x, sinx, cosx) <- algVals, cosx /=0]

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

-----------------------------------------------------------------------------
-- TESTS FOR SIN ETC OF HUGE NUMBERS
-- Although sensible to a point, sin 1e300 is pretty random and would not be sensible to use.

largeTrigTests :: (RealFloat a, HasVal a (Expected a), Show a) => [Test a (Expected a)]
largeTrigTests =  [Test "LargeTrig" "sin" (show x) (sin x) (A $ sinLarge x) | x <- bigNums]
               ++ [Test "LargeTrig" "cos" (show x) (cos x) (A $ cosLarge x) | x <- bigNums]

sinLarge :: RealFloat a => a -> a
sinLarge = fromRational . sinTay . mod2pi

cosLarge :: RealFloat a => a -> a
cosLarge = fromRational . cosTay . mod2pi

mod2pi :: RealFloat a => a -> Rational
mod2pi x = xRat - 2 * fromInteger n * piRat
  where
    xRat = toRational x
    (n, _) = properFraction $ xRat / (2 * piRat)

piRat :: Rational
piRat = 4 / foldr (\i f -> 2*i-1 + i^(2::Int)/f) (2*n - 1) [1..n]
  where n = 30  --21 gives similar accuracy as pi::Double

-----------------------------------------------------------------------------
--COMMON STUFF

bigNums :: RealFloat a => [a]
bigNums = ns ++ map negate ns where
  ns = take 14 $ iterate (*10) 10 --at x=1e15, sin x/cos x = -1.672409893861645; tan x = -1.672414782127583.

smallNums :: (Enum a, RealFloat a) => [a]
smallNums = ns ++ map negate ns where
  ns = [1/16,1/8..10]

-----------------------------------------------------------------------------
-- TESTS FOR MONOTONICITY
-- Only useful where formulae cutover from one expression to another.

monotonTest :: RealFloat a => String -> IO ()
monotonTest name = if (isIncreasingAt @Double asinh asinhCutover)
          then putStrLn $ "Monoton asinh " ++ name ++ " passed."
          else putStrLn $ "Monoton asinh " ++ name ++ " failed."

asinhCutover :: forall a. RealFloat a => a
asinhCutover = encodeFloat 1 (e `div` 2) where
  (_, e) = floatRange (undefined :: a)

isIncreasingAt :: RealFloat a => (a -> a) -> a -> Bool
isIncreasingAt f x0 = smallInc y0  y1
                   && smallInc ym1 y0
  where
    (y0, _, y1 ) = yStep nextUp   f x0
    (_ , _, ym1) = yStep nextDown f x0
    smallInc ya yb = yb > ya && yb - ya < 0.00001 --FIXME: This is good for asinh (which is only slightly increasing), but probably no good for other cases.
    --These simplistic definitions fail at zero, infinities etc:
    nextUp   = next 1
    nextDown = next (-1)
    next step x | isNaN x = x
                | otherwise = encodeFloat (m+step) e where (m,e) = decodeFloat x

yStep :: RealFloat a
      => (a -> a)     --a step function in x
      -> (a -> a)     --the function to test
      -> a            --x0
      -> (a, a, a)    --(f x0, x1, f x1) where x1 is the first x where f(x1) /= f(x0)
yStep xStep f x0 = (y0, x1, f x1) where
    y0 = f x0
    xs = iterate xStep (xStep x0)
    x1 = head $ dropWhile ((== y0) . f) xs

-----------------------------------------------------------------------------
-- COMPARISON FORMULAE, using Taylor series, etc

--Taylor series calcs per https://en.wikipedia.org/wiki/Taylor_series
sinTay, cosTay, sinhTay, coshTay, expTay :: Rational -> Rational
sinTay = taylor (cycle [1,-1]) [1,3..]
cosTay = taylor (cycle [1,-1]) [0,2..]
sinhTay = taylor (repeat 1) [1,3..]
coshTay = taylor (repeat 1) [0,2..]
expTay = taylor (repeat 1) [0..]

taylor :: [Rational] -> [Integer] -> Rational -> Rational
taylor coefs expons x = sum $ take 20 $ taylorTerms coefs expons x

--only valid for some functions (i.e. those above)
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


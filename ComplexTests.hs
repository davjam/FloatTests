{-# OPTIONS -Wall -Wpartial-fields -Wno-unused-matches #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ExplicitForAll, TypeApplications, ScopedTypeVariables #-}

--NB This now tests quite a lot of different values. It's a bit slow unless compiled.
--But you can do e.g.
--putFails "" (gnumericTests @Double)

import MyComplex
import qualified OldComplex as O
import qualified MyFloat as F
import Double0
import HasVal

import Data.Char
import Data.Maybe
import GHC.Float

------------------------------------
--Tests
------------------------------------

main :: IO ()
main = do
  putFails "Bug Fix Tests D0"     (bugFixTests @D0)
  putFails "Bug Fix Tests Double" (bugFixTests @Double)
  putFails "Bug Fix Tests Float"  (bugFixTests @Float)
  
  putFails "sqrt D0"         (sqrtTests @D0)
  putFails "sqrt Double"     (sqrtTests @Double)
  putFails "sqrt Float"      (sqrtTests @Float)

  putFails "sqrt extremes"     extremeSqrtTests

  putFails "Real vs Complex D0"     (realCpxMatchTests @D0)
  putFails "Real vs Complex Double" (realCpxMatchTests @Double)
  putFails "Real vs Complex Float"  (realCpxMatchTests @Float)

  putFails "nonNaN D0"        (nonNaNTests @D0        )
  putFails "nonNaN Double"    (nonNaNTests @Double    )
  putFails "nonNaN Float"     (nonNaNTests @Float     )

  putFails "Conjugate D0"     (conjTests @D0    )
  putFails "Conjugate Double" (conjTests @Double)
  putFails "Conjugate Float"  (conjTests @Float )

  putFails "inverseTests D0"     (inverseTests @D0     A)
  putFails "inverseTests Double" (inverseTests @Double A)
  putFails "inverseTests Float"  (inverseTests @Float  $ A2 2)  --Float just isn't as accurate

  putFails "gnumericTests D0"     (gnumericTests @D0)
  putFails "gnumericTests Double" (gnumericTests @Double)
  putFails "gnumericTests Float"  (gnumericTests @Float)

  putFails "regressionTests D0"     (regressionTests @D0      A)
  putFails "regressionTests Double" (regressionTests @Double  A)
  putFails "regressionTests Float"  (regressionTests @Float   $ A2 3)
  
  putFails "Double vs Float"  doubleVsFloatTests

bugFixTests :: forall a. (RealFloat a, Show a) => [Test a]
bugFixTests = concat
  [--tests for historic fixes
   let z = (-1):+0     in testC False "#4228 #1 atanh"                (show z) (atanh z) (E (-inf))(E 0)
  ,let z = ( 1):+0     in testC False "#4228 #2 atanh"                (show z) (atanh z) (E ( inf))(E 0)
  ,let z = (-1):+0     in testC False "#8532 #1 acosh"                (show z) (acosh z) (E 0)     (E pi)
  ,let z = (-1):+(-0)  in testC False "#8532 #2 acosh"                (show z) (acosh z) (E 0)     (E $ if isIEEE (realPart z) then -pi else pi)
  ,let z = 0:+0        in testC False "#8539 #1 (**2)"                (show z) (z**2   ) (E 0)     (E 0) --MORE NEEDED FOR 8539? (Though I've not changed **)

  --Complex->Float fixes
  ,let z = mn:+0       in [Test       "magnitude"                     (show z) (magnitude z)  (E mn)]
  ,let z = 0:+mn       in [Test       "magnitude"                     (show z) (magnitude z)  (E mn)]
  ,let z = (-0):+0     in [Test       "phase"                         (show z) (phase z)      (E $ if isIEEE (undefined :: a) then  pi else 0)]
  ,let z = (-0):+(-0)  in [Test       "phase"                         (show z) (phase z)      (E $ if isIEEE (undefined :: a) then -pi else 0)]
                          
  --Complex->Complex fixes (in order listed in https://gitlab.haskell.org/ghc/ghc/-/issues/20425)
  --neg zero
  , testC' False "11" Log   (  0  :+  0  ) (E (-inf))        (E   0 )
  , testC' False "11" Log   (  0  :+(-0) ) (E (-inf))        (E (-0))
  , testC' False "12" Tan   (  0  :+  3  ) (E   0   )         R
  , testC' False "12" Tan   ((-0) :+  3  ) (E (-0  ))         R
  , testC' False "13" Tanh  (  3  :+  0  )  R                (E   0 )
  , testC' False "13" Tanh  (  3  :+(-0) )  R                (E (-0))
  , testC' False "14" Acosh (  3  :+  0  )  R                (E   0 )
  , testC' False "14" Acosh (  3  :+(-0) )  R                (E (-0))
  , testC' False "15" Sqrt  ( mx  :+ mx  )  R                 R
  , testC' False "16" Log   ( mx  :+ mx  )  R                 R
  , testC' False "17" Tan   ( 0   :+ 800 ) (E 0)             (E 1)
  , testC' False "18" Tanh  (800  :+ 0   ) (E 1)             (E 0)
  , testC' False "19" Asin  (mx   :+ mx  )  R                 R         --covers 19 & 20
  , testC' False "21" Acos  (mx   :+ mx  )  R                 R
  , testC' False "22" Acosh (mx   :+ mx  )  R                 R
  , testC' False "23" Sqrt  ((-4) :+  0  ) (E  0 )           (E 2)
  , testC' True  "23" Sqrt  ((-4) :+(-0) ) (E  0 )           (E (-2))
  , testC' False "24" Asin  ((-2) :+  0  )  N                 P
  , testC' True  "24" Asin  ((-2) :+(-0) )  N                 N
  , testC' True  "25" Acos  (  3  :+  0  ) (E 0)              N
  , testC' False "25" Acos  (  3  :+(-0) ) (E 0)              P
  , testC' False "26" Atan  (  0  :+  3  )  P                 P
  , testC' True  "26" Atan  ((-0) :+  3  )  N                 P
  , testC' False "27" Asinh (  0  :+  3  )  P                 P
  , testC' True  "27" Asinh ((-0) :+  3  )  N                 P
  , testC' True  "28" Atanh (  3  :+  0  )  P                 P
  , testC' False "28" Atanh (  3  :+(-0) )  P                 N
  , testC' False "29" Atan  (  0  :+1e-20) (E 0)             (E 1e-20)
  , testC' False "29" Atan  (1e-20:+0    ) (E 1e-20)         (E 0)
  , testC' False "30" Tan   (pi/2 :+0    ) (A $ tan (pi/2))  (E 0)

  ,let z1 = (-1531.9375):+0 --For Float, original code gave significantly different results for z1 & z2.
       z2 = z1 - 0.0001
                       in [Test       "31: asin" (show z1) (imagPart $ asin z1) (A2 2 $ imagPart $ asin z2)]
  ]

sqrtTests ::  forall a. (RealFloat a, Show a) => [Test a]
sqrtTests = concat $  --all based on the expectations listed in Kahan's CSQRT function.
     [ testC False "sqrt #1  sqrt " (show z) (sqrt z) (E 0)   (E $  abs (sqrt x)) | x <- xs, x >= 0, let z = (-x)  :+   0   ] 
  ++ [ testC True  "sqrt #2  sqrt " (show z) (sqrt z) (E 0)   (E $ -abs (sqrt x)) | x <- xs, x >= 0, let z = (-x)  :+ (-0  )]
  ++ [ testC False "sqrt #3  sqrt " (show z) (sqrt z) (E inf) (E   inf )    | x <- xs ++ bads, let z =   x   :+ ( inf)]
  ++ [ testC False "sqrt #4  sqrt " (show z) (sqrt z) (E inf) (E (-inf))    | x <- xs ++ bads, let z =   x   :+ (-inf)]
  ++ [ testC False "sqrt #5  sqrt " (show z) (sqrt z) (E nan) (E nan)       | x <- xs        , let z = nan   :+ x     ]
  ++ [ testC False "sqrt #6  sqrt " (show z) (sqrt z) (E nan) (E nan)       | x <- xs        , let z = x     :+ nan   ]
  ++ [ testC False "sqrt #7  sqrt " (show z) (sqrt z) (E nan) (E nan)       |                  let z = nan   :+ nan   ]
  ++ [ testC False "sqrt #8  sqrt " (show z) (sqrt z) (E inf) (E (sign0 x)) | x <- xs        , let z = inf   :+ x     ]
  ++ [ testC False "sqrt #9  sqrt " (show z) (sqrt z) (E inf) (E nan)       |                  let z = inf   :+ nan   ]
  ++ [ testC False "sqrt #10 sqrt " (show z) (sqrt z) (E inf) (E nan)       |                  let z = inf   :+ (-nan)]
  ++ [ testC False "sqrt #11 sqrt " (show z) (sqrt z) (E 0)   (E (signI x)) | x <- xs        , let z = (-inf):+ x     ]
  ++ [ testC False "sqrt #12 sqrt " (show z) (sqrt z) (E nan) (E inf)       |                  let z = (-inf):+ nan   ]
  ++ [ testC False "sqrt #13 sqrt " (show z) (sqrt z) (E nan) (E inf)       |                  let z = (-inf):+ (-nan)] --suspect +/- in expected result is typo in Kahan. 

extremeSqrtTests :: [Test Double]
extremeSqrtTests = concat $
  [ testC False "sqrt " (show z) (sqrt z) (B u) (B v) | (z,u:+v) <- extremes ++ ex2]
  where
    extremes =
      [ --expected results from WolframAlpha
        (  mx  :+ mx   , 1.4730945569055654e154 :+ 6.1017574412827022e153)
      , (  mx  :+ mn   , 1.3407807929942596e154 :+ 0)
      , (  mn  :+ mx   , 9.4807519081091762e153 :+ 9.4807519081091762e153)
      , (  mn  :+ mn   , 2.45673e-162           :+ 1.01761e-162)
      , (  0   :+ mx   , 9.4807519081091762e153 :+ 9.4807519081091762e153)  --mx:+0 tested in realCpxMatchTests
      , (  0   :+ mn   , 1.58114e-162           :+ 1.58114e-162)
      , ((-mx) :+ mx   , 6.1017574412827022e153 :+ 1.4730945569055654e154)
      , ((-mx) :+ mn   , 0                      :+ 1.3407807929942596e154)
      , ((-mn) :+ mx   , 9.4807519081091762e153 :+ 9.4807519081091762e153)
      , ((-mn) :+ mn   , 1.01761e-162           :+ 2.45673e-162)
      ]
    ex2 = [(conjugate z, conjugate w) | (z,w) <- extremes]

realCpxMatchTests :: (RealFloat a, Show a) => [Test a]
realCpxMatchTests = concat
  --check imag is zero, but don't care what sign
  --since it's difficult to predict e.g. cos (3:+0) has -0.0, cos (4:+0) has 0.0.
  --(Conjugte check with check that cos (3:+0) is conj of cos (3:+(-0)), etc).
  [ testC False (fnName fn) (show z) fz (B fx) oob
  | fn <- allFunctions
  , x <- xs
  , not (exclude fn x)
  , let fx = fnR fn x
  , not $ isNaN fx
  , z  <- [x :+ 0, x :+ (-0)]
  --Nearly all points where fnR x is defined are not on branch cuts.
  --The only exception in log (-0) :+ (+/-0), which has a different result in the complex case.
  , branchCutPointQuadrant fn z == Nothing
  , let fz = fnF fn z
        oob | outOfBounds fn z == Just Imag = E nan
            | otherwise                     = Z
  ]
  where
  exclude Sq   x | isNegativeZero x = True
  exclude Sqrt x | isNegativeZero x = True
  exclude _    _                    = False

nonNaNTests :: forall a. (RealFloat a, Show a) => [Test a]
nonNaNTests = concat
  [ testC False (fnName fn) (show z) (fnF fn z) (oob Real) (oob Imag)
  | fn <- allFunctions
  , x <- extremes
  , y <- extremes
  , let z = x :+ y
        oob p | outOfBounds fn z == Just p = E nan
              | otherwise                  = NNaN
  ]
  where extremes = [-mx, -mn, -0, 0, mn, mx]

--For non-IEEE values, don't test points on branch cuts, since:
-- if it's on the real line, conjugate is same value, and both will be mapped to the same quadrant.
-- it it's on the imag line, it's one of atan or asinh. For both, +ve imag maps to QI  and -ve imag to QIII, hence won't be conjugates.
conjTests :: forall a. (RealFloat a, Show a) => [Test a]
conjTests = concat
  [ testC False (fnName fn) (show z) (f $ conjugate z) (E u) (E v)
  | fn <- allFunctions
  , let f = fnF fn
  , x <- xs
  , y <- xs
  , let z = x :+ y
  , isIEEE x || branchCutPointQuadrant fn z == Nothing
  , let (u:+v) = conjugate $ f z
  ]

inverseTests :: forall a. (RealFloat a, Show a) => (a -> Expected a) -> [Test a]
inverseTests match = concat $ 
  [ testC False (fnName invFn ++ " . " ++ fnName fn) (show z) (fnF invFn fnFz) (match $ realPart z) (match $ imagPart z)
  | (fn, invFn) <- [(Sq,  Sqrt)
                   ,(Exp, Log)
                   ,(Sin, Asin)
                   ,(Cos, Acos)
                   ,(Tan, Atan)
                   ,(Sinh, Asinh)
                   ,(Cosh, Acosh)
                   ,(Tanh, Atanh)
                    ]
  , x <- map fixPi xs, y <- map fixPi xs, let z = x:+y
  , isInversable invFn z
  , let fnFz = fnF fn z
  , isGood (realPart fnFz) && isGood (imagPart fnFz)
  , not (isBranchPoint invFn fnFz)  --exp exp of many things maps to 0:+0, log 0:+0 only maps back to one of them.
  ]
  where --With Float, pi (the Haskell variable) > pi (the mathematical value).
        --This causes e.g. exp (1:+pi) to go the wrong side of the branch cut, so log maps back to exp (1 :+ (-pi))
        --So here, we just round pi (and pi/2) so it's < math pi.
        --(For double, pi < math pi already).
        fixPi   3.1415927  =  3.1415926
        fixPi (-3.1415927) = -3.1415926
        fixPi ( 1.5707964) =  1.5707963
        fixPi (-1.5707964) = -1.5707963
        fixPi x            = x

--The expected results are calculated by gnumeric spreadsheet, which doesn;t support negative zeros.
--(Perhaps this, and the results, should only work for type D0 - but I thought it best to test with all).
--Hence we push any -0 results to 0 before testing, and (for points on branch cut), map to the +/- zero
--we'd expect to give the right result.
gnumericTests :: (RealFloat a, Show a) => [Test a]
gnumericTests = concatMap testFn allFunctions where
  testFn fn = concat $ zipWith testVal zs (fnYs fn) where
    testVal z (C (u:+v)) | isIEEE u  = testC False (fnName fn) (show z') (pushToPlusZero $ fnF fn z') (B u) (B v)
                         | otherwise = testC False (fnName fn) (show z ) (pushToPlusZero $ fnF fn z ) (B u) (B v)
      where z' | Just q <- branchCutPointQuadrant fn z = pushToQuadrant q z
               | otherwise                             =                  z
    testVal _ Err        = []
    pushToPlusZero (x:+y) = ppz x :+ ppz y
      where
        ppz r | isNegativeZero r = 0
              | otherwise        = r
  zs = [x:+y|y<-gnumericXs,x<-gnumericXs]


regressionTests :: (RealFloat a, HasVal a, Show a) => (a -> Expected a) -> [Test a]
regressionTests match = concat $
  [ testC False (fnName fn) (show z) (fnF fn z) (match $ O.realPart fnCz) (match $ O.imagPart fnCz)
  | fn <- allFunctions
  , fn /= Sq
  , x <- xs
  , y <- xs
  , let z = x :+ y
  , not (expectedRegression fn z)
  , let zC = x O.:+ y
        fnCz = fromMaybe (fnC fn zC) $ override fn zC
  ]

doubleVsFloatTests :: [Test Float]
doubleVsFloatTests = concat $ 
  [ testC False (fnName fn) (show zD) (fnF fn zF) (A $ double2Float ud) (A $ double2Float vd)
  | fn <- allFunctions
  , x <- xs
  , y <- xs
  , let zF = x :+ y
        uf:+vf = fnF fn zF
  , not (isNaN uf)
  , not (isNaN vf)
  , let zD = float2Double x :+ float2Double y
        ud:+vd = fnF fn zD
  ]
  

------------------------------------
--The functions to test
------------------------------------

data Function = Sq | Sqrt | Exp | Log
              | Sin | Asin | Cos | Acos | Tan | Atan
              | Sinh | Asinh | Cosh | Acosh | Tanh | Atanh
  deriving (Eq, Enum, Bounded, Show)

fnName :: Function -> String
fnName = map toLower . show

allFunctions :: [Function]
allFunctions = [minBound..maxBound]

fnF :: RealFloat a => Function -> Complex a -> Complex a
fnF Sq    = square
fnF Sqrt  = sqrt
fnF Exp   = exp
fnF Log   = log
fnF Sin   = sin
fnF Cos   = cos  
fnF Tan   = tan  
fnF Asin  = asin 
fnF Acos  = acos 
fnF Atan  = atan 
fnF Sinh  = sinh 
fnF Cosh  = cosh 
fnF Tanh  = tanh 
fnF Asinh = asinh
fnF Acosh = acosh
fnF Atanh = atanh

--per Kahan (stops error in NaN test).
square :: forall a. RealFloat a => Complex a -> Complex a
square (w:+n) | isNaN x = if | isInfinite y -> F.copySign 0 w :+ y
                               | isInfinite n -> (-1/0) :+ y
                               | isInfinite w -> 1/0 :+ y
                               | otherwise    -> x :+ y
                | isNaN y && isInfinite x = x :+ F.copySign 0 y
                | otherwise = x:+y
  where
    x = (w-n)*(w+n)
    y = wn+wn
    wn = w*n

fnC :: RealFloat a => Function -> O.Complex a -> O.Complex a
fnC Sq    = \z->z*z
fnC Sqrt  = sqrt
fnC Exp   = exp
fnC Log   = log
fnC Sin   = sin
fnC Cos   = cos  
fnC Tan   = tan  
fnC Asin  = asin 
fnC Acos  = acos 
fnC Atan  = atan 
fnC Sinh  = sinh 
fnC Cosh  = cosh 
fnC Tanh  = tanh 
fnC Asinh = asinh
fnC Acosh = acosh
fnC Atanh = atanh

--see https://stackoverflow.com/questions/69450017/mapping-over-rankntypes-functions for explanation of this apparent dup
fnR :: RealFloat a => Function -> a -> a  
fnR Sq    = \z->z*z
fnR Sqrt  = sqrt
fnR Exp   = exp
fnR Log   = log
fnR Sin   = sin
fnR Cos   = cos  
fnR Tan   = tan  
fnR Asin  = asin 
fnR Acos  = acos 
fnR Atan  = atan 
fnR Sinh  = sinh 
fnR Cosh  = cosh 
fnR Tanh  = tanh 
fnR Asinh = F.asinh
fnR Acosh = acosh
fnR Atanh = F.atanh

data Quadrant = QI | QII | QIII | QIV
  deriving (Eq, Show)
  
branchCutPointQuadrant :: RealFloat a => Function -> Complex a -> Maybe Quadrant
branchCutPointQuadrant Sqrt  (x:+0) | x < 0    = Just QII
branchCutPointQuadrant Log   (x:+0) | isNeg x  = Just QII
branchCutPointQuadrant Asin  (x:+0) | x < (-1) = Just QII
                                    | x > 1    = Just QIV
branchCutPointQuadrant Acos  (x:+0) | x < (-1) = Just QII
                                    | x > 1    = Just QIV
branchCutPointQuadrant Atan  (0:+y) | y < (-1) = Just QIII
                                    | y > 1    = Just QI
branchCutPointQuadrant Asinh (0:+y) | y < (-1) = Just QIII
                                    | y > 1    = Just QI
branchCutPointQuadrant Acosh (x:+0) | x < 0    = Just QII
                                    | x < 1    = Just QI
branchCutPointQuadrant Atanh (x:+0) | x < (-1) = Just QII
                                    | x > 1    = Just QIV
branchCutPointQuadrant _     _                 = Nothing

--Due to rounding, some D0 things close to the edge of the range map to the branch cut and don't inverse properly.
isInversable :: RealFloat a => Function -> Complex a -> Bool
isInversable fn z | not (isInRange fn z) = False
isInversable fn    (x:+y) | isIEEE x = True
isInversable Sqrt  (x:+y) | abs x <= mn && y <= 0 && y >= -0.5 = False
isInversable Acosh (x:+y) | abs x <= mn && y <= 0 = False
isInversable _ _ = True

--This is the "pure" definition of the ranges (though I've not have to list all non-IEEE exclusions).
isInRange :: RealFloat a => Function -> Complex a -> Bool
isInRange Sqrt  (0:+y) | notIEEE y && y < 0 = False
isInRange Sqrt  (x:+y) | isNegativeZero x = False
                       | otherwise = x >= 0
isInRange Log   (x:+y) = abs y <= pi
isInRange Asin  (x:+y) = abs x <= pi / 2
isInRange Acos  (x:+y) | notIEEE x && y < 0 = False
                       | notIEEE x && x == pi && y > 0 = False
isInRange Acos  (x:+y) | isNegativeZero x = False
                       | otherwise = 0 <= x && x <= pi
isInRange Atan  (x:+y) = abs x <= pi / 2
isInRange Asinh (x:+y) = abs y <= pi / 2
isInRange Acosh (0:+y) | notIEEE y && y < 0 = False
isInRange Acosh (x:+y) | notIEEE x && x == -pi = False
isInRange Acosh (x:+y) | isNegativeZero x = False
                       | otherwise = x >= 0 && abs y <= pi
isInRange Atanh (x:+y) = abs y <= pi / 2
isInRange _ _ = True

notIEEE :: RealFloat a => a -> Bool
notIEEE = not . isIEEE

isBranchPoint :: RealFloat a => Function -> Complex a -> Bool
isBranchPoint Log   (0:+0)    = True --per CL p310 "the domain excludes the origin"
isBranchPoint Atan  (0:+1)    = True --ditto p312
isBranchPoint Atan  (0:+(-1)) = True
isBranchPoint Atanh (1:+0)    = True --ditto p314
isBranchPoint Atanh ((-1):+0) = True
isBranchPoint _ _             = False

--These functions do some variant on sin x * exp y.
--when x == 0 and y > log mx, this becomes 0 + Infinity = NaN
--Hence these won't match complex vs real or conjugate tests.

data Part = Real | Imag
  deriving (Eq, Show)

--when "out of bounds" we end up (correctly, I think) doing 0 * Infinity giving NaN.
outOfBounds :: RealFloat a => Function -> Complex a -> Maybe Part
outOfBounds Exp  (x:+0) |     x > logmx = Just Imag
outOfBounds Sin  (0:+y) | abs y > logmx = Just Real
outOfBounds Cos  (0:+y) | abs y > logmx = Just Imag
outOfBounds Sinh (x:+0) | abs x > logmx = Just Imag
outOfBounds Cosh (x:+0) | abs x > logmx = Just Imag
outOfBounds _    _                      = Nothing

--This is where overflow Infinity/NaN incorrectly happens in existing functions.
--(Hence I can't regression test against them).
expectedRegression :: (HasVal a, RealFloat a) => Function -> Complex a -> Bool
-- deal with any infinities first.
-- then subsequent logic (e.g. abs x > blah) isn't accidentally failing to check infinities
expectedRegression _      (x:+y)  | isInfinite x                    = False
                                  | isInfinite y                    = False
expectedRegression fn   z         | branchCutPointQuadrant fn z
                                                /= Nothing          = True  --XXXX we should check - the originals might have gone the right way.
expectedRegression Sqrt z@(x:+y)  | oldSqrtOverflow z = True
                                  | x == 0 && abs y < sqrt mn       = True  --underflow
                                  | y == 0 && abs x < sqrt mn       = True  --underflow
expectedRegression Log  z@(x:+y)  | isNegativeZero x && y == 0      = True  --branch cut at -0
                                  | x == 0 && abs y < sqrt mn       = True  --underflow in magnitude
                                  | y == 0 && abs x < sqrt mn       = True  --underflow in magnitude
                                  | isGood x && isGood y
                                      && isInfinite (magnitude z)   = True  --e.g. mx :+ mx
expectedRegression Asin z@(x:+y)  | z*z+1 == z*z                    = True  --loss of precision -> NaN
                                  | z*z+(0:+1) == z*z               = True
                                  | isGood x && isGood y &&
                                    (isBad (realPart $ z*z)
                                     || isBad (imagPart $ z*z))     = True
                                  | y /= 0 && abs y <= sqrt mn      = True  --underflow in sqrt
expectedRegression Acos z@(x:+y)  | oldSqrtOverflow z               = True
                                  | z*z+1 == z*z                    = True  --loss of precision -> NaN
                                  | z*z+(0:+1) == z*z               = True
                                  | expectedRegression Sqrt (1-z*z) = True
expectedRegression Tan z@(x:+y)   | abs y >= log mx                 = True  --sinh y, cosh y overflow
                                  | isNegativeZero y                = True  --loss of -0
                                  | x ==  pi / 2                    = True  --inaccuracy
                                  | x == -pi / 2                    = True
                                  | not (tan x `hasVal` 
                                            (A $ sin x / cos x))    = True  --starts to fail after abs x > 1e10
expectedRegression Atan z@(0:+1)                                    = True  --mismatches Kahan's principle expressions
expectedRegression Atan z@(0:+(-1))                                 = True
expectedRegression Atan z@(x:+y)  | abs x >= sqrt mx                = True  --UNCLEAR
                                  | abs y >= sqrt mx                = True                       
                                  | expectedRegression Sqrt (1+z*z) = True
expectedRegression Asinh z@(x:+y) | oldSqrtOverflow z               = True
                                  | z*z+1 == z*z                    = True
                                  | z*z+(0:+1) == z*z               = True
                                  | expectedRegression Sqrt (1+z*z) = True
expectedRegression Acosh z@(x:+y) | isInfinite (realPart $ 2*z)     = True  --overflow
                                  | isInfinite (imagPart $ 2*z)     = True
                                  | expectedRegression Sqrt (z+1)   = True
                                  | expectedRegression Sqrt (z-1)   = True
expectedRegression Tanh z@(x:+y)   | abs x >= log mx                = True  --sinh x, cosh x overflow
                                   | y ==  pi / 2                   = True  --inaccuracy
                                   | y == -pi / 2                   = True
                                   | not (tan y `hasVal`
                                              (A $ sin y / cos y))  = True
expectedRegression Atanh z@(1:+0)                                   = True  --mismatches Kahan's principle expressions
expectedRegression Atanh z@((-1):+0)                                = True

expectedRegression Atanh z@(x:+y) | abs x >= sqrt mx                = True
                                  | abs y >= sqrt mx                = True  --UNCLEAR
                                  | w == 0                          = True  -- undeflow
                                  | isInfinite (realPart w)         = True
                                  | isInfinite (imagPart w)         = True
                                  | expectedRegression Log w        = True
  where w = (1.0+z) / (1.0-z)
expectedRegression _ _                                              = False

oldSqrtOverflow :: RealFloat a => Complex a -> Bool
oldSqrtOverflow z | isInfinite(realPart zz) = True
                  | isInfinite(imagPart zz) = True
                  | otherwise               = False
  where zz = z * z

isGood, isBad :: RealFloat a => a -> Bool
isBad x | isInfinite x = True
        | isNaN      x = True
        | otherwise    = False
isGood = not . isBad

override :: RealFloat a => Function -> O.Complex a -> Maybe (O.Complex a)
--Where the current functions don't fail with Infs/NaNs, but just return innacurate results.
--All values from WA - except where noted.
override Asin  ((-1.0) O.:+ (-6.2138610988780994e-21))  = Just $ (-1.57079632671606857156503670)     O.:+ (-7.88280476662849959740264749e-11)
override Asin  ((-1.0) O.:+   6.2138610988780994e-21 )  = Just $ (-1.57079632671606857156503670)     O.:+ ( 7.88280476662849959740264749e-11)
override Asin  (( 1.0) O.:+ (-6.2138610988780994e-21))  = Just $ ( 1.57079632671606857156503670)     O.:+ (-7.88280476662849959740264749e-11)
override Asin  (( 1.0) O.:+   6.2138610988780994e-21 )  = Just $ ( 1.57079632671606857156503670)     O.:+ ( 7.88280476662849959740264749e-11)
override Acos  ((-1.0) O.:+ (-6.2138610988780994e-21))  = Just $   3.14159265351096519079635839      O.:+   7.88280476662849959740264749e-11
override Acos  ((-1.0) O.:+ ( 6.2138610988780994e-21))  = Just $   3.14159265351096519079635839      O.:+ (-7.88280476662849959740264749e-11)
override Acos  (( 1.0) O.:+ ( 6.2138610988780994e-21))  = Just $   7.8828047666284996e-11            O.:+ (-7.8828047666284996e-11)
override Acos  (( 1.0) O.:+ (-6.2138610988780994e-21))  = Just $   7.8828047666284996e-11            O.:+ ( 7.8828047666284996e-11)
override Asinh ((-6.2138610988780994e-21) O.:+ (-1.0))  = Just $ (-7.88280476662849959740264749e-11) O.:+ (-1.57079632671606857156503670)
override Asinh ((-6.2138610988780994e-21) O.:+ ( 1.0))  = Just $ (-7.88280476662849959740264749e-11) O.:+ ( 1.57079632671606857156503670)
override Asinh (( 6.2138610988780994e-21) O.:+ ( 1.0))  = Just $ ( 7.88280476662849959740264749e-11) O.:+ ( 1.57079632671606857156503670)
override Asinh (( 6.2138610988780994e-21) O.:+ (-1.0))  = Just $ ( 7.88280476662849959740264749e-11) O.:+ (-1.57079632671606857156503670)
override Acosh ((-1.0) O.:+ (-6.2138610988780994e-21))  = Just $   7.88280476662849959740264749e-11  O.:+ (-3.14159265351096519079635839)
override Acosh ((-1.0) O.:+ ( 6.2138610988780994e-21))  = Just $   7.88280476662849959740264749e-11  O.:+ ( 3.14159265351096519079635839)
override Acosh (( 1.0) O.:+ (-6.2138610988780994e-21))  = Just $   7.8828047666284996e-11            O.:+ (-7.8828047666284996e-11)
override Acosh (( 1.0) O.:+ ( 6.2138610988780994e-21))  = Just $   7.8828047666284996e-11            O.:+   7.8828047666284996e-11
override Log   z@(x O.:+ y) | abs x == 5.0e-324 --WA not quite accurate here.
                           && abs y == 5.0e-324         = Just $ magEq x                             O.:+ O.phase z

--For these, I think the old atanh was wrong.
--I also think WA IS WRONG! (WA seems to think these are on the branch cut and goes in wrong direction. It gets e.g. 5.0e-19 right).
override Atanh z@(x O.:+ ( 5.0e-324))  | x > 2 && x < 4 = Just $   x' O.:+ ( 1.570796326794897)
  where x' O.:+ _ = atanh z
override Atanh z@(x O.:+ (-5.0e-324))  | x > 2 && x < 4 = Just $   x' O.:+ (-1.570796326794897)
  where x' O.:+ _ = atanh z
--these match Float values:
override Atanh z@(x O.:+ ( 1.0e-45))  | x > 2 && x < 4 = Just $   x' O.:+ ( 1.5707964)
  where x' O.:+ _ = atanh z
override Atanh z@(x O.:+ (-1.0e-45))  | x > 2 && x < 4 = Just $   x' O.:+ (-1.5707964)
  where x' O.:+ _ = atanh z

override _ z = Nothing

{-
magnitude (x:+x) = sqrt(x*x + x*x) = sqrt(2*x*x) = sqrt 2 * x

if (m,e) = decodeFloat x, then x = m * r^e

log(magnitude x) = log (sqrt 2 * x)
                 = log (sqrt 2 * m * r^e)
                 = log (2^(1/2) * m * r^e)
                 = log (2^(1/2)) + log m + log (r^e)
                 = 0.5*log 2 + log m + e*log r
-}

magEq :: RealFloat a => a -> a
magEq x = 0.5*log 2 + log m + e*log r
  where (m',e') = decodeFloat (abs x)
        m = fromIntegral m'
        e = fromIntegral e'
        r = fromIntegral $ floatRadix x

------------------------------------
-- numbers to test with
------------------------------------

xs :: RealFloat a => [a]
xs =  andNegs (map fromRational $ take 51 $ iterate (+0.1) 0)
   ++ andNegs [pi/2, e, pi]
   ++ andNegs (take 5 $ iterate sqrt mx)
   ++ andNegs (take 5 $ iterate sqrt mn)
  where e = exp 1
        andNegs ys = map negate ys ++ ys
  
bads :: RealFloat a => [a]
bads = [inf, -inf, nan]

inf, nan ::   RealFloat a => a
inf = 1/0
nan = 0/0
  
mx :: forall a. RealFloat a => a  
mx = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e
    a = undefined :: a

mn :: forall a. RealFloat a => a  
mn = encodeFloat 1 n where
    e = floatDigits a
    (e', _) = floatRange a
    n = e' - e
    a = undefined :: a

------------------------------------
-- internal functions
------------------------------------

--a more compact version for many situations
testC' :: (RealFloat a, Show a) => Bool -> String -> Function -> Complex a -> Expected a -> Expected a -> [Test a]
testC' fIEEEOnly label f z rExp iExp = testC fIEEEOnly (label ++ ": " ++ fnName f) (show z) (fnF f z) rExp iExp

--create a list of two tests, one for the realPart and one for the imagPart.
testC :: RealFloat a => Bool -> String -> String -> Complex a -> Expected a -> Expected a -> [Test a]
testC fIEEEOnly name val (x:+y) u v
  | fIEEEOnly && not (isIEEE x) = []
  | otherwise                   = [Test name (val++"(R)") x u, Test name (val++"(I)") y v]

logmx :: RealFloat a => a
logmx = log mx

isNeg :: RealFloat a => a -> Bool
isNeg x | isNegativeZero x = True
        | x < 0            = True
        | otherwise        = False

sign0 :: RealFloat a => a -> a
sign0 x | isNegativeZero x = -0
        | x < 0            = -0
        | otherwise        =  0

signI :: RealFloat a => a -> a
signI x | isNegativeZero x = -inf
        | x < 0            = -inf
        | otherwise        =  inf

--When testing a value with a signed zero on a branch cut, against a source that
--doesn't, push the -0 into the quadrant with continuity before testing.
pushToQuadrant :: RealFloat a => Quadrant -> Complex a -> Complex a
pushToQuadrant = pushToQuadrant' where
  pushToQuadrant' QI   (x:+y) = mkPos x :+ mkPos y
  pushToQuadrant' QII  (x:+y) = mkNeg x :+ mkPos y
  pushToQuadrant' QIII (x:+y) = mkNeg x :+ mkNeg y
  pushToQuadrant' QIV  (x:+y) = mkPos x :+ mkNeg y

  mkPos 0 = 0
  mkPos x = x
  mkNeg 0 = -0
  mkNeg x = x


------------------------------------
-- External results (from Gnumeric spreadsheet).

data External a = C a | Err

fnYs :: RealFloat a => Function -> [External (Complex a)]
fnYs Sq    = []
fnYs Sqrt  = sqrts 
fnYs Exp   = exps  
fnYs Log   = logs  
fnYs Sin   = sins  
fnYs Cos   = coss  
fnYs Tan   = tans  
fnYs Asin  = asins 
fnYs Acos  = acoss 
fnYs Atan  = atans 
fnYs Sinh  = sinhs 
fnYs Cosh  = coshs 
fnYs Tanh  = tanhs 
fnYs Asinh = asinhs
fnYs Acosh = acoshs
fnYs Atanh = atanhs

sqrts, exps, logs, sins, coss, tans, asins, acoss, atans, sinhs, coshs, tanhs, asinhs, acoshs, atanhs :: RealFloat a => [External (Complex a)]

gnumericXs :: RealFloat a => [a]
gnumericXs = [-3,-2,-1,0,1,2,3]

sqrts=
  [C$(0.788238760503214):+(-1.90297670599502),C$(0.8959774761298382):+(-1.67414922803554),C$(1.0397782600555705):+(-1.442615274452683),C$(1.2247448713915892):+(-1.2247448713915892),C$(1.442615274452683):+(-1.0397782600555705),C$(1.6741492280355401):+(-0.895977476129838),C$(1.9029767059950162):+(-0.7882387605032136)
  ,C$(0.5502505227003375):+(-1.8173540210239707),C$(0.6435942529055827):+(-1.5537739740300374),C$(0.7861513777574233):+(-1.272019649514069),C$(1.0000000000000002):+(-1.0000000000000002),C$(1.272019649514069):+(-0.7861513777574233),C$(1.5537739740300374):+(-0.6435942529055827),C$(1.8173540210239707):+(-0.5502505227003375)
  ,C$(0.284848784593141):+(-1.755317301824428),C$(0.34356074972251244):+(-1.455346690225355),C$(0.45508986056222733):+(-1.0986841134678098),C$(0.7071067811865476):+(-0.7071067811865476),C$(1.0986841134678098):+(-0.45508986056222733),C$(1.455346690225355):+(-0.34356074972251244),C$(1.755317301824428):+(-0.284848784593141)
  ,C$(0):+(1.7320508075688772),C$(0):+(1.4142135623730951),C$(0):+(1),C$(0):+(0),C$(1):+(0),C$(1.4142135623730951):+(0),C$(1.7320508075688772):+(0)
  ,C$(0.284848784593141):+(1.755317301824428),C$(0.34356074972251244):+(1.455346690225355),C$(0.45508986056222733):+(1.0986841134678098),C$(0.7071067811865476):+(0.7071067811865476),C$(1.0986841134678098):+(0.45508986056222733),C$(1.455346690225355):+(0.34356074972251244),C$(1.755317301824428):+(0.284848784593141)
  ,C$(0.5502505227003375):+(1.8173540210239707),C$(0.6435942529055827):+(1.5537739740300374),C$(0.7861513777574233):+(1.272019649514069),C$(1.0000000000000002):+(1.0000000000000002),C$(1.272019649514069):+(0.7861513777574233),C$(1.5537739740300374):+(0.6435942529055827),C$(1.8173540210239707):+(0.5502505227003375)
  ,C$(0.7882387605032136):+(1.9029767059950162),C$(0.8959774761298382):+(1.67414922803554),C$(1.0397782600555705):+(1.442615274452683),C$(1.2247448713915892):+(1.2247448713915892),C$(1.442615274452683):+(1.0397782600555705),C$(1.6741492280355401):+(0.895977476129838),C$(1.9029767059950162):+(0.7882387605032136)
  ]
exps=
  [C$(-0.04928882411191869):+(-0.00702595148935012),C$(-0.13398091492954262):+(-0.019098516261135196),C$(-0.36419788641329287):+(-0.05191514970317339),C$(-0.9899924966004454):+(-0.1411200080598672),C$(-2.6910786138197937):+(-0.383603953541131),C$(-7.315110094901103):+(-1.0427436562359045),C$(-19.884530844146987):+(-2.834471132487004)
  ,C$(-0.02071873100224288):+(-0.045271253156092976),C$(-0.05631934999212789):+(-0.12306002480577674),C$(-0.1530918656742263):+(-0.33451182923926226),C$(-0.4161468365471424):+(-0.9092974268256817),C$(-1.1312043837568135):+(-2.4717266720048188),C$(-3.074932320639359):+(-6.71884969742825),C$(-8.358532650935372):+(-18.263727040666765)
  ,C$(0.02690006784157161):+(-0.041894373450204546),C$(0.07312196559805964):+(-0.1138807140643681),C$(0.19876611034641298):+(-0.3095598756531122),C$(0.5403023058681398):+(-0.8414709848078965),C$(1.4686939399158851):+(-2.2873552871788423),C$(3.992324048441272):+(-6.217676312367968),C$(10.852261914197959):+(-16.901396535150095)
  ,C$(0.049787068367863944):+(0),C$(0.1353352832366127):+(0),C$(0.36787944117144233):+(0),C$(1):+(0),C$(2.718281828459045):+(0),C$(7.38905609893065):+(0),C$(20.085536923187668):+(0)
  ,C$(0.02690006784157161):+(0.041894373450204546),C$(0.07312196559805964):+(0.1138807140643681),C$(0.19876611034641298):+(0.3095598756531122),C$(0.5403023058681398):+(0.8414709848078965),C$(1.4686939399158851):+(2.2873552871788423),C$(3.992324048441272):+(6.217676312367968),C$(10.852261914197959):+(16.901396535150095)
  ,C$(-0.02071873100224288):+(0.045271253156092976),C$(-0.05631934999212789):+(0.12306002480577674),C$(-0.1530918656742263):+(0.33451182923926226),C$(-0.4161468365471424):+(0.9092974268256817),C$(-1.1312043837568135):+(2.4717266720048188),C$(-3.074932320639359):+(6.71884969742825),C$(-8.358532650935372):+(18.263727040666765)
  ,C$(-0.04928882411191869):+(0.00702595148935012),C$(-0.13398091492954262):+(0.019098516261135196),C$(-0.36419788641329287):+(0.05191514970317339),C$(-0.9899924966004454):+(0.1411200080598672),C$(-2.6910786138197937):+(0.383603953541131),C$(-7.315110094901103):+(1.0427436562359045),C$(-19.884530844146987):+(2.834471132487004)
  ]
logs=
  [C$(1.4451858789480825):+(-2.356194490192345),C$(1.2824746787307684):+(-2.158798930342464),C$(1.151292546497023):+(-1.892546881191539),C$(1.0986122886681098):+(-1.5707963267948966),C$(1.151292546497023):+(-1.2490457723982544),C$(1.2824746787307684):+(-0.982793723247329),C$(1.4451858789480825):+(-0.7853981633974483)
  ,C$(1.2824746787307684):+(-2.5535900500422257),C$(1.0397207708399179):+(-2.356194490192345),C$(0.8047189562170501):+(-2.0344439357957027),C$(0.6931471805599453):+(-1.5707963267948966),C$(0.8047189562170501):+(-1.1071487177940904),C$(1.0397207708399179):+(-0.7853981633974483),C$(1.2824746787307684):+(-0.5880026035475675)
  ,C$(1.151292546497023):+(-2.819842099193151),C$(0.8047189562170501):+(-2.677945044588987),C$(0.34657359027997264):+(-2.356194490192345),C$(0):+(-1.5707963267948966),C$(0.34657359027997264):+(-0.7853981633974483),C$(0.8047189562170501):+(-0.4636476090008061),C$(1.151292546497023):+(-0.3217505543966422)
  ,C$(1.0986122886681098):+(3.141592653589793),C$(0.6931471805599453):+(3.141592653589793),C$(0):+(3.141592653589793),Err,C$(0):+(0),C$(0.6931471805599453):+(0),C$(1.0986122886681098):+(0)
  ,C$(1.151292546497023):+(2.819842099193151),C$(0.8047189562170501):+(2.677945044588987),C$(0.34657359027997264):+(2.356194490192345),C$(0):+(1.5707963267948966),C$(0.34657359027997264):+(0.7853981633974483),C$(0.8047189562170501):+(0.4636476090008061),C$(1.151292546497023):+(0.3217505543966422)
  ,C$(1.2824746787307684):+(2.5535900500422257),C$(1.0397207708399179):+(2.356194490192345),C$(0.8047189562170501):+(2.0344439357957027),C$(0.6931471805599453):+(1.5707963267948966),C$(0.8047189562170501):+(1.1071487177940904),C$(1.0397207708399179):+(0.7853981633974483),C$(1.2824746787307684):+(0.5880026035475675)
  ,C$(1.4451858789480825):+(2.356194490192345),C$(1.2824746787307684):+(2.158798930342464),C$(1.151292546497023):+(1.892546881191539),C$(1.0986122886681098):+(1.5707963267948966),C$(1.151292546497023):+(1.2490457723982544),C$(1.2824746787307684):+(0.982793723247329),C$(1.4451858789480825):+(0.7853981633974483)
  ]
sins=
  [C$(-1.4207485419881771):+(9.917621010017536),C$(-9.15449914691143):+(4.168906959966565),C$(-8.471645454300148):+(-5.412680923178193),C$(0):+(-10.017874927409903),C$(8.471645454300148):+(-5.412680923178193),C$(9.15449914691143):+(4.168906959966565),C$(1.4207485419881771):+(9.917621010017536)
  ,C$(-0.5309210862485197):+(3.59056458998578),C$(-3.4209548611170133):+(1.5093064853236158),C$(-3.165778513216168):+(-1.9596010414216063),C$(0):+(-3.626860407847019),C$(3.165778513216168):+(-1.9596010414216063),C$(3.4209548611170133):+(1.5093064853236158),C$(0.5309210862485197):+(3.59056458998578)
  ,C$(-0.21775955162215221):+(1.1634403637032504),C$(-1.4031192506220405):+(0.4890562590412937),C$(-1.2984575814159773):+(-0.6349639147847361),C$(0):+(-1.1752011936438014),C$(1.2984575814159773):+(-0.6349639147847361),C$(1.4031192506220405):+(0.4890562590412937),C$(0.21775955162215221):+(1.1634403637032504)
  ,C$(-0.1411200080598672):+(0),C$(-0.9092974268256817):+(0),C$(-0.8414709848078965):+(0),C$(0):+(0),C$(0.8414709848078965):+(0),C$(0.9092974268256817):+(0),C$(0.1411200080598672):+(0)
  ,C$(-0.21775955162215221):+(-1.1634403637032504),C$(-1.4031192506220405):+(-0.4890562590412937),C$(-1.2984575814159773):+(0.6349639147847361),C$(0):+(1.1752011936438014),C$(1.2984575814159773):+(0.6349639147847361),C$(1.4031192506220405):+(-0.4890562590412937),C$(0.21775955162215221):+(-1.1634403637032504)
  ,C$(-0.5309210862485197):+(-3.59056458998578),C$(-3.4209548611170133):+(-1.5093064853236158),C$(-3.165778513216168):+(1.9596010414216063),C$(0):+(3.626860407847019),C$(3.165778513216168):+(1.9596010414216063),C$(3.4209548611170133):+(-1.5093064853236158),C$(0.5309210862485197):+(-3.59056458998578)
  ,C$(-1.4207485419881771):+(-9.917621010017536),C$(-9.15449914691143):+(-4.168906959966565),C$(-8.471645454300148):+(5.412680923178193),C$(0):+(10.017874927409903),C$(8.471645454300148):+(5.412680923178193),C$(9.15449914691143):+(-4.168906959966565),C$(1.4207485419881771):+(-9.917621010017536)
  ]
coss=
  [C$(-9.966909834129453):+(-1.413722590498827),C$(-4.189625690968807):+(-9.109227893755337),C$(5.439580991019764):+(-8.429751080849945),C$(10.067661995777765):+(0),C$(5.439580991019764):+(8.429751080849945),C$(-4.189625690968807):+(9.109227893755337),C$(-9.966909834129453):+(1.413722590498827)
  ,C$(-3.7245455049153224):+(-0.5118225699873846),C$(-1.5656258353157435):+(-3.297894836311237),C$(2.0327230070196656):+(-3.0518977991518),C$(3.7621956910836314):+(0),C$(2.0327230070196656):+(3.0518977991518),C$(-1.5656258353157435):+(3.297894836311237),C$(-3.7245455049153224):+(0.5118225699873846)
  ,C$(-1.5276382501165433):+(-0.1658444019189788),C$(-0.64214812471552):+(-1.0686074213827783),C$(0.8337300251311491):+(-0.9888977057628651),C$(1.5430806348152437):+(0),C$(0.8337300251311491):+(0.9888977057628651),C$(-0.64214812471552):+(1.0686074213827783),C$(-1.5276382501165433):+(0.1658444019189788)
  ,C$(-0.9899924966004454):+(0),C$(-0.4161468365471424):+(0),C$(0.5403023058681398):+(0),C$(1):+(0),C$(0.5403023058681398):+(0),C$(-0.4161468365471424):+(0),C$(-0.9899924966004454):+(0)
  ,C$(-1.5276382501165433):+(0.1658444019189788),C$(-0.64214812471552):+(1.0686074213827783),C$(0.8337300251311491):+(0.9888977057628651),C$(1.5430806348152437):+(0),C$(0.8337300251311491):+(-0.9888977057628651),C$(-0.64214812471552):+(-1.0686074213827783),C$(-1.5276382501165433):+(-0.1658444019189788)
  ,C$(-3.7245455049153224):+(0.5118225699873846),C$(-1.5656258353157435):+(3.297894836311237),C$(2.0327230070196656):+(3.0518977991518),C$(3.7621956910836314):+(0),C$(2.0327230070196656):+(-3.0518977991518),C$(-1.5656258353157435):+(-3.297894836311237),C$(-3.7245455049153224):+(-0.5118225699873846)
  ,C$(-9.966909834129453):+(1.413722590498827),C$(-4.189625690968807):+(9.109227893755337),C$(5.439580991019764):+(8.429751080849945),C$(10.067661995777765):+(0),C$(5.439580991019764):+(-8.429751080849945),C$(-4.189625690968807):+(-9.109227893755337),C$(-9.966909834129453):+(-1.413722590498827)
  ]
tans=
  [C$(0.0013786327196592897):+(-0.9952503011786029),C$(0.0037640256415042484):+(-1.0032386273536098),C$(-0.004517137276658426):+(-1.002054988245812),C$(0):+(-0.9950547536867306),C$(0.004517137276658426):+(-1.002054988245812),C$(-0.0037640256415042484):+(-1.0032386273536098),C$(-0.0013786327196592897):+(-0.9952503011786029)
  ,C$(0.009884375038322497):+(-0.9653858790221332),C$(0.028392952868232298):+(-1.0238355945704727),C$(-0.03381282607989671):+(-1.0147936161466338),C$(0):+(-0.964027580075817),C$(0.03381282607989671):+(-1.0147936161466338),C$(-0.028392952868232298):+(-1.0238355945704727),C$(-0.009884375038322497):+(-0.9653858790221332)
  ,C$(0.05916853956605073):+(-0.7680176472869112),C$(0.24345820118572525):+(-1.16673625724092),C$(-0.2717525853195118):+(-1.0839233273386946),C$(0):+(-0.761594155955765),C$(0.2717525853195118):+(-1.0839233273386946),C$(-0.24345820118572525):+(-1.16673625724092),C$(-0.05916853956605073):+(-0.7680176472869112)
  ,C$(0.1425465430742778):+(0),C$(2.185039863261519):+(0),C$(-1.557407724654902):+(0),C$(0):+(0),C$(1.557407724654902):+(0),C$(-2.185039863261519):+(0),C$(-0.1425465430742778):+(0)
  ,C$(0.05916853956605073):+(0.7680176472869112),C$(0.24345820118572525):+(1.16673625724092),C$(-0.2717525853195118):+(1.0839233273386946),C$(0):+(0.761594155955765),C$(0.2717525853195118):+(1.0839233273386946),C$(-0.24345820118572525):+(1.16673625724092),C$(-0.05916853956605073):+(0.7680176472869112)
  ,C$(0.009884375038322497):+(0.9653858790221332),C$(0.028392952868232298):+(1.0238355945704727),C$(-0.03381282607989671):+(1.0147936161466338),C$(0):+(0.964027580075817),C$(0.03381282607989671):+(1.0147936161466338),C$(-0.028392952868232298):+(1.0238355945704727),C$(-0.009884375038322497):+(0.9653858790221332)
  ,C$(0.0013786327196592897):+(0.9952503011786029),C$(0.0037640256415042484):+(1.0032386273536098),C$(-0.004517137276658426):+(1.002054988245812),C$(0):+(0.9950547536867306),C$(0.004517137276658426):+(1.002054988245812),C$(-0.0037640256415042484):+(1.0032386273536098),C$(-0.0013786327196592897):+(0.9952503011786029)
  ]
asins=
  [C$(-0.7715181921218484):+(-2.138622086316221),C$(-0.5706527843210994):+(-1.9833870299165355),C$(-0.30760364953071123):+(-1.8641615441578825),C$(0):+(-1.8184464592320668),C$(0.30760364953071123):+(-1.8641615441578825),C$(0.5706527843210994):+(-1.9833870299165355),C$(0.7715181921218484):+(-2.138622086316221)
  ,C$(-0.9646585044076028):+(-1.9686379257930964),C$(-0.754249144698046):+(-1.7343245214879666),C$(-0.42707858639247614):+(-1.5285709194809982),C$(0):+(-1.4436354751788103),C$(0.42707858639247614):+(-1.5285709194809982),C$(0.754249144698046):+(-1.7343245214879666),C$(0.9646585044076028):+(-1.9686379257930964)
  ,C$(-1.2330952175293441):+(-1.8241987021938828),C$(-1.0634400235777521):+(-1.4693517443681854),C$(-0.6662394324925152):+(-1.0612750619050357),C$(0):+(-0.881373587019543),C$(0.6662394324925152):+(-1.0612750619050357),C$(1.0634400235777521):+(-1.4693517443681854),C$(1.2330952175293441):+(-1.8241987021938828)
  ,C$(-1.5707963267948966):+(1.762747174039086),C$(-1.5707963267948966):+(1.3169578969248166),C$(-1.5707963267948966):+(0),C$(0):+(0),C$(1.5707963267948966):+(0),C$(1.5707963267948966):+(-1.3169578969248166),C$(1.5707963267948966):+(-1.762747174039086)
  ,C$(-1.2330952175293441):+(1.8241987021938828),C$(-1.0634400235777521):+(1.4693517443681854),C$(-0.6662394324925152):+(1.0612750619050357),C$(0):+(0.881373587019543),C$(0.6662394324925152):+(1.0612750619050357),C$(1.0634400235777521):+(1.4693517443681854),C$(1.2330952175293441):+(1.8241987021938828)
  ,C$(-0.9646585044076028):+(1.9686379257930964),C$(-0.754249144698046):+(1.7343245214879666),C$(-0.42707858639247614):+(1.5285709194809982),C$(0):+(1.4436354751788103),C$(0.42707858639247614):+(1.5285709194809982),C$(0.754249144698046):+(1.7343245214879666),C$(0.9646585044076028):+(1.9686379257930964)
  ,C$(-0.7715181921218484):+(2.138622086316221),C$(-0.5706527843210994):+(1.9833870299165355),C$(-0.30760364953071123):+(1.8641615441578825),C$(0):+(1.8184464592320668),C$(0.30760364953071123):+(1.8641615441578825),C$(0.5706527843210994):+(1.9833870299165355),C$(0.7715181921218484):+(2.138622086316221)
  ]
acoss=
  [C$(2.3423145189167447):+(2.138622086316221),C$(2.141449111115996):+(1.9833870299165355),C$(1.8783999763256076):+(1.8641615441578825),C$(1.5707963267948966):+(1.8184464592320668),C$(1.2631926772641855):+(1.8641615441578825),C$(1.0001435424737972):+(1.9833870299165355),C$(0.7992781346730483):+(2.138622086316221)
  ,C$(2.5354548312024994):+(1.9686379257930964),C$(2.3250454714929427):+(1.7343245214879666),C$(1.9978749131873728):+(1.5285709194809982),C$(1.5707963267948966):+(1.4436354751788103),C$(1.1437177404024204):+(1.5285709194809982),C$(0.8165471820968505):+(1.7343245214879666),C$(0.6061378223872939):+(1.9686379257930964)
  ,C$(2.8038915443242405):+(1.8241987021938828),C$(2.6342363503726487):+(1.4693517443681854),C$(2.2370357592874117):+(1.0612750619050357),C$(1.5707963267948966):+(0.881373587019543),C$(0.9045568943023814):+(1.0612750619050357),C$(0.5073563032171445):+(1.4693517443681854),C$(0.33770110926555247):+(1.8241987021938828)
  ,C$(3.141592653589793):+(-1.762747174039086),C$(3.141592653589793):+(-1.3169578969248166),C$(3.141592653589793):+(0),C$(1.5707963267948966):+(0),C$(0):+(0),C$(0):+(1.3169578969248166),C$(0):+(1.762747174039086)
  ,C$(2.8038915443242405):+(-1.8241987021938828),C$(2.6342363503726487):+(-1.4693517443681854),C$(2.2370357592874117):+(-1.0612750619050357),C$(1.5707963267948966):+(-0.881373587019543),C$(0.9045568943023814):+(-1.0612750619050357),C$(0.5073563032171445):+(-1.4693517443681854),C$(0.33770110926555247):+(-1.8241987021938828)
  ,C$(2.5354548312024994):+(-1.9686379257930964),C$(2.3250454714929427):+(-1.7343245214879666),C$(1.9978749131873728):+(-1.5285709194809982),C$(1.5707963267948966):+(-1.4436354751788103),C$(1.1437177404024204):+(-1.5285709194809982),C$(0.8165471820968505):+(-1.7343245214879666),C$(0.6061378223872939):+(-1.9686379257930964)
  ,C$(2.3423145189167447):+(-2.138622086316221),C$(2.141449111115996):+(-1.9833870299165355),C$(1.8783999763256076):+(-1.8641615441578825),C$(1.5707963267948966):+(-1.8184464592320668),C$(1.2631926772641855):+(-1.8641615441578825),C$(1.0001435424737972):+(-1.9833870299165355),C$(0.7992781346730483):+(-2.138622086316221)
  ]
atans=
  [C$(-1.4011500195678743):+(-0.16348161685166604),C$(-1.4099210495965755):+(-0.22907268296853875),C$(-1.4614618538579256):+(-0.30594385790552897),C$(-1.5707963267948966):+(-0.34657359027997264),C$(1.4614618538579256):+(-0.30594385790552897),C$(1.4099210495965755):+(-0.22907268296853875),C$(1.4011500195678743):+(-0.16348161685166604)
  ,C$(-1.3389725222944935):+(-0.14694666622552968),C$(-1.3112232696716353):+(-0.23887786125685906),C$(-1.3389725222944937):+(-0.4023594781085251),C$(-1.5707963267948966):+(-0.5493061443340549),C$(1.3389725222944937):+(-0.4023594781085251),C$(1.3112232696716353):+(-0.23887786125685906),C$(1.3389725222944935):+(-0.14694666622552968)
  ,C$(-1.2767950250211129):+(-0.09193119503132932),C$(-1.1780972450961724):+(-0.17328679513998638),C$(-1.0172219678978514):+(-0.4023594781085251),Err,C$(1.0172219678978514):+(-0.4023594781085251),C$(1.1780972450961724):+(-0.17328679513998638),C$(1.2767950250211129):+(-0.09193119503132932)
  ,C$(-1.2490457723982544):+(0),C$(-1.1071487177940904):+(0),C$(-0.7853981633974483):+(0),C$(0):+(0),C$(0.7853981633974483):+(0),C$(1.1071487177940904):+(0),C$(1.2490457723982544):+(0)
  ,C$(-1.2767950250211129):+(0.09193119503132935),C$(-1.1780972450961724):+(0.17328679513998635),C$(-1.0172219678978514):+(0.4023594781085251),Err,C$(1.0172219678978514):+(0.4023594781085251),C$(1.1780972450961724):+(0.17328679513998635),C$(1.2767950250211129):+(0.09193119503132935)
  ,C$(-1.3389725222944935):+(0.14694666622552968),C$(-1.3112232696716353):+(0.23887786125685906),C$(-1.3389725222944937):+(0.4023594781085251),C$(1.5707963267948966):+(0.5493061443340549),C$(1.3389725222944937):+(0.4023594781085251),C$(1.3112232696716353):+(0.23887786125685906),C$(1.3389725222944935):+(0.14694666622552968)
  ,C$(-1.4011500195678743):+(0.16348161685166607),C$(-1.4099210495965755):+(0.22907268296853872),C$(-1.4614618538579256):+(0.30594385790552886),C$(1.5707963267948966):+(0.34657359027997264),C$(1.4614618538579256):+(0.30594385790552886),C$(1.4099210495965755):+(0.22907268296853872),C$(1.4011500195678743):+(0.16348161685166607)
  ]
sinhs=
  [C$(9.917621010017536):+(-1.4207485419881771),C$(3.59056458998578):+(-0.5309210862485197),C$(1.1634403637032504):+(-0.21775955162215221),C$(0):+(-0.1411200080598672),C$(-1.1634403637032504):+(-0.21775955162215221),C$(-3.59056458998578):+(-0.5309210862485197),C$(-9.917621010017536):+(-1.4207485419881771)
  ,C$(4.168906959966565):+(-9.15449914691143),C$(1.5093064853236158):+(-3.4209548611170133),C$(0.4890562590412937):+(-1.4031192506220405),C$(0):+(-0.9092974268256817),C$(-0.4890562590412937):+(-1.4031192506220405),C$(-1.5093064853236158):+(-3.4209548611170133),C$(-4.168906959966565):+(-9.15449914691143)
  ,C$(-5.412680923178193):+(-8.471645454300148),C$(-1.9596010414216063):+(-3.165778513216168),C$(-0.6349639147847361):+(-1.2984575814159773),C$(0):+(-0.8414709848078965),C$(0.6349639147847361):+(-1.2984575814159773),C$(1.9596010414216063):+(-3.165778513216168),C$(5.412680923178193):+(-8.471645454300148)
  ,C$(-10.017874927409903):+(0),C$(-3.626860407847019):+(0),C$(-1.1752011936438014):+(0),C$(0):+(0),C$(1.1752011936438014):+(0),C$(3.626860407847019):+(0),C$(10.017874927409903):+(0)
  ,C$(-5.412680923178193):+(8.471645454300148),C$(-1.9596010414216063):+(3.165778513216168),C$(-0.6349639147847361):+(1.2984575814159773),C$(0):+(0.8414709848078965),C$(0.6349639147847361):+(1.2984575814159773),C$(1.9596010414216063):+(3.165778513216168),C$(5.412680923178193):+(8.471645454300148)
  ,C$(4.168906959966565):+(9.15449914691143),C$(1.5093064853236158):+(3.4209548611170133),C$(0.4890562590412937):+(1.4031192506220405),C$(0):+(0.9092974268256817),C$(-0.4890562590412937):+(1.4031192506220405),C$(-1.5093064853236158):+(3.4209548611170133),C$(-4.168906959966565):+(9.15449914691143)
  ,C$(9.917621010017536):+(1.4207485419881771),C$(3.59056458998578):+(0.5309210862485197),C$(1.1634403637032504):+(0.21775955162215221),C$(0):+(0.1411200080598672),C$(-1.1634403637032504):+(0.21775955162215221),C$(-3.59056458998578):+(0.5309210862485197),C$(-9.917621010017536):+(1.4207485419881771)
  ]
coshs=
  [C$(-9.966909834129453):+(1.413722590498827),C$(-3.7245455049153224):+(0.5118225699873846),C$(-1.5276382501165433):+(0.1658444019189788),C$(-0.9899924966004454):+(0),C$(-1.5276382501165433):+(-0.1658444019189788),C$(-3.7245455049153224):+(-0.5118225699873846),C$(-9.966909834129453):+(-1.413722590498827)
  ,C$(-4.189625690968807):+(9.109227893755337),C$(-1.5656258353157435):+(3.297894836311237),C$(-0.64214812471552):+(1.0686074213827783),C$(-0.4161468365471424):+(0),C$(-0.64214812471552):+(-1.0686074213827783),C$(-1.5656258353157435):+(-3.297894836311237),C$(-4.189625690968807):+(-9.109227893755337)
  ,C$(5.439580991019764):+(8.429751080849945),C$(2.0327230070196656):+(3.0518977991518),C$(0.8337300251311491):+(0.9888977057628651),C$(0.5403023058681398):+(0),C$(0.8337300251311491):+(-0.9888977057628651),C$(2.0327230070196656):+(-3.0518977991518),C$(5.439580991019764):+(-8.429751080849945)
  ,C$(10.067661995777765):+(0),C$(3.7621956910836314):+(0),C$(1.5430806348152437):+(0),C$(1):+(0),C$(1.5430806348152437):+(0),C$(3.7621956910836314):+(0),C$(10.067661995777765):+(0)
  ,C$(5.439580991019764):+(-8.429751080849945),C$(2.0327230070196656):+(-3.0518977991518),C$(0.8337300251311491):+(-0.9888977057628651),C$(0.5403023058681398):+(0),C$(0.8337300251311491):+(0.9888977057628651),C$(2.0327230070196656):+(3.0518977991518),C$(5.439580991019764):+(8.429751080849945)
  ,C$(-4.189625690968807):+(-9.109227893755337),C$(-1.5656258353157435):+(-3.297894836311237),C$(-0.64214812471552):+(-1.0686074213827783),C$(-0.4161468365471424):+(0),C$(-0.64214812471552):+(1.0686074213827783),C$(-1.5656258353157435):+(3.297894836311237),C$(-4.189625690968807):+(9.109227893755337)
  ,C$(-9.966909834129453):+(-1.413722590498827),C$(-3.7245455049153224):+(-0.5118225699873846),C$(-1.5276382501165433):+(-0.1658444019189788),C$(-0.9899924966004454):+(0),C$(-1.5276382501165433):+(0.1658444019189788),C$(-3.7245455049153224):+(0.5118225699873846),C$(-9.966909834129453):+(1.413722590498827)
  ]
tanhs=
  [C$(-0.9952503011786028):+(0.0013786327196592895),C$(-0.9653858790221331):+(0.009884375038322492),C$(-0.7680176472869112):+(0.05916853956605073),C$(0):+(0.1425465430742778),C$(0.7680176472869112):+(0.05916853956605073),C$(0.9653858790221331):+(0.009884375038322492),C$(0.9952503011786028):+(0.0013786327196592895)
  ,C$(-1.0032386273536098):+(0.0037640256415042476),C$(-1.0238355945704727):+(0.028392952868232284),C$(-1.16673625724092):+(0.24345820118572528),C$(0):+(2.1850398632615184),C$(1.16673625724092):+(0.24345820118572528),C$(1.0238355945704727):+(0.028392952868232284),C$(1.0032386273536098):+(0.0037640256415042476)
  ,C$(-1.002054988245812):+(-0.0045171372766584245),C$(-1.0147936161466335):+(-0.033812826079896684),C$(-1.0839233273386946):+(-0.27175258531951174),C$(0):+(-1.5574077246549018),C$(1.0839233273386946):+(-0.27175258531951174),C$(1.0147936161466335):+(-0.033812826079896684),C$(1.002054988245812):+(-0.0045171372766584245)
  ,C$(-0.9950547536867304):+(0),C$(-0.9640275800758168):+(0),C$(-0.761594155955765):+(0),C$(0):+(0),C$(0.761594155955765):+(0),C$(0.9640275800758168):+(0),C$(0.9950547536867304):+(0)
  ,C$(-1.002054988245812):+(0.0045171372766584245),C$(-1.0147936161466335):+(0.033812826079896684),C$(-1.0839233273386946):+(0.27175258531951174),C$(0):+(1.5574077246549018),C$(1.0839233273386946):+(0.27175258531951174),C$(1.0147936161466335):+(0.033812826079896684),C$(1.002054988245812):+(0.0045171372766584245)
  ,C$(-1.0032386273536098):+(-0.0037640256415042476),C$(-1.0238355945704727):+(-0.028392952868232284),C$(-1.16673625724092):+(-0.24345820118572528),C$(0):+(-2.1850398632615184),C$(1.16673625724092):+(-0.24345820118572528),C$(1.0238355945704727):+(-0.028392952868232284),C$(1.0032386273536098):+(-0.0037640256415042476)
  ,C$(-0.9952503011786028):+(-0.0013786327196592895),C$(-0.9653858790221331):+(-0.009884375038322492),C$(-0.7680176472869112):+(-0.05916853956605073),C$(0):+(-0.1425465430742778),C$(0.7680176472869112):+(-0.05916853956605073),C$(0.9653858790221331):+(-0.009884375038322492),C$(0.9952503011786028):+(-0.0013786327196592895)
  ]
asinhs=
  [C$(-2.138622086316221):+(-0.7715181921218484),C$(-1.9686379257930964):+(-0.9646585044076028),C$(-1.8241987021938828):+(-1.2330952175293441),C$(-1.762747174039086):+(-1.5707963267948966),C$(1.8241987021938828):+(-1.2330952175293441),C$(1.9686379257930964):+(-0.9646585044076028),C$(2.138622086316221):+(-0.7715181921218484)
  ,C$(-1.9833870299165355):+(-0.5706527843210994),C$(-1.7343245214879666):+(-0.754249144698046),C$(-1.4693517443681854):+(-1.0634400235777521),C$(-1.3169578969248166):+(-1.5707963267948966),C$(1.4693517443681854):+(-1.0634400235777521),C$(1.7343245214879666):+(-0.754249144698046),C$(1.9833870299165355):+(-0.5706527843210994)
  ,C$(-1.8641615441578825):+(-0.30760364953071123),C$(-1.5285709194809982):+(-0.42707858639247614),C$(-1.0612750619050357):+(-0.6662394324925152),C$(0):+(-1.5707963267948966),C$(1.0612750619050357):+(-0.6662394324925152),C$(1.5285709194809982):+(-0.42707858639247614),C$(1.8641615441578825):+(-0.30760364953071123)
  ,C$(-1.8184464592320668):+(0),C$(-1.4436354751788103):+(0),C$(-0.881373587019543):+(0),C$(0):+(0),C$(0.881373587019543):+(0),C$(1.4436354751788103):+(0),C$(1.8184464592320668):+(0)
  ,C$(-1.8641615441578825):+(0.30760364953071123),C$(-1.5285709194809982):+(0.42707858639247614),C$(-1.0612750619050357):+(0.6662394324925152),C$(0):+(1.5707963267948966),C$(1.0612750619050357):+(0.6662394324925152),C$(1.5285709194809982):+(0.42707858639247614),C$(1.8641615441578825):+(0.30760364953071123)
  ,C$(-1.9833870299165355):+(0.5706527843210994),C$(-1.7343245214879666):+(0.754249144698046),C$(-1.4693517443681854):+(1.0634400235777521),C$(1.3169578969248166):+(1.5707963267948966),C$(1.4693517443681854):+(1.0634400235777521),C$(1.7343245214879666):+(0.754249144698046),C$(1.9833870299165355):+(0.5706527843210994)
  ,C$(-2.138622086316221):+(0.7715181921218484),C$(-1.9686379257930964):+(0.9646585044076028),C$(-1.8241987021938828):+(1.2330952175293441),C$(1.762747174039086):+(1.5707963267948966),C$(1.8241987021938828):+(1.2330952175293441),C$(1.9686379257930964):+(0.9646585044076028),C$(2.138622086316221):+(0.7715181921218484)
  ]
acoshs=
  [C$(2.138622086316221):+(-2.3423145189167447),C$(1.9833870299165355):+(-2.141449111115996),C$(1.8641615441578825):+(-1.8783999763256076),C$(1.8184464592320668):+(-1.5707963267948966),C$(1.8641615441578825):+(-1.2631926772641855),C$(1.9833870299165355):+(-1.0001435424737972),C$(2.138622086316221):+(-0.7992781346730483)
  ,C$(1.9686379257930964):+(-2.5354548312024994),C$(1.7343245214879666):+(-2.3250454714929427),C$(1.5285709194809982):+(-1.9978749131873728),C$(1.4436354751788103):+(-1.5707963267948966),C$(1.5285709194809982):+(-1.1437177404024204),C$(1.7343245214879666):+(-0.8165471820968505),C$(1.9686379257930964):+(-0.6061378223872939)
  ,C$(1.8241987021938828):+(-2.8038915443242405),C$(1.4693517443681854):+(-2.6342363503726487),C$(1.0612750619050357):+(-2.2370357592874117),C$(0.881373587019543):+(-1.5707963267948966),C$(1.0612750619050357):+(-0.9045568943023814),C$(1.4693517443681854):+(-0.5073563032171445),C$(1.8241987021938828):+(-0.33770110926555247)
  ,C$(1.762747174039086):+(3.141592653589793),C$(1.3169578969248166):+(3.141592653589793),C$(0):+(3.141592653589793),C$(0):+(1.5707963267948966),C$(0):+(0),C$(1.3169578969248166):+(0),C$(1.762747174039086):+(0)
  ,C$(1.8241987021938828):+(2.8038915443242405),C$(1.4693517443681854):+(2.6342363503726487),C$(1.0612750619050357):+(2.2370357592874117),C$(0.881373587019543):+(1.5707963267948966),C$(1.0612750619050357):+(0.9045568943023814),C$(1.4693517443681854):+(0.5073563032171445),C$(1.8241987021938828):+(0.33770110926555247)
  ,C$(1.9686379257930964):+(2.5354548312024994),C$(1.7343245214879666):+(2.3250454714929427),C$(1.5285709194809982):+(1.9978749131873728),C$(1.4436354751788103):+(1.5707963267948966),C$(1.5285709194809982):+(1.1437177404024204),C$(1.7343245214879666):+(0.8165471820968505),C$(1.9686379257930964):+(0.6061378223872939)
  ,C$(2.138622086316221):+(2.3423145189167447),C$(1.9833870299165355):+(2.141449111115996),C$(1.8641615441578825):+(1.8783999763256076),C$(1.8184464592320668):+(1.5707963267948966),C$(1.8641615441578825):+(1.2631926772641855),C$(1.9833870299165355):+(1.0001435424737972),C$(2.138622086316221):+(0.7992781346730483)
  ]
atanhs=
  [C$(-0.16348161685166604):+(-1.4011500195678743),C$(-0.14694666622552968):+(-1.3389725222944935),C$(-0.09193119503132932):+(-1.2767950250211129),C$(0):+(-1.2490457723982544),C$(0.09193119503132935):+(-1.2767950250211129),C$(0.14694666622552968):+(-1.3389725222944935),C$(0.16348161685166607):+(-1.4011500195678743)
  ,C$(-0.22907268296853875):+(-1.4099210495965755),C$(-0.23887786125685906):+(-1.3112232696716353),C$(-0.17328679513998638):+(-1.1780972450961724),C$(0):+(-1.1071487177940904),C$(0.17328679513998635):+(-1.1780972450961724),C$(0.23887786125685906):+(-1.3112232696716353),C$(0.22907268296853872):+(-1.4099210495965755)
  ,C$(-0.30594385790552897):+(-1.4614618538579256),C$(-0.4023594781085251):+(-1.3389725222944937),C$(-0.4023594781085251):+(-1.0172219678978514),C$(0):+(-0.7853981633974483),C$(0.4023594781085251):+(-1.0172219678978514),C$(0.4023594781085251):+(-1.3389725222944937),C$(0.30594385790552886):+(-1.4614618538579256)
  ,C$(-0.34657359027997264):+(1.5707963267948966),C$(-0.5493061443340549):+(1.5707963267948966),Err,C$(0):+(0),Err,C$(0.5493061443340549):+(-1.5707963267948966),C$(0.34657359027997264):+(-1.5707963267948966)
  ,C$(-0.30594385790552897):+(1.4614618538579256),C$(-0.4023594781085251):+(1.3389725222944937),C$(-0.4023594781085251):+(1.0172219678978514),C$(0):+(0.7853981633974483),C$(0.4023594781085251):+(1.0172219678978514),C$(0.4023594781085251):+(1.3389725222944937),C$(0.30594385790552886):+(1.4614618538579256)
  ,C$(-0.22907268296853875):+(1.4099210495965755),C$(-0.23887786125685906):+(1.3112232696716353),C$(-0.17328679513998638):+(1.1780972450961724),C$(0):+(1.1071487177940904),C$(0.17328679513998635):+(1.1780972450961724),C$(0.23887786125685906):+(1.3112232696716353),C$(0.22907268296853872):+(1.4099210495965755)
  ,C$(-0.16348161685166604):+(1.4011500195678743),C$(-0.14694666622552968):+(1.3389725222944935),C$(-0.09193119503132932):+(1.2767950250211129),C$(0):+(1.2490457723982544),C$(0.09193119503132935):+(1.2767950250211129),C$(0.14694666622552968):+(1.3389725222944935),C$(0.16348161685166607):+(1.4011500195678743)
  ]

{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables     #-}

--An (almost) exact copy of this (and TestValue, Double0) is in ghc\testsuite\tests\numeric\should_run\ComplexCalcs.hs

-- Tests of fixes to complex calculations
-- Most fixed are from #20425, but some tests test fixes to earlier bugs.

--import Data.Complex
import MyComplex
import Data.Char
import TestValue
import Double0

main :: IO ()
main = do
  putFails "Bug Fix Tests Double" (bugFixTests @Double)
  putFails "Bug Fix Tests Float"  (bugFixTests @Float)
  putFails "Bug Fix Tests D0"     (bugFixTests @D0)

bugFixTests :: forall a. (RealFloat a, Show a) => [Test a]
bugFixTests = concat
  [--tests for historic fixes
    testC' "#4228" Atanh  ((-1):+0   )  (E (-inf), E   0  ) Nothing
  , testC' "#4228" Atanh  (( 1):+0   )  (E ( inf), E   0  ) Nothing
  , testC' "#8532" Acosh  ((-1):+0   )  (E 0     , E   pi ) Nothing
  , testC' "#8532" Acosh  ((-1):+(-0))  (E 0     , E (-pi)) (Just (E 0, E pi))
  ,let z = 0:+0 in testC "#8539 #1 (**2)"  (show z) (z**2   ) (E 0, E 0) Nothing --MORE NEEDED FOR 8539? (Though I've not changed **)

  --the following all fixed by #20425

  --Complex->Float fixes
  ,let z = mn:+0       in [Test "magnitude" (show z) (magnitude z)  (E mn)]
  ,let z = 0:+mn       in [Test "magnitude" (show z) (magnitude z)  (E mn)]
  ,let z = (-0):+0     in [Test "phase"     (show z) (phase z)      (E $ if isieee then  pi else 0)]
  ,let z = (-0):+(-0)  in [Test "phase"     (show z) (phase z)      (E $ if isieee then -pi else 0)]
                          
  --Complex->Complex fixes (in order listed in https://gitlab.haskell.org/ghc/ghc/-/issues/20425)
  --neg zero
  , testC' "11" Log   (  0  :+  0  ) (E (-inf)      , E   0  ) Nothing
  , testC' "11" Log   (  0  :+(-0) ) (E (-inf)      , E (-0) ) Nothing
  , testC' "12" Tan   (  0  :+  3  ) (E   0         , R      ) Nothing
  , testC' "12" Tan   ((-0) :+  3  ) (E (-0  )      , R      ) Nothing
  , testC' "13" Tanh  (  3  :+  0  ) (R             , E   0  ) Nothing
  , testC' "13" Tanh  (  3  :+(-0) ) (R             , E (-0) ) Nothing
  , testC' "14" Acosh (  3  :+  0  ) (R             , E   0  ) Nothing
  , testC' "14" Acosh (  3  :+(-0) ) (R             , E (-0) ) Nothing
  , testC' "15" Sqrt  ( mx  :+ mx  ) (R             , R      ) Nothing
  , testC' "16" Log   ( mx  :+ mx  ) (R             , R      ) Nothing
  , testC' "17" Tan   ( 0   :+ 800 ) (E 0           , E 1    ) Nothing
  , testC' "18" Tanh  (800  :+ 0   ) (E 1           , E 0    ) Nothing
  , testC' "19" Asin  (mx   :+ mx  ) (R             , R      ) Nothing  --covers 19 & 20
  , testC' "21" Acos  (mx   :+ mx  ) (R             , R      ) Nothing
  , testC' "22" Acosh (mx   :+ mx  ) (R             , R      ) Nothing
  , testC' "23" Sqrt  ((-4) :+  0  ) (E  0          , E 2    ) Nothing
  , testC' "23" Sqrt  ((-4) :+(-0) ) (E  0          , E (-2) ) (Just (E 0, E 2))
  , testC' "24" Asin  ((-2) :+  0  ) (N             , P      ) Nothing
  , testC' "24" Asin  ((-2) :+(-0) ) (N             , N      ) (Just (N, P))
  , testC' "25" Acos  (  3  :+  0  ) (E 0           , N      ) (Just (E 0, P))
  , testC' "25" Acos  (  3  :+(-0) ) (E 0           , P      ) Nothing
  , testC' "26" Atan  (  0  :+  3  ) (P             , P      ) Nothing
  , testC' "26" Atan  ((-0) :+  3  ) (N             , P      ) (Just (P, P))
  , testC' "27" Asinh (  0  :+  3  ) (P             , P      ) Nothing
  , testC' "27" Asinh ((-0) :+  3  ) (N             , P      ) (Just (P, P))
  , testC' "28" Atanh (  3  :+  0  ) (P             , P      ) (Just (P, N))
  , testC' "28" Atanh (  3  :+(-0) ) (P             , N      ) Nothing
  , testC' "29" Atan  (  0  :+1e-20) (E 0           , E 1e-20) Nothing
  , testC' "29" Atan  (1e-20:+0    ) (E 1e-20       , E 0    ) Nothing
  , testC' "30" Tan   (pi/2 :+0    ) (A $ tan (pi/2), E 0    ) Nothing

  ,let z1 = (-1531.9375):+0 --For Float, original code gave significantly different results for z1 & z2.
       z2 = z1 - 0.0001
                       in [Test "31: asin"   (show z1) (imagPart $ asin z1) (X 2 $ imagPart $ asin z2)]
  ,let z = 0:+(-0)     in testC "32: signum" (show z)  (signum z) (E 0, E $ -0) Nothing
  ,let z = mx:+mx      in testC "33: signum" (show z)  (signum z) (A $ 1/sqrt 2, A $ 1 / sqrt 2) Nothing
  ,let z = 2/mx:+0     in testC "34:(1:+0)/" (show z)  ((1:+0)/z) (A $ mx/2, E 0) Nothing 
  ,let z = (-0):+0     in [Test "50: phase"  (show z)  (phase z)  (E $ if isieee then pi else 0)]
  ,let z = (-0):+0     in testC "51: log"    (show z)  (log z)    (E $ -inf, E pi) (Just (E $ -inf, E 0))
  ,let z = inf:+inf    in testC "52: signum" (show z)  (signum z) (A $ 1/sqrt 2, A $ 1 / sqrt 2) Nothing
  ]
  where isieee = isIEEE (undefined :: a)

inf, mn, mx :: forall a.RealFloat a => a
inf = 1/0

mx = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e
    a = undefined :: a

mn = encodeFloat 1 n where
    e = floatDigits a
    (e', _) = floatRange a
    n = e' - e
    a = undefined :: a

data Function = Sq | Sqrt | Exp | Log
              | Sin | Asin | Cos | Acos | Tan | Atan
              | Sinh | Asinh | Cosh | Acosh | Tanh | Atanh
  deriving (Eq, Enum, Bounded, Show)

fnName :: Function -> String
fnName = map toLower . show

fnF :: RealFloat a => Function -> Complex a -> Complex a
fnF Sq    = undefined
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

type CExp a = (Expected a, Expected a)  --(expected real, expected imag)

--a more compact version for many situations
testC' :: (RealFloat a, Show a) => String -> Function -> Complex a -> CExp a -> Maybe (CExp a) -> [Test a]
testC' label f z = testC (label ++ ": " ++ fnName f) (show z) (fnF f z)

--create a list of two tests, one for the realPart and one for the imagPart.
testC :: RealFloat a => String -> String -> Complex a -> CExp a -> (Maybe (CExp a)) -> [Test a]
testC name val (x:+y) = testC2 where
  testC2 _    (Just (u,v)) | not (isIEEE x) = tests u v
  testC2 (u,v) _                            = tests u v
  tests u v = [ Test name (val++"(R)") x u
              , Test name (val++"(I)") y v
              ]

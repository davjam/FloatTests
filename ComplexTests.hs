{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ExplicitForAll, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

import HasVal
import Double0

--Switch between these to test old / new results:
--import Data.Complex
import MyComplex 

{- TO ADD:
sqrt((-0):+(-0)), etc.
complex functions with zero imag match real functions.
issue 8532: ensure acosh((-1):+0) = 0:+pi.
log (0 :+ (-0)) == (-Infinity) :+ (-0.0).
mkPolar is inverse of polar, for all +/- 0, +/- pi combinations.
check with types that don't support -ve zeros.
Test TrigDiags with Float.
atanh with 1, -1, tiny values (less that rh).
-}

------------------------------------
--The functions to test
------------------------------------

data Function = Sqrt | Exp | Log
              | Sin | Asin | Cos | Acos | Tan | Atan
              | Sinh | Asinh | Cosh | Acosh | Tanh | Atanh
  deriving (Eq, Show, Enum, Bounded)

allFunctions :: [Function]
allFunctions = [minBound..maxBound]

fnName :: Function -> String
fnName Sqrt  = "sqrt"
fnName Exp   = "exp"
fnName Log   = "log"
fnName Sin   = "sin"
fnName Cos   = "cos"  
fnName Tan   = "tan"  
fnName Asin  = "asin" 
fnName Acos  = "acos" 
fnName Atan  = "atan" 
fnName Sinh  = "sinh" 
fnName Cosh  = "cosh" 
fnName Tanh  = "tanh" 
fnName Asinh = "asinh"
fnName Acosh = "acosh"
fnName Atanh = "atanh"

fnF :: RealFloat a => Function -> Complex a -> Complex a
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

--see https://stackoverflow.com/questions/69450017/mapping-over-rankntypes-functions for explanation of this apparent dup
fnR :: RealFloat a => Function -> a -> a  
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
fnR Asinh = asinh
fnR Acosh = acosh
fnR Atanh = atanh

data Quadrant = QI | QII | QIII | QIV
  deriving (Eq, Show)
  
branchCutPointQuadrant :: RealFloat a => Function -> Complex a -> Maybe Quadrant
branchCutPointQuadrant Sqrt  (x:+0) | x < 0    = Just QII
branchCutPointQuadrant Log   (x:+0) | x < 0    = Just QII
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

------------------------------------
--Tests
------------------------------------

main :: IO ()
main = do
  putFails "Real vs Complex D0"     (realCpxMatchTests @D0)
  putFails "Real vs Complex Double" (realCpxMatchTests @Double)
  putFails "Real vs Complex Float"  (realCpxMatchTests @Float)

  putFails "Conjugate D0"     (conjTests @D0     False)
  putFails "Conjugate Double" (conjTests @Double True )
  putFails "Conjugate Float"  (conjTests @Float  True )

  putFails "gnumericTests D0"     (gnumericTests @D0)
  putFails "gnumericTests Double" (gnumericTests @Double)
  putFails "gnumericTests Float"  (gnumericTests @Float)

--create a list of two tests, one for the realPart and one for the imagPart.
testC :: String -> String -> Complex a -> Expected a -> Expected a -> [Test a]
testC name val (x:+y) u v = [Test name (val++"(R)") x u, Test name (val++"(I)") y v]

------------------------------------
-- Basic tests (real vs complex, conjugation)

realCpxMatchTests :: (RealFloat a, Show a) => [Test a]
realCpxMatchTests = concat
  [ testC (fnName fn) (show z) fz (A fx) (A 0)
  | fn <- allFunctions
  , x <- xs
  , let fx = fnR fn x
  , not $ isNaN fx
  , let z = x :+ 0; fz = fnF fn z
  ]

conjTests :: forall a. (RealFloat a, Show a) => Bool -> [Test a]
conjTests fTestImagZeros = concat
  [ testC (fnName fn) (show z) (f $ conjugate z) (A u) (A v)
  | fn <- allFunctions
  , let f = fnF fn
  , x <- xs
  , y <- xs
  , if fTestImagZeros then True else y /= 0
  , let z = x :+ y
  , let (u:+v) = conjugate $ f z
  ]

xs :: RealFloat a => [a]
xs = [-5, -4, -pi, -3, e, -2, -pi/2, -1, -0, 0, 1, pi/2, 2, e, 3, pi, 4, 5]
  where e = exp 1

------------------------------------
-- Tests vs external calculation (by Gnumeric spreadsheet).

--All of these expected values come from gnumeric, which doesn't support negative zeros.
--When testing a value on a branch cut with a signed zero, 
--make sure we push it into the quadrant with continuity before testing against gnumeric.

gnumericTests :: (RealFloat a, Show a) => [Test a]
gnumericTests = concatMap testFn allFunctions where
  testFn fn = concat $ zipWith testVal zs (fnYs fn) where
    testVal z (C (u:+v)) | isIEEE u  = testC (fnName fn) (show z') (fnF fn z') (A u) (A v)
                         | otherwise = testC (fnName fn) (show z ) (fnF fn z ) (A u) (A v)
      where z' | Just q <- branchCutPointQuadrant fn z = pushToQuadrant q z
               | otherwise                             =                  z
    testVal _ Err        = []
  zs = [x:+y|y<-xs2,x<-xs2] where xs2 = [-3,-2,-1,0,1,2,3]

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

data External a = C a | Err --e.g. a calculation in Excel, Gnumeric, etc.

fnYs :: RealFloat a => Function -> [External (Complex a)]
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

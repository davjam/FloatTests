{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use tan" #-}

import HasVal
import Double0

--uncomment these to test revised functions.
import Prelude hiding (asinh, atanh)
import MyFloat (asinh, atanh)

main :: IO ()
main = do
  putFails "Inverses D0"     (inveseTests $ invDbls @D0)
  putFails "Inverses Double" (inveseTests $ invDbls @Double)
  putFails "Inverses Float"  (inveseTests invFlts)

  putFails "Special Value D0"     (specValTests @D0)
  putFails "Special Value Double" (specValTests @Double)
  putFails "Special Value Float"  (specValTests @Float )

  putFails "Identity D0"     (identityTests @D0)
  putFails "Identity Double" (identityTests @Double)
  putFails "Identity Float"  (identityTests @Float )

  putFails "Algebraic Values D0"     (algValTests @D0)
  putFails "Algebraic Values Double" (algValTests @Double)
  putFails "Algebraic Values Float"  (algValTests @Float )

  putFails "Large Trig D0"     (largeTrigTests @D0)
  putFails "Large Trig Double" (largeTrigTests @Double)
  putFails "Large Trig Float"  (largeTrigTests @Float )

  putFails "Monotonicity D0"     (monotonTests @D0)
  putFails "Monotonicity Double" (monotonTests @Double)
  putFails "Monotonicity Float"  (monotonTests @Float )


-----------------------------------------------------------------------------
-- INVERSE TESTS

inveseTests :: (RealFloat a, Show a) => [[a]] -> [Test a]
inveseTests vals =  [ Test name (show x) (fInv (f x)) (A x)
                    | ((name, f, fInv), xs) <- zip inverses vals
                    , x                     <- xs
                    ]

inverses :: RealFloat a => [(String, a -> a, a -> a)]
inverses = [( "recip"      , recip , recip )
           ,( "log.exp"    , exp   , log   )
           ,( "asin.sin"   , sin   , asin  )
           ,( "acos.cos"   , cos   , acos  )
           ,( "atan.tan"   , tan   , atan  )
           ,( "asinh.sinh" , sinh  , asinh )
           ,( "acosh.cosh" , cosh  , acosh )
           ,( "atanh.tanh" , tanh  , atanh )
           ]

invDbls :: (RealFloat a, Enum a) => [[a]]
invDbls =  [[-1e20, -1e3, -1, -1e-40, 1e-40, 1e90]
           ,[-10, -5 .. 300]
           ,[-1.5, -1.4 .. 1.5]
           ,[0, 0.1 .. 3]
           ,[-0.7, -0.6 .. 0.7]
           ,[-700, -672 .. 700]
           ,[0, 15 .. 700]
           ,[-0.99, -0.87 .. 0.9]
           ]

invFlts :: [[Float]]
invFlts =  [[-1e10, -10, -1, -1e-20, 1e-20, 1e30]
           ,[-10 .. 60]
           ,[-1.5, -1.4 .. 1.5]
           ,[0, 0.1 .. 3]
           ,[-0.7, -0.6 .. 0.7]
           ,[-80, -71 .. 80]
           ,[0, 15 .. 80]
           ,[-0.99, -0.87 .. 0.9]
           ]

-----------------------------------------------------------------------------
-- SPECIAL VALUE TESTS
-- See IEEE 754 Standards 9.2.1 Special Values for some of these.

specValTests :: (RealFloat a, Show a) => [Test a]
specValTests = [Test name (show x) (f x) expected | Fn name f exps <- fns, (x,expected) <- zip specVals exps]

data Fn a = Fn 
  String          --name
  (a -> a)        --function
  [Expected a]    --expected results

specVals :: RealFloat a => [a]
specVals =                              [  nan,     -inf , -mx       ,     -1      ,     -0  ,      0  ,     1     ,     mx   ,     inf ]

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
--These are values from WolframAlpha:
exp1   = 2.7182818284590452353602874713526624977572470936999595749669676277
expN1  = 0.3678794411714423215955237701614608674458111310317678345078368016
sin1   = 0.8414709848078965066525023216302989996225630607983710656727517099
cos1   = 0.5403023058681397174009366074429766037323104206179222276700972553
tan1   = 1.5574077246549022305069748074583601730872507723815200383839466056
sinh1  = 1.1752011936438014568823818505956008151557179813340958702295654130
cosh1  = 1.5430806348152437784779056207570616826015291123658637047374022147
tanh1  = 0.7615941559557648881194582826047935904127685972579365515968105001
asinh1 = 0.8813735870195430252326093249797923090281603282616354107532956086

--we can't define e.g. sqrtmx, since mx is different for Double and Float.

-----------------------------------------------------------------------------
-- TESTS FOR IDENTITIES.
-- https://en.wikipedia.org/wiki/Trigonometric_functions#Basic_identities

identityTests :: (RealFloat a, Enum a, Show a) => [Test a]
identityTests = [ Test name (show x) (f1 x) (A $ f2 x)
                | (name, f1, f2) <- identities
                , x <- smallNums ++ bigNums
                ]

identities :: RealFloat a => [(String, a -> a, a -> a)]
identities = [("sin -x == -sin x"  , sin . negate                             , negate . sin)
             ,("cos -x == cos x"   , cos . negate                             , cos)
             ,("tan -x == - tan x" , tan . negate                             , negate . tan)
             ,("sin^2 + cos^2 = 1" , \x -> (sin x)^(2::Int) + (cos x)^(2::Int), const 1)
             ,("tan=sin/cos"       , tan                                      , \x -> sin x / cos x)
             ,("sinh -x == -sinh x", sinh . negate                            , negate . sinh)
             ,("cosh -x == cosh x" , cosh . negate                            , cosh)
             ,("tanh -x == -tanh x", tanh . negate                            , negate . tanh)
             {-although mathematically true, these fail computationally even on small numbers:
             ,("cosh x + sinh x = exp x", \x -> cosh x + sinh x, exp)
             ,("cosh^2 - sinh^2 = 1", \x -> (cosh x)^(2::Int) - (sinh x)^(2::Int), const 1)
             -}
             ]

-----------------------------------------------------------------------------
-- ALGEBRAIC IDENTITY TESTS
-- https://en.wikipedia.org/wiki/Trigonometric_functions#Simple_algebraic_values

algValTests :: (RealFloat a, Show a) => [Test a]
algValTests = concat [[Test "sin" xName (sin x) (A sinx)
                      ,Test "cos" xName (cos x) (A cosx)
                      ]
                      | (xName, x, sinx, cosx) <- algVals
                      ]
                ++   [Test "tan" xName (tan x) (A $ sinx/cosx) | (xName, x, sinx, cosx) <- algVals, cosx /=0]

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

largeTrigTests :: (RealFloat a, Show a) => [Test a]
largeTrigTests =  [Test "sin" (show x) (sin x) (A y) | (x, y) <- zip bigNums largeSins]
               ++ [Test "cos" (show x) (cos x) (A y) | (x, y) <- zip bigNums largeCoss]

largeSins :: RealFloat a => [a]
largeSins = [-0.544021110889369813404747661851377281683643012916223891574184012   --  1e1
            ,-0.506365641109758793656557610459785432065032721290657323443392473   --  1e2
            , 0.8268795405320025602558874291092181412127249678477883209081232758  --  1e3
            ,-0.305614388888252141360910035232506974231850043861806239110155145   --  1e4
            , 0.0357487979720165093164705006958088290090456925781088968546167365  --  1e5
            ,-0.349993502171292952117652486780771469061406605328716273857059054   --  1e6
            , 0.4205477931907824912985065897409455951671752475308045898687660412  --  1e7
            , 0.9316390271097260080275166536120429704729018385275364343082838951  --  1e8
            , 0.5458434494486995642443872708975145289950229302628922076717364238  --  1e9
            , 0.5440211108893698134047476618513772816836430129162238915741840126  -- -1e1
            , 0.5063656411097587936565576104597854320650327212906573234433924735  -- -1e2
            ,-0.826879540532002560255887429109218141212724967847788320908123275   -- -1e3
            , 0.3056143888882521413609100352325069742318500438618062391101551456  -- -1e4
            ,-0.035748797972016509316470500695808829009045692578108896854616736   -- -1e5
            , 0.3499935021712929521176524867807714690614066053287162738570590546  -- -1e6
            ,-0.420547793190782491298506589740945595167175247530804589868766041   -- -1e7
            ,-0.931639027109726008027516653612042970472901838527536434308283895   -- -1e8
            ,-0.545843449448699564244387270897514528995022930262892207671736423   -- -1e9
            ]

largeCoss :: RealFloat a => [a]
largeCoss = [-0.839071529076452452258863947824064834519930165133168546835953731   --  1e1  
            , 0.8623188722876839341019385139508425355100840085355108292801621126  --  1e2
            , 0.5623790762907029910782492266053959687558118217381969177028251858  --  1e3
            ,-0.952155368259014851240386760663306001307070126044500996151572085   --  1e4
            ,-0.999360807438212451891135414144802203235386587459727476441041121   --  1e5
            , 0.9367521275331447869385325350749187757080978042123658797205783411  --  1e6
            ,-0.907270386181739561161712750921675682281603816635841849698126676   --  1e7
            ,-0.363385089355690553872375352547100110924259160874345033861427300   --  1e8
            , 0.8378871813639023343897756435515723560595769480881178785846012511  --  1e9
            ,-0.839071529076452452258863947824064834519930165133168546835953731   -- -1e1
            , 0.8623188722876839341019385139508425355100840085355108292801621126  -- -1e2
            , 0.5623790762907029910782492266053959687558118217381969177028251858  -- -1e3
            ,-0.952155368259014851240386760663306001307070126044500996151572085   -- -1e4
            ,-0.999360807438212451891135414144802203235386587459727476441041121   -- -1e5
            , 0.9367521275331447869385325350749187757080978042123658797205783411  -- -1e6
            ,-0.907270386181739561161712750921675682281603816635841849698126676   -- -1e7
            ,-0.363385089355690553872375352547100110924259160874345033861427300   -- -1e8
            , 0.8378871813639023343897756435515723560595769480881178785846012511  -- -1e9
            ]

-----------------------------------------------------------------------------
--COMMON STUFF

bigNums :: RealFloat a => [a]
bigNums = ns ++ map negate ns where
  ns = take 9 $ iterate (*10) 10 --at x=1e10, sin x/cos x = -0.5583496377943541; tan x = -0.5583496378112418, which fails our hasVal test.

smallNums :: (Enum a, RealFloat a) => [a]
smallNums = ns ++ map negate ns where
  ns = [1/16,1/8..10]

-----------------------------------------------------------------------------
-- TESTS FOR MONOTONICITY
-- Only useful where formulae cutover from one expression to another.

monotonTests :: (RealFloat a, Show a) => [Test a]
monotonTests = concat [[Test "asinh" (show xup  ) yup   (SI y0)
                       ,Test "asinh" (show xdown) ydown (SD y0)
                       ]
                      | (f, x0) <- monotonTestPoints
                      , let (y0, xup,   yup)   = yStep nextUp   f x0
                      , let (_ , xdown, ydown) = yStep nextDown f x0
                      ]
  where
    nextUp   = next 1
    nextDown = next (-1)
    next step x | isNaN x = x
                | otherwise = encodeFloat (m+step) e where (m,e) = decodeFloat x

monotonTestPoints :: (RealFloat a) => [(a -> a, a)]
monotonTestPoints = [(asinh,asinhCutover)]

--and here we need to know the inner workings of the function to know which points to test:
--(maybe this test should be pushed back into mingw-w64).
asinhCutover :: forall a. RealFloat a => a
asinhCutover = encodeFloat 1 (e `div` 2) where
  (_, e) = floatRange (undefined :: a)

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
--
-- I used to use these for validation, but they're quite slow.
-- I decided it's probably best to hard code some expected values (that I
-- got from WolframAlpha). I've kept these in case they are useful in future
-- along with some functions to check they're working properly.

_taySpecCheck :: Bool
_taySpecCheck = and
  [exp1   @Double == fromRational (expTay  1 )
  ,expN1  @Double == fromRational (expTay(-1))
  ,sin1   @Double == fromRational (sinTay  1 )
  ,cos1   @Double == fromRational (cosTay  1 )
  ,sinh1  @Double == fromRational (sinhTay 1 )
  ,cosh1  @Double == fromRational (coshTay 1 )
  ,asinh1 @Double == asinhNewt             1
  ]

_tayAlgCheck :: Bool
_tayAlgCheck = and $
    concat [[(fromRational @Double $ sinTay $ toRational x) `hasVal` A sinx
            ,(fromRational @Double $ cosTay $ toRational x) `hasVal` A cosx
            ]
           | (_, x, sinx, cosx) <- algVals @Double
           ]

_tayLargeCheck :: Bool
_tayLargeCheck = and $
     zipWith (\x y -> sinLarge x `hasVal` A y) (bigNums @Double) (largeSins @Double)
  ++ zipWith (\x y -> cosLarge x `hasVal` A y) (bigNums @Double) (largeCoss @Double)

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

fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact (n - 1)

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

--Newton method per https://en.wikipedia.org/wiki/Newton%27s_method

asinhNewt :: RealFloat a => a -> a
asinhNewt x = iterate (\z -> z - (sinh z - x)/cosh z) 1 !! 30


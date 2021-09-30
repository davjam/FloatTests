{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}

import HasVal
import Data.Complex

-- uncomment these for my fixes:
--import Prelude hiding (sqrt, log, tan, asin, acos, tanh, atan, asinh, acosh, atanh)
--import MyComplex 

------------------------------------
--Tests
------------------------------------

main :: IO ()
main = do
  putFails "Conjugate Double" (conjTests @Double)

conjTests :: forall a. (RealFloat a, Show a) => [Test (Complex a) (Complex (Expected a))]
conjTests = [Test name (show z) (f $ conjugate z) (A u :+ A v)
            | (name, f) <- allFns
            , x <- xs
            , y <- xs
            , let z = x :+ y
            , let (u:+v) = conjugate $ f z
            ]
  where xs = [-5, -4, -3, -2, -1, -0, 0, 1, 2, 3, 4, 5]

allFns :: RealFloat a => [(String, Complex a -> Complex a)]
allFns =
  [ ("sqrt"                   , sqrt                    )
  , ("exp"                    , exp                     )
  , ("log"                    , log                     )
  , ("sin"                    , sin                     )
  , ("asin"                   , asin                    )
  , ("cos"                    , cos                     )
  , ("acos"                   , acos                    )
  , ("tan"                    , tan                     )
  , ("atan"                   , atan                    )
  , ("sinh"                   , sinh                    )
  , ("asinh"                  , asinh                   )
  , ("cosh"                   , cosh                    )
  , ("acosh"                  , acosh                   )
  , ("tanh"                   , tanh                    )
  , ("atanh"                  , atanh                   )
  ]

{- TO ADD:
sqrt((-0):+(-0)), etc.
complex functions with zero imag match real functions.
issue 8532: ensure acosh((-1):+0) = 0:+pi.
log (0 :+ (-0)) == (-Infinity) :+ (-0.0).
mkPolar is inverse of polar, for all +/- 0, +/- pi combinations.
-}

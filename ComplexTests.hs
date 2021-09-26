{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE TypeApplications #-}


import           Data.Complex
import qualified MyComplex as C


------------------------------------
--Tests
------------------------------------

failsFor :: (IEEE a, RealFloat a) => (Complex a -> Complex a) -> (Complex a -> Complex a) -> [Complex a]
failsFor f g = filter (\x -> not $ f x ~= g x) [x:+y | x <- xs, y <- xs]
  where xs = [-5, -4, -3, -2, -1, -0, 0, 1, 2, 3, 4, 5]

main :: IO ()
main  =  mapM_ putFailsFor (allPlots @Double)
  where
    putFailsFor (name, _) | name `elem` knownFails = return ()
    putFailsFor (name, f) = do
      case failsFor (conjugate . f) (f . conjugate) of
        [] -> return ();
        xs -> putStrLn $ name ++ " conj " ++ show xs
    knownFails = [] --"sqrt", "log", "asin", "acos", "atan", "tanh", "asinh", "acosh", "atanh"]

class IEEE a where
  infix 4 ~=
  (~=) :: a -> a -> Bool

instance IEEE Double where
  x ~= y | isInfinite x        =  x == y
         | isNaN x && isNaN y  =  True
         | isNeg x /= isNeg y            =  False
         | otherwise           =  abs (x - y) < 0.00001
    where
      isNeg r = isNegativeZero r || r < 0
{-
See comment on _asin conjugate
instance IEEE Double where
  x ~= y | isNaN x && isNaN y            =  True
         | isNeg x /= isNeg y            =  False
         | isInfinite x == isInfinite y  =  True
         | otherwise                     =  abs (x - y) < 0.00001
-}

instance IEEE a => IEEE (Complex a) where
  x :+ y ~= x' :+ y' = x ~= x' && y ~= y'

allPlots :: RealFloat a => [(String, Complex a -> Complex a)]
allPlots =
  [ ("sqrt"                   , sqrt                    )
  , ("C.sqrt"                 , C.sqrt                  )
  , ("exp"                    , exp                     )
  , ("log"                    , log                     )
  , ("C.log"                  , C.log                   )
  , ("sin"                    , sin                     )
  , ("asin"                   , asin                    )
  , ("C.asin"                 , C.asin                  )
  , ("cos"                    , cos                     )
  , ("acos"                   , acos                    )
  , ("C.acos"                 , C.acos                  )
  , ("tan"                    , tan                     )
  , ("atan"                   , atan                    )
  , ("C.atan"                 , C.atan                  )
  , ("sinh"                   , sinh                    )
  , ("asinh"                  , asinh                   )
  , ("C.asinh"                , C.asinh                 )
  , ("cosh"                   , cosh                    )
  , ("acosh"                  , acosh                   )
  , ("C.acosh"                , C.acosh                 )
  , ("tanh"                   , tanh                    )
  , ("C.tanh"                 , C.tanh                  )
  , ("atanh"                  , atanh                   )
  , ("C.atanh"                , C.atanh                 )
  ]

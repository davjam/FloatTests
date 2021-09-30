{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module HasVal (Expected(..), HasVal(..), Test(..), putFails)
where

import Data.Foldable
import Data.Complex

data Expected a = E a    --exactly
                | A a    --approximately (but to many sfs)
                | R      --any real (not Inf, not NaN)
                | SI a   --small increment above
                | SD a   --small decrement below
  deriving Show

class HasVal a b where
  hasVal :: a -> b -> Bool

instance HasVal Double (Expected Double) where
  x `hasVal` y = hasFltVal 36 x y
 
instance HasVal Float (Expected Float) where
  x `hasVal` y = hasFltVal 16 x y

--This is a weird use of Complex, but allows e.g.  E 4 :+ A 3
instance HasVal a (Expected a) => HasVal (Complex a) (Complex (Expected a)) where
  (x:+y) `hasVal` (v:+w) = x `hasVal` v && y `hasVal` w


hasFltVal :: RealFloat a => Int -> a -> Expected a -> Bool
hasFltVal _   x R     | isNaN x          = False
                      | isInfinite x     = False
                      | otherwise        = True
hasFltVal _   x (E y) | isNaN y          = isNaN x
                      | isNaN x          = False
                      | isNegativeZero y = isNegativeZero x
                      | isNegativeZero x = False
                      | otherwise        = x == y
hasFltVal bps x (A y) | isNaN y          = isNaN x
                      | isNaN x          = False
                      | isInfinite y     = abs x > 2^(2*bps)
                      | abs y < err      = abs x < err
                      | otherwise        = fromIntegral (round $ x/y * mul :: Integer) / mul == 1
  where mul = 2^bps
        err = 1/mul
hasFltVal bps x (SI s) = s < x && x - s < 1/2^bps
hasFltVal bps x (SD s) = s > x && s - x < 1/2^bps

data Test a b = Test Cat Name Val a b
  deriving Show

type Cat  = String
type Name = String
type Val  = String

putFails :: (Show a, Show b, HasVal a b) => String -> [Test a b] -> IO ()
putFails label tests | null fails   = putStrLn $ label ++ " passed."
                     | otherwise = do
  putStrLn $ label ++ " FAILURES:"
  traverse_ putFail fails
  where
    fails = filter (\(Test _ _ _ value expected) -> not (value `hasVal` expected)) tests
    putFail (Test c n v value expected) = putStrLn $ c++" "++n++" "++v++" sb "++show expected++" is "++show value


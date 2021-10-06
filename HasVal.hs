{-# OPTIONS -Wall -Wpartial-fields #-}

module HasVal (Expected(..), HasVal(..), Test(..), putFails)
where

import Double0

import Data.Foldable

data Expected a = E a    --exactly
                | A a    --approximately (but to many sfs)
                | R      --any real (not Inf, not NaN)
                | SI a   --small increment above
                | SD a   --small decrement below
  deriving Show

class HasVal a where
  hasVal :: a -> Expected a -> Bool

instance HasVal Double where
  x `hasVal` y = hasFltVal 36 x y
 
instance HasVal D0 where
  x `hasVal` y = hasFltVal 36 x y
 
instance HasVal Float where
  x `hasVal` y = hasFltVal 16 x y
  
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

data Test a = Test Name Val a (Expected a)
  deriving Show

type Name = String
type Val  = String

putFails :: (Show a, HasVal a) => String -> [Test a] -> IO ()
putFails label tests | null fails   = putStrLn $ label ++ " passed."
                     | otherwise = do
  putStrLn $ label ++ " FAILURES:"
  traverse_ putFail fails
  putStrLn ""
  where
    fails = filter (\(Test _ _ value expected) -> not (value `hasVal` expected)) tests
    putFail (Test n v value expected) = putStrLn $ n++" "++v++" sb "++show expected++" is "++show value


{-# OPTIONS -Wall -Wpartial-fields #-}

module HasVal (Expected(..), HasVal(..), Test(..), putFails)
where

import Double0

import Data.Foldable
import Control.Monad

data Expected a = E a    --exactly
                | A a    --approximately (but to many sfs)
                | B a    --like A, but if 0, -0, Inf, -Inf, NaN checked exactly (including sign)
                | X Int a--like A, but even more approXimate.
                | R      --any real (not Inf, not NaN)
                | P      --real and positive (not zero)
                | N      --real and negative (not zero)
                | V      --A valid result (not NaN)
                | Z      --exactly zero, but either + or -.
                | I a    --small increment above
                | D a    --small decrement below
  deriving Show

class HasVal a where
  hasVal :: a -> Expected a -> Bool
  infix 4 `hasVal`

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
hasFltVal bps x P     = hasFltVal bps x R && x > 0
hasFltVal bps x N     = hasFltVal bps x R && x < 0
hasFltVal _   0 Z     = True
hasFltVal _   _ Z     = False
hasFltVal _   x V     | isNaN x          = False
                      | otherwise        = True
hasFltVal _   x (E y) | isNaN y          = isNaN x
                      | isNaN x          = False
                      | isNegativeZero y = isNegativeZero x
                      | isNegativeZero x = False
                      | otherwise        = x == y
hasFltVal bps x (B y) | isNaN y          = isNaN x
                      | isNaN x          = False
                      | y == 0           = x == y && isNegativeZero x == isNegativeZero y
                      | isInfinite y     = x == y
                      | otherwise        = approx bps x y
hasFltVal bps x (A y) = hasFltVal bps x (X 1 y)
hasFltVal bps x (X n y)
                      | isNaN y          = isNaN x
                      | isNaN x          = False
                      | isInfinite y     = x == y
                      | otherwise        = approx (bps `div` n) x y
hasFltVal bps x (I s) = s < x && x - s < 1/2^bps
hasFltVal bps x (D s) = s > x && s - x < 1/2^bps

approx :: RealFloat a => Int -> a -> a -> Bool
approx bps x y
  | abs y < err = abs x < err
  | otherwise   = fromIntegral (round $ x/y * mul :: Integer) / mul == 1
  where mul = 2^bps
        err = 1/mul

data Test a = Test Name Val a (Expected a)
  deriving Show

type Name = String
type Val  = String

putFails :: (Show a, HasVal a) => String -> [Test a] -> IO ()
putFails label tests | null fails   = putStrLn $ label ++ " passed."
                     | otherwise = do
  putStrLn $ label ++ " " ++ (show $ length fails) ++ " FAILURES:"
  traverse_ putFail $ take 30 fails
  when (length fails > 30) $ putStrLn "..."
  putStrLn ""
  where
    fails = filter (\(Test _ _ value expected) -> not (value `hasVal` expected)) tests
    putFail (Test n v value expected) = putStrLn $ n++" $ "++v++" sb "++show expected++" is "++show value


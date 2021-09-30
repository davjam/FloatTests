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
  x `hasVal` R     = fltR x
  x `hasVal` (E e) = fltE x e
  x `hasVal` (A a) = fltA 0.000001 x a
  x `hasVal` (SI s) = s < x && x - s < 0.000001
  x `hasVal` (SD s) = s > x && s - x < 0.000001
  
 
instance HasVal Float (Expected Float) where
  x `hasVal` R     = fltR x
  x `hasVal` (E e) = fltE x e
  x `hasVal` (A a) = fltA 0.0001 x a
  x `hasVal` (SI s) = s < x && x - s < 0.0001
  x `hasVal` (SD s) = s > x && s - x < 0.0001

--This is a weird use of Complex, but allows e.g.  E 4 :+ A 3
instance HasVal a (Expected a) => HasVal (Complex a) (Complex (Expected a)) where
  (x:+y) `hasVal` (v:+w) = x `hasVal` v && y `hasVal` w

fltE :: RealFloat a => a -> a -> Bool
fltE x y | isNaN x          = isNaN y
         | isNaN y          = False
         | isNegativeZero x = isNegativeZero y
         | isNegativeZero y = False
         | otherwise        = x == y

fltA :: RealFloat a => a -> a -> a -> Bool
fltA err x y | isNaN y              = isNaN x
             | isNaN x              = False
             | abs y < err          = abs x < err
             | signum x /= signum y = False
             | isInfinite y         = abs x > 1/err^(2::Int)
             | otherwise            = abs (x/y - 1) < err

fltR :: RealFloat a => a -> Bool
fltR x | isNaN x          = False
       | isInfinite x     = False
       | otherwise        = True

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


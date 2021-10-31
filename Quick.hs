{-# LANGUAGE TemplateHaskell #-} 

import Test.QuickCheck
--import Data.Complex
import MyComplex
import TestValue
import Control.Monad
import Debug.Trace

instance (RealFloat a, Arbitrary a) => Arbitrary (Complex a) where
  arbitrary = liftM2 (:+) arbitrary arbitrary
  shrink (x :+ y) = [ x' :+ y | x' <- shrink x ] ++
                    [ x :+ y' | y' <- shrink y ]

--prop1 :: RealFloat a => a -> a -> Bool
prop1 x = sin x^2 + cos x^2 ~= 1

infix 4 ~=
--(~=) :: (Num a, Ord a) => Bool
x ~= y = abs (x - y) < 1e-10

prop_signum_conj, prop_signum_neg, prop_signum_flip :: Complex Double -> Bool
prop_signum_conj z = signum(conjugate z) `is` (E (conjugate (signum z)))
prop_signum_neg  z = signum(- z) `is` (E (- (signum z)))
prop_signum_flip z@(x:+y) = realPart(signum z) `hasVal` E (imagPart (signum (y:+x)))

prop_signum_real, prop_signum_diag :: Double -> Property
prop_signum_real x = x>0 ==> signum (x:+0) `is` E (1:+0)
prop_signum_diag x = x > 0 ==> (signum(x:+x) `is` A (1/sqrt 2 :+ 1/sqrt 2))


prop_mult_conj, prop_mult_neg, prop_mult_comm :: Complex Double -> Complex Double -> Bool
prop_mult_conj z z' = conjugate z * conjugate z' `is` B (conjugate (z * z'))
prop_mult_neg z z' = (-z) * z' `is` B (- (z * z'))
prop_mult_comm z z' = z * z' `is` B (z' * z)

prop_abs_signum :: Complex Double -> Bool
prop_abs_signum z = abs z * signum z `is` (B z)


infix 4 `is`
x :+ y `is` (E (u :+ v)) = x `hasVal` (E u) && y `hasVal` (E v)
x :+ y `is` (A (u :+ v)) = x `hasVal` (A u) && y `hasVal` (A v)
x :+ y `is` (B (u :+ v)) = x `hasVal` (B u) && y `hasVal` (B v)

return []
runTests = $quickCheckAll


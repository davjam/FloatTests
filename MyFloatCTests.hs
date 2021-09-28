{-# OPTIONS -Wall -Wpartial-fields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

--ghc MyFloatCTests MyFloatC.c
import Prelude hiding (asinh, atanh)

foreign import ccall unsafe "MyFloatC.c asinh"  asinh :: Double -> Double
foreign import ccall unsafe "MyFloatC.c atanh"  atanh :: Double -> Double

main :: IO ()
main = do
  print $ asinh $ -0
  print $ atanh    0
  print $ asinh    1e300



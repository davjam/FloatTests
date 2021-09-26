{-# OPTIONS -Wall -Wpartial-fields #-}

module MyFloat (asinh)
where

import           Prelude       hiding (asinh)
import qualified Prelude       as P   (asinh)

{-
corrects neg zero, but gives NaN for large values
-}

asinh :: RealFloat a => a -> a
asinh x | isNegativeZero x = -0
        | otherwise        = P.asinh x

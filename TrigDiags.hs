{-# OPTIONS -Wall -Wpartial-fields #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Prelude hiding (sqrt, exp, log, sin, asin, cos, acos, tan, atan, sinh, asinh, cosh, acosh, tanh, atanh)
import qualified Prelude as P (sqrt, exp, log, sin, asin, cos, acos, tan, atan, sinh, asinh, cosh, acosh, tanh, atanh)

import           Data.Foldable
import           Numeric (showFFloat)
import           Control.Monad
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Text.Blaze.Svg11 (Svg, AttributeValue, toValue, (!), (!?))
import qualified Text.Blaze.Svg11                as S
import qualified Text.Blaze.Svg11.Attributes     as SA
--import           Text.Blaze.Html.Renderer.Pretty  --looks better in editor
import           Text.Blaze.Html.Renderer.String    --looks better in browser inspection pane
import           Text.Printf
import           System.Directory
import           Debug.Trace

import qualified OldComplex   as O  --Old implementation
import qualified MyComplex    as N  --New implementation
import Double0


------------------------------------
-- Overall document
------------------------------------

--I put the html output on github following https://stackoverflow.com/questions/8446218/how-to-see-an-html-page-on-github-as-a-normal-rendered-html-page-to-see-preview
--see: https://davjam.github.io/FloatTests/TrigDiags/Curr.html and https://davjam.github.io/FloatTests/TrigDiags/Fixed.html

main :: IO ()
main = do
  createDirectoryIfMissing False "TrigDiags"
  writeGraphsDoc @(O.Complex Double) True FullGraph currPlots "TrigDiags\\Curr.html"
  writeGraphsDoc @(N.Complex Double) True FullGraph currPlots "TrigDiags\\Fixed.html"
  writeGraphsDoc @(N.Complex Float ) True FullGraph currPlots "TrigDiags\\FloatFixed.html"
  writeGraphsDoc @(N.Complex D0    ) True FullGraph currPlots "TrigDiags\\D0Fixed.html"

_t1 :: IO ()  --used for testing little bits
_t1 = writeGraphsDoc @(N.Complex Double) True [AnnulusSectorSeg A3 7 CO] [("id", id), ("acosh", acosh)] "TrigDiags\\Test.html"

writeGraphsDoc :: (IsComplex c a, Graphable b c a) => Bool -> b -> PlotList c a -> FilePath -> IO ()
writeGraphsDoc includeAxes graphable plotList filePath = trace filePath $
  writeFile filePath $ renderHtml $ graphsDoc includeAxes graphable plotList

graphsDoc :: (IsComplex c a, Graphable b c a) => Bool -> b -> PlotList c a -> H.Html
graphsDoc includeAxes graphable plotList = H.docTypeHtml ! SA.lang "en" $ do
  H.head $ do
    H.meta ! HA.charset "utf-8"
    H.meta ! HA.name "viewport"
           ! HA.content "width=device-width, initial-scale=1.0, viewport-fit=cover"
    H.title "Complex Function Graphs"

    --y coords in svg increase from top to bottom, but we want our graphs to work the other way.
    H.style "\n\
            \body           {font-family:sans-serif}\n\
            \.coords        {display:block;transform:scaleY(-1)}\n\
            \.axes          {stroke:black;stroke-width:0.01;shape-rendering:crispedges}\n\
            \"

    --Enable MathJax content. https://www.mathjax.org/#gettingstarted
    H.script ! HA.src "https://polyfill.io/v3/polyfill.min.js?features=es6" $ ""
    H.script ! HA.id "MathJax-script" ! HA.async "" ! HA.src "https://cdn.jsdelivr.net/npm/mathjax@3.0.1/es5/tex-mml-chtml.js" $ ""

  H.body $
    traverse_ (uncurry $ graph includeAxes graphable) plotList

type PlotList c a = [(Title, PlotFn c a)]
type Title        = String
type PlotFn c a   = Pt c -> Pt c
type Pt c         = c
type R a          = a

{-
Keep R as Double
due to weidness with Float where:
pathPtsX (flipHoriz . (:+ 0)) asin 2 big
has some kind of "step" change L
1531.9375
> asin (-1229 :: Complex Float)
(-1.5707964) :+ 7.912301
> asin (-1531.9375 :: Complex Float)
(-1.5707964) :+ 7.912301
> asin (-1531.9376 :: Complex Float)
(-1.5707964) :+ 8.317766
> asin (-1649 :: Complex Float)
(-1.5707964) :+ 8.317766
-}
currPlots :: (IsComplex c a) => PlotList c a
currPlots =
  [ ("id"                     , id                      )
  , ("sqrt"                   , sqrt                    )
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
  , ("acosh"                  , acosh                   ) -- nb the lisp book has an extra line (0+0i to 0-1i), that I think should not be there(*).
  , ("tanh"                   , tanh                    )
  , ("atanh"                  , atanh                   )
  ]

graph :: forall a b c. (IsComplex c a, Graphable b c a) => Bool -> b -> Title -> PlotFn c a -> H.Html
graph includeAxes graphable title plotFn = trace title $ do
  H.h1 $ H.toHtml title
  S.svg ! SA.width "800" ! SA.height "800"
        ! SA.viewbox "-4.2 -4.2 8.4 8.4"
        ! SA.class_ "coords"
        $ do
    toSvg fullGraph
    when includeAxes $ S.g ! SA.class_ "axes" $ traverse_ toSvg  axes --axes last, so they appear on top.
  H.p $ do "("; H.toHtml $ pointCount fullGraph; " pts)"
  H.hr
  where
    fullGraph = graphSvg plotFn graphable :: SVG c a

    axes :: [SVG c a]
    axes = xAxis ++ yAxis
      where
        xAxis = SVG (Line CentreZeros (negate 4.2 +:+ 0) (4.2 +:+ 0))
              :  map (tick True)  [-4,-3,-2,-1,1,2,3,4]
              ++ map (tick False) [-pi,-pi/2,pi/2,pi]
        yAxis = map (trans rot90) xAxis
        tick major x = SVG $ Line CentreZeros (x + offs) (x - offs)
          where
            offs | major     = 0 +:+ 0.08
                 | otherwise = 0 +:+ 0.06

------------------------------------
-- Graphable things
------------------------------------

data FullGraph = FullGraph
  deriving Show


data Parallel = Parallel Orient ParallelId
  deriving Show

data Orient = Vert | Horiz
  deriving (Show, Enum, Bounded)

data ParallelId = PN3 | PN2 | PN1 | PN0 | PP0 | PP1 | PP2 | PP3
  deriving (Show, Enum, Bounded)

data ParallelSeg = ParallelSeg Orient ParallelTB ParallelLR ParallelSegId
  deriving Show

data ParallelTB = T | B
  deriving (Show, Enum, Bounded)

data ParallelLR = R | L
  deriving (Show, Enum, Bounded)

data ParallelSegId = P0S P0SId | P1S P1SId | P2S | P3S
  deriving (Show)

data P0SId = P0Sa | P0Sb | P0Sc | P0Sd
  deriving (Show, Enum, Bounded)

data P1SId = P1Sa | P1Sb
  deriving (Show, Enum, Bounded)


data Annulus = A1 | A2 | A3 | A4
  deriving (Show, Enum, Bounded)

data AnnulusSector = AnnulusSector Annulus AnnulusSectorId
  deriving Show
  
type AnnulusSectorId = Int

data AnnulusSectorSeg = AnnulusSectorSeg Annulus AnnulusSectorId AnnulusSectorSegId
  deriving Show

data AnnulusSectorSegId = R0 | CO | R1 | CI  --in order of polygon traversal: radial (most clockwise), circumference outer, radial (most anticlockwise), circ inner.
  deriving (Show, Enum, Bounded)


allParallels :: forall a. RealFloat a => [Parallel]
allParallels = [Parallel o pId | o <- enumerate, pId <- enumerate, parallelReqd pId]
  where
    parallelReqd _   | isIEEE (undefined :: a) = True
    parallelReqd PN0                           = False
    parallelReqd _                             = True

parallelSegs :: Parallel -> [ParallelSeg]
parallelSegs (Parallel o PN3) = [ParallelSeg o B s  P3S    | s <- enumerate]
parallelSegs (Parallel o PN2) = [ParallelSeg o B s  P2S    | s <- enumerate]
parallelSegs (Parallel o PN1) = [ParallelSeg o B s (P1S p) | s <- enumerate, p <- enumerate]
parallelSegs (Parallel o PN0) = [ParallelSeg o B s (P0S p) | s <- enumerate, p <- enumerate]
parallelSegs (Parallel o PP0) = [ParallelSeg o T s (P0S p) | s <- enumerate, p <- enumerate]
parallelSegs (Parallel o PP1) = [ParallelSeg o T s (P1S p) | s <- enumerate, p <- enumerate]
parallelSegs (Parallel o PP3) = [ParallelSeg o T s  P3S    | s <- enumerate]
parallelSegs (Parallel o PP2) = [ParallelSeg o T s  P2S    | s <- enumerate]

parallelSegPath :: forall c a. (IsComplex c a) => ParallelSeg -> ParamPath c a
parallelSegPath (ParallelSeg o tb lr p) = ParamPath (toValue lr <> toValue p) w co d pathFn start end
  where
    pathFn = rot . flipV . flipH . part
    rot = case o of
      Horiz -> id
      Vert  -> rot90
    flipV = case tb of
      T -> id
      B -> flipVert
    flipH = case lr of
      L -> flipHoriz
      R -> id
    part = case p of
      (P0S _) -> (+:+ 0)
      (P1S _) -> (+:+ 1)
      P2S     -> (+:+ 2)
      P3S     -> (+:+ 3)
    (start, end) = case p of
      (P0S P0Sa) -> (tiny, 0.5)
      (P0S P0Sb) -> (0.5, 1)
      (P0S P0Sc) -> (2, 1)
      (P0S P0Sd) -> (2, big)
      (P1S P1Sa) -> (2, tiny)
      (P1S P1Sb) -> (2, big)
      _        -> (tiny, big)
    w = case (isIEEE a, tb, lr, p) of
      (False, T, _, P0S _) -> 0.04
      (_    , T, _, P0S _) -> 0.03
      (_    , B, _, P0S _) -> 0.06
      _                    -> 0.015
    a = undefined :: a
    co = case (o, tb, lr, p) of
      --colours from https://www.deviantart.com/otipeps/art/The-Color-Wheel-16-colors-695466699
      (Horiz, _ , R, P0S _) -> "blue"
      (Horiz, _ , L, P0S _) -> "orange"
      (Horiz, T , R, _    ) -> "violet"
      (Horiz, T , L, _    ) -> "red"
      (Horiz, B , L, _    ) -> "yellow"
      (Horiz, B , R, _    ) -> "cyan"

      (Vert , _ , R, P0S _) -> "green"
      (Vert , _ , L, P0S _) -> "magenta"
      (Vert , T , R, _    ) -> "cyan"
      (Vert , T , L, _    ) -> "violet"
      (Vert , B , L, _    ) -> "red"
      (Vert , B , R, _    ) -> "yellow"
    d = case (o, tb, p) of
      (_,     B, P0S _) -> "0.04,0.04"
      (Horiz, _, P0S _) -> ""
      (Horiz, _, _    ) -> "0.08,0.02"
      _                 -> ""

allAnnuli :: [Annulus]
allAnnuli = enumerate

annulusSectorCount :: Annulus -> Int  --too cumbersome to represent as a new type.
annulusSectorCount a = 2^(j+1) where j = fromEnum a + 1

annulusSectors :: Annulus -> [AnnulusSector]
annulusSectors a = map (AnnulusSector a) $ reverse [0 .. annulusSectorCount a - 1]  --reverse so lighter ones on top (same as book).

annulusSectorSegs :: AnnulusSector -> [AnnulusSectorSeg]
annulusSectorSegs (AnnulusSector a sId) = map (AnnulusSectorSeg a sId) enumerate

annulusSectorSegPath :: (IsComplex c a) => AnnulusSectorSeg -> ParamPath c a
annulusSectorSegPath (AnnulusSectorSeg a s p) = ParamPath (toValue p) 0.01 "black" "" pathFn start end
  where
    (pathFn, start, end) = case p of
      R0 -> ((`mkPolar` ang0 ), inner, outer)
      CO -> (  mkPolar  outer , ang0 , ang1 )
      R1 -> ((`mkPolar` ang1 ), outer, inner)
      CI -> (  mkPolar  inner , ang1 , ang0 )
    (ang0, ang1) = inset ( segAng *  fromIntegral s
                         , segAng * (fromIntegral s + 1)
                         )
    (inner, outer) = inset $ case a of
      A1 -> (0.25, 0.5)
      A2 -> (0.75, 1  )
      A3 -> (pi/2, 2  )
      A4 -> (3   , pi )
    segAng = 2 * pi / fromIntegral numSegs
    numSegs = annulusSectorCount a
    --don't go right into corners, since they sometimes fail.
    --(and sometimes with horrid discontinuities, e.g. atanh on A2 7 R2).
    --we can't add tiny (it gets lost due to lack of precision), so add this instead.
    --Also mkPolar 2 pi have -ve imag, so we need to step back to ensure we don't cross quadrants.
    --0.000001 works for Float, but 0.0000001 doesn't.
    inset (x,y) = (x + small, y - small) where small = 0.000001
    --Maybe we should try to convert 0.0 to -0.0 depending on quadrant instead?
    --atanh (1 :+ 0.000000000000000000000001) = 27.97759470620852 :+ 0.7853981633974483
    --atanh (1 :+ 0) = NaN :+ NaN

------------------------------------
-- Graphable class and instances
------------------------------------

class Graphable b c a where
  graphSvg :: PlotFn c a -> b -> SVG c a

instance (RealFloat a, Graphable b c a) => Graphable [b] c a where
  graphSvg plotFn xs = SVG $ map (graphSvg plotFn) xs

instance (IsComplex c a) => Graphable FullGraph c a where
  graphSvg plotFn FullGraph = SVG
    [ SVG $ Group "annuli"    $ map (graphSvg plotFn) allAnnuli
    , SVG $ Group "parallels" $ map (graphSvg plotFn) $ allParallels @a
    ]

instance (IsComplex c a) => Graphable Parallel c a where
  graphSvg plotFn p = SVG $ Group (toValue p) $ map (graphSvg plotFn) $ parallelSegs p

instance (IsComplex c a) => Graphable ParallelSeg c a where
  graphSvg = paramPathSvg @a

instance (IsComplex c a) => Graphable Annulus c a where
  graphSvg plotFn a = SVG $ Group (toValue a) $ map (graphSvg plotFn) $ annulusSectors a

instance (IsComplex c a) => Graphable AnnulusSector c a where
  graphSvg plotFn sec@(AnnulusSector a s) = SVG $ Closed ("s" <> toValue s) sectorCol CentreZeros
                                                $ map snd $ concatMap (paramPathPts plotFn) $ annulusSectorSegs sec
    where
    sectorCol = 80 - (80 * s) `div` annulusSectorCount a

--AnnulusSectorSeg not graphed individually in FullGraph (instead, the points from each seg are concat'd and SVGd as a Closed polygon).
--But we want to be able to do this, e.g. for debugging a single segment.
instance (IsComplex c a) => Graphable AnnulusSectorSeg c a where
  graphSvg = paramPathSvg @a

instance (IsComplex c a) => Graphable (ParamPath c a) c a where
  graphSvg plotFn pp@(ParamPath cl w co d _ _ _) = SVG $ Path cl w co d SplitZeros $ map snd $ paramPathPts plotFn pp

------------------------------------
-- Graphable item labels (used in class names of groups, polylines and polygones).
-- Designed to follow the grouping of the standard graph layout, and avoid repetition.
------------------------------------

instance H.ToValue Parallel where
  toValue (Parallel o p) = H.stringValue $ show o ++ show p

instance H.ToValue ParallelLR where
  toValue = H.stringValue . show

instance H.ToValue ParallelSegId where
  toValue (P0S p0p) = toValue p0p
  toValue (P1S p1p) = toValue p1p
  toValue _         = mempty

instance H.ToValue P0SId where
  toValue = H.stringValue . drop 3 . show

instance H.ToValue P1SId where
  toValue = H.stringValue . drop 3 . show

instance H.ToValue Annulus where
  toValue = H.stringValue . show

instance H.ToValue AnnulusSectorSegId where
  toValue = H.stringValue . show

------------------------------------
-- Paths
------------------------------------

class HasParamPath b c a where
  paramPath :: b -> ParamPath c a

instance (IsComplex c a) => HasParamPath ParallelSeg c a where
  paramPath = parallelSegPath

instance (IsComplex c a) => HasParamPath AnnulusSectorSeg c a where
  paramPath = annulusSectorSegPath

instance (IsComplex c a) => HasParamPath (ParamPath c a) c a where
  paramPath = id

paramPathSvg :: forall a b c. (IsComplex c a, HasParamPath b c a) => PlotFn c a -> b -> SVG c a
paramPathSvg plotFn = graphSvg plotFn . (paramPath @b @c @a)

data ParamPath c a = ParamPath Class Width Colour Dasharray (PathFn c a) a a

_paramPathPt :: (IsComplex c a, HasParamPath b c a) => PlotFn c a -> b -> R a -> Pt c
_paramPathPt plotFn p = plotFn . pathFn
  where (ParamPath _ _ _ _ pathFn _ _) = paramPath p

type Class = AttributeValue       --used for labelling, not for applying style
type Width = Float
type Colour = S.AttributeValue
type Dasharray = S.AttributeValue

type PathFn c a = a -> Pt c --maybe the original line, or maybe transformed by a PlotFn

paramPathPts :: (IsComplex c a, HasParamPath b c a) => PlotFn c a -> b -> [(R a, Pt c)]
paramPathPts plotFn x = removeRedundantPoints $ pathFnPts (plotFn . pathFn) s e
  where (ParamPath _ _ _ _ pathFn s e) = paramPath x

pathFnPts :: (IsComplex c a) => PathFn c a -> R a -> R a -> [(R a, Pt c)]
pathFnPts f s0 e0 | e0 < 1000  = pathFnPts' s0 e0
                  | isPeriodic = pathFnPts' s0 pi ++ pathFnPts' pi (1.5*pi) ++ pathFnPts' (1.5*pi) (2*pi)
                  | otherwise  = pathFnPts' s0 e0
  where
    pathFnPts' s e | isWayOut fs && isWayOut fe = []
                   | isWayOut fs = pathFnPts' (backoff s e) e
                   | isWayOut fe = pathFnPts' s (backoff e s)
                   | otherwise = refinePts f s e
      where
        fs = f s
        fe = f e
        backoff p other | p > 10 || p < 0.1 = if strictMonoton p sp other
                                                then sp
                                                else sp * P.sqrt other
                        | otherwise = p + signum (other - p) * 0.0005
          where
            sp = P.sqrt p
            strictMonoton x y z = x < y && y < z || x > y && y > z
    isPeriodic = f0 `isCloseTo` f2
              && f2 `isCloseTo` f4
              && f1 `isCloseTo` f3
              && (      f0 `isCloseTo` f1  &&      f2 `isCloseTo` f3
                || not (f0 `isCloseTo` f1) && not (f2 `isCloseTo` f3)
                 )
      where
        f0 = f   s0
        f1 = f $ s0 + 1
        f2 = f $ s0     + 2*pi
        f3 = f $ s0 + 1 + 2*pi
        f4 = f $ s0     + 4*pi

refinePts :: (IsComplex c a) => PathFn c a -> R a -> R a -> [(R a, Pt c)]
refinePts f s0 e0 = removeRedundantPoints $ (s0, fs0) : pathPts' s0 fs0 e0 (f e0)
  where
    fs0 = f s0
    pathPts' s fs e fe | {-trace (show s ++ "," ++ show e) $-} isOutside fs && isOutside fe = []
                       | fs `isCloseTo` fe = [(e, fe)]     --don't include the start, so we don't double-up on the mid points.
                       | m == s || m == e = [(e, fe)]      --sometimes these get too close, even though their projection isn't.
                       | otherwise = pathPts' s fs m fm ++ pathPts' m fm e fe
      where
        fm = f m
        m  = midR s e

midR :: RealFloat a => R a -> R a -> R a
midR 0 e = e / 2
midR s 0 = s / 2
midR s e = P.sqrt s * P.sqrt e

isCloseTo :: (IsComplex c a) => Pt c -> Pt c -> Bool
x `isCloseTo` y = isSmall (realPart z) && isSmall (imagPart z)
  where
    z = x - y
    isSmall d = abs d < 0.05

isOutside, isWayOut :: (IsComplex c a) => Pt c -> Bool
isOutside = isOut 10
isWayOut  = isOut 1000

isOut :: (IsComplex c a) => R a -> Pt c -> Bool
isOut lim p = isOff (realPart p) || isOff (imagPart p)
  where isOff r | isNaN r      = True
                | isInfinite r = True
                | abs r > lim  = True
                | otherwise    = False

maxNonInfiniteFloat :: forall a. RealFloat a => R a
maxNonInfiniteFloat = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e
    a = undefined :: a

minPositiveFloat :: forall a. RealFloat a => R a
minPositiveFloat = encodeFloat 1 $ fst (floatRange a) - floatDigits a
  where a = undefined :: a

big, tiny :: RealFloat a => R a
big  = P.sqrt $ P.sqrt maxNonInfiniteFloat
tiny = P.sqrt $ P.sqrt minPositiveFloat

rot90, flipHoriz, flipVert :: (IsComplex c a) => Pt c -> Pt c
rot90 = negate . iTimes --(*) (0 :+ (-1))
flipHoriz z = negate (realPart z) +:+ imagPart z
flipVert  z = realPart z +:+ negate (imagPart z)

iTimes :: (IsComplex c a) => c -> c
iTimes z = (-imagPart z) +:+ realPart z

removeRedundantPoints :: (IsComplex c a) => [(R a, Pt c)] -> [(R a, Pt c)]
removeRedundantPoints ((r0,p0):(r1,p1):(r2,p2):px)
  | p0 == p1         = removeRedundantPoints ((r0,p0):(r2,p2):px)
  | p1 == p2         = removeRedundantPoints ((r0,p0):(r1,p1):px)
  | z        < 0.001
 || z > 2*pi - 0.001 = removeRedundantPoints ((r0,p0):(r2,p2):px) --we can have one at +3.14 and one at -3.14, and they should be considered aligned.
  | otherwise        = (r0,p0):removeRedundantPoints((r1,p1):(r2,p2):px)
  where
    z = abs(phase (p1 - p0) - phase (p2 - p1))
removeRedundantPoints ps = ps

------------------------------------
-- Debugging helpers
------------------------------------

{- example usage
> x = ParallelSeg Vert T L P2S
> pf = sqrt :: PlotFn (N.Complex Double) Double
> _putParamPath pf x
-}

_putParamPath :: forall a b c. (PrintfArg a, IsComplex c a, HasParamPath b c a) => PlotFn c a -> b -> IO ()
_putParamPath plotFn x = do
  putStrLn "       R,    Path,        ,    Plot,        "
  mapM_ putPts $ paramPathPts plotFn p
  where
  p@(ParamPath _ _ _ _ pathFn _ _) = paramPath x :: ParamPath c a
  putPts (r,plotPt) = printf "%8.4g,%8.4f,%8.4f,%8.4f,%8.4f\n"
                             r
                             (realPart pathPt) (imagPart pathPt)
                             (realPart plotPt) (imagPart plotPt)
    where
      pathPt = pathFn r

------------------------------------
-- Svg Classes and Types
-- Lightweight wrappers around Blaze Svg objects, that take complex points and
-- e.g. lists of points (instead of string containing x,y pairs.
------------------------------------

class IsSvg b c a | b -> c, b->a where
  toSvg :: b -> Svg
  trans :: PlotFn c a -> b -> b
  pointCount :: b -> Int

data SVG c a = forall b. IsSvg b c a => SVG b

instance RealFloat a => IsSvg (SVG c a) c a where
  toSvg (SVG x) = toSvg x
  trans f (SVG x) = SVG (trans f x)
  pointCount (SVG x) = pointCount x

instance RealFloat a => IsSvg [SVG c a] c a where
  toSvg = foldMap toSvg
  trans f = map (trans f)
  pointCount = sum . map pointCount

data Line c a = Line PtOpts (Pt c) (Pt c)

instance (IsComplex c a) => IsSvg (Line c a) c a where
  toSvg (Line b s e) = S.line ! SA.x1 (ptX b s)
                              ! SA.y1 (ptY b s)
                              ! SA.x2 (ptX b e)
                              ! SA.y2 (ptY b e)
  trans f (Line b x y) = Line b (f x) (f y)
  pointCount _ = 2

data Path c a = Path Class Width Colour Dasharray PtOpts [Pt c] --open path, smooth curve.

instance (IsComplex c a) => IsSvg (Path c a) c a where
  toSvg (Path _ _ _ _ _ []) = mempty
  toSvg (Path cl w co d b pts) = S.polyline ! SA.points (ptsVal b pts)
                                            ! SA.class_ cl
                                            ! SA.strokeWidth (toValue w)
                                            ! SA.stroke co
                                            ! SA.strokeDasharray d
                                            ! SA.fill "none"
                                            !? (isHorzVert pts, SA.shapeRendering "crispedges")
    where isHorzVert [z1,z2] | realPart z1 == realPart z2
                            || imagPart z1 == imagPart z2  =  True
          isHorzVert _                                     =  False
  trans f (Path cl w co d b pts) = Path cl w co d b $ map f pts
  pointCount (Path _ _ _ _ _ pts) = length pts

type Saturation = Int  --saturation percent 0 = black, 100 = White

colour :: Saturation -> H.AttributeValue
colour x = "hsl(0, 1%, " <> toValue x <> "%)"

data Closed c a = Closed Class Saturation PtOpts [Pt c]

instance (IsComplex c a) => IsSvg (Closed c a) c a where
  toSvg (Closed _ _ _ []) = mempty
  toSvg (Closed c col b pts) = S.polygon ! SA.points (ptsVal b pts) ! SA.class_ c ! SA.fill (colour col)
  trans f (Closed c col b pts) = Closed c col b $ map f pts
  pointCount (Closed _ _ _ pts) = length pts

data Group c a = Group Class [SVG c a]

instance (IsComplex c a) => IsSvg (Group c a) c a where
  toSvg (Group c xs) = S.g ! SA.class_ c $ traverse_ toSvg xs
  trans f (Group c xs) = Group c $ map (trans f) xs
  pointCount (Group _ xs) = sum $ map pointCount xs

ptsVal :: (IsComplex c a) => PtOpts -> [Pt c] -> AttributeValue
ptsVal _ [] = undefined
ptsVal b (p:px) = pt b p <> foldMap ((" " <>) . pt b) px

pt, ptX, ptY :: (IsComplex c a) => PtOpts -> Pt c -> AttributeValue
ptX b = dblVal . bump b . realPart
ptY b = dblVal . bump b . imagPart
pt b p = ptX b p <> "," <> ptY b p

data PtOpts = SplitZeros | CentreZeros

bump :: RealFloat a => PtOpts -> R a -> a
bump _           x | not (isIEEE x)   = x
bump CentreZeros x | abs x < 0.0001   = x
bump _           x | isNegativeZero x = x - 0.02
                   | x < 0            = x - 0.02
                   | otherwise        = x + 0.02

dblVal :: RealFloat a => a -> AttributeValue
dblVal x = toValue $ showFFloat (Just 3) x ""

------------------------------------
-- Utils
------------------------------------

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

class (Floating c, Eq c, RealFloat a) => IsComplex c a | c -> a where
  realPart, imagPart :: c -> a
  (+:+) :: a -> a -> c
  mkPolar :: a -> a -> c
  phase :: c -> a
  sqrt, exp, log, sin, asin, cos, acos, tan, atan, sinh, asinh, cosh, acosh, tanh, atanh  :: c -> c
  sqrt  = P.sqrt 
  exp   = P.exp  
  log   = P.log  
  sin   = P.sin  
  asin  = P.asin 
  cos   = P.cos  
  acos  = P.acos 
  tan   = P.tan  
  atan  = P.atan 
  sinh  = P.sinh 
  asinh = P.asinh
  cosh  = P.cosh 
  acosh = P.acosh
  tanh  = P.tanh 
  atanh = P.atanh
  
instance RealFloat a => IsComplex (O.Complex a) a where
  realPart = O.realPart
  imagPart = O.imagPart
  (+:+) = (O.:+)
  mkPolar = O.mkPolar
  phase = O.phase

instance RealFloat a => IsComplex (N.Complex a) a where
  realPart = N.realPart
  imagPart = N.imagPart
  (+:+) = (N.:+)
  mkPolar = N.mkPolar
  phase = N.phase

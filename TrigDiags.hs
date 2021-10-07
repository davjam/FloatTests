{-# OPTIONS -Wall -Wpartial-fields #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Data.Foldable
import           Numeric
import           Control.Monad
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Text.Blaze.Svg11 (Svg, AttributeValue, toValue, (!), (!?))
import qualified Text.Blaze.Svg11                as S
import qualified Text.Blaze.Svg11.Attributes     as SA
--import           Text.Blaze.Html.Renderer.Pretty  --looks better in editor
import           Text.Blaze.Html.Renderer.String    --looks better in browser inspection pane
import           Text.Printf

--CHANGE THIS TO TEST EXISTING Data.Complex (DONT FORGET TO CHANGE MAIN)
--import Data.Complex
import MyComplex

--Import this to test with non-IEEE floats
--import Double0

import Debug.Trace

------------------------------------
-- Overall document
------------------------------------

--I put the html output on github following https://stackoverflow.com/questions/8446218/how-to-see-an-html-page-on-github-as-a-normal-rendered-html-page-to-see-preview
--see: https://davjam.github.io/FloatTests/TrigDiags/Curr.html and https://davjam.github.io/FloatTests/TrigDiags/Fixed.html

main :: IO ()
main = do
  --CHANGE THIS TOO
  --writeGraphsDoc True FullGraph currPlots  "TrigDiags\\Curr.html"
  writeGraphsDoc True FullGraph currPlots "TrigDiags\\Fixed.html"
  --writeGraphsDoc True FullGraph currPlots "TrigDiags\\FloatFixed.html"
  --writeGraphsDoc True FullGraph currPlots "TrigDiags\\D0Fixed.html"

_t1 :: IO ()  --used for testing little bits
_t1 = writeGraphsDoc True [AnnulusSectorSeg A3 7 CO] [("id", id), ("acosh", acosh)] "TrigDiags\\Test.html"

writeGraphsDoc :: Graphable a => Bool -> a -> PlotList -> FilePath -> IO ()
writeGraphsDoc includeAxes graphable plotList filePath = writeFile filePath $ renderHtml $ graphsDoc includeAxes graphable plotList

graphsDoc :: Graphable a => Bool -> a -> PlotList -> H.Html
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

type PlotList = [(Title, PlotFn)]
type Title    = String
type PlotFn   = Pt -> Pt
type Pt       = Complex R
type R        = Double
--type R        = Float
--type R        = D0

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
currPlots :: PlotList
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

graph :: Graphable a => Bool -> a -> Title -> PlotFn -> H.Html
graph includeAxes graphable title plotFn = trace title $ do
  H.h1 $ H.toHtml title
  S.svg ! SA.width "800" ! SA.height "800"
        ! SA.viewbox "-4.2 -4.2 8.4 8.4"
        ! SA.class_ "coords"
        $ do
    toSvg fullGraph
    when includeAxes $ S.g ! SA.class_ "axes" $ traverse_ toSvg axes --axes last, so they appear on top.
  H.p $ do "("; H.toHtml $ pointCount fullGraph; " pts)"
  H.hr
  where
    fullGraph = graphSvg plotFn graphable

    axes :: [SVG]
    axes = xAxis ++ yAxis
      where
        xAxis = SVG (Line CentreZeros (negate 4.2 :+ 0) (4.2 :+ 0))
              :  map (tick True)  [-4,-3,-2,-1,1,2,3,4]
              ++ map (tick False) [-pi,-pi/2,pi/2,pi]
        yAxis = map (trans rot90) xAxis
        tick major x = SVG $ Line CentreZeros (x + offs) (x - offs)
          where
            offs | major     = 0 :+ 0.08
                 | otherwise = 0 :+ 0.06

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


allParallels :: [Parallel]
allParallels = [Parallel o pId | o <- enumerate, pId <- enumerate]

parallelSegs :: Parallel -> [ParallelSeg]
parallelSegs (Parallel o PN3) = [ParallelSeg o B s  P3S    | s <- enumerate]
parallelSegs (Parallel o PN2) = [ParallelSeg o B s  P2S    | s <- enumerate]
parallelSegs (Parallel o PN1) = [ParallelSeg o B s (P1S p) | s <- enumerate, p <- enumerate]
parallelSegs (Parallel o PN0) = [ParallelSeg o B s (P0S p) | s <- enumerate, p <- enumerate]
parallelSegs (Parallel o PP0) = [ParallelSeg o T s (P0S p) | s <- enumerate, p <- enumerate]
parallelSegs (Parallel o PP1) = [ParallelSeg o T s (P1S p) | s <- enumerate, p <- enumerate]
parallelSegs (Parallel o PP3) = [ParallelSeg o T s  P3S    | s <- enumerate]
parallelSegs (Parallel o PP2) = [ParallelSeg o T s  P2S    | s <- enumerate]

parallelSegPath :: ParallelSeg -> ParamPath
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
      (P0S _) -> (:+ 0)
      (P1S _) -> (:+ 1)
      P2S     -> (:+ 2)
      P3S     -> (:+ 3)
    (start, end) = case p of
      (P0S P0Sa) -> (tiny, 0.5)
      (P0S P0Sb) -> (0.5, 1)
      (P0S P0Sc) -> (2, 1)
      (P0S P0Sd) -> (2, big)
      (P1S P1Sa) -> (2, tiny)
      (P1S P1Sb) -> (2, big)
      _        -> (tiny, big)
    w = case (o, tb, lr, p) of
      --(Horiz, _, L, P0S _) -> 0.07
      --(Vert,  _, R, P0S _) -> 0.07
      (_   ,  T, _, P0S _) -> 0.03
      (_   ,  B, _, P0S _) -> 0.06
      --(Horiz, B, L, _    ) -> 0.03
      --(Vert,  B, R, _    ) -> 0.03
      _                    -> 0.015
    co = case (o, tb, lr, p) of
{-      
      --colours from https://www.pinterest.co.uk/pin/426012445987636198/
      (Horiz, _ , R, P0S _) -> "rgb(125,255,0)"
      (Horiz, _ , L, P0S _) -> "rgb(125,0,255)"
      (Horiz, T , R, _    ) -> "rgb(255,255,0)"
      (Horiz, T , L, _    ) -> "rgb(255,0,255)"
      (Horiz, B , L, _    ) -> "rgb(0,0,255)"
      (Horiz, B , R, _    ) -> "rgb(0,255,0)"

      (Vert , _ , R, P0S _) -> "rgb(0,255,255"
      (Vert , _ , L, P0S _) -> "rgb(255,0,0)"
      (Vert , T , R, _    ) -> "rgb(0,255,0)"
      (Vert , T , L, _    ) -> "rgb(255,255,0)"
      (Vert , B , L, _    ) -> "rgb(255,0,255)"
      (Vert , B , R, _    ) -> "rgb(0,0,255)"
-}
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

annulusSectorSegPath :: AnnulusSectorSeg -> ParamPath
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

class Graphable a where
  graphSvg :: PlotFn -> a -> SVG

instance Graphable a => Graphable [a] where
  graphSvg plotFn xs = SVG $ map (graphSvg plotFn) xs

instance Graphable FullGraph where
  graphSvg plotFn FullGraph = SVG
    [ SVG $ Group "annuli"    $ map (graphSvg plotFn) allAnnuli
    , SVG $ Group "parallels" $ map (graphSvg plotFn) allParallels
    ]

instance Graphable Parallel where
  graphSvg plotFn p = SVG $ Group (toValue p) $ map (graphSvg plotFn) $ parallelSegs p

instance Graphable ParallelSeg where
  graphSvg = paramPathSvg

instance Graphable Annulus where
  graphSvg plotFn a = SVG $ Group (toValue a) $ map (graphSvg plotFn) $ annulusSectors a

instance Graphable AnnulusSector where
  graphSvg plotFn sec@(AnnulusSector a s) = SVG $ Closed ("s" <> toValue s) sectorCol CentreZeros
                                                $ map snd $ concatMap (paramPathPts plotFn) $ annulusSectorSegs sec
    where
    sectorCol = 80 - (80 * s) `div` annulusSectorCount a

--AnnulusSectorSeg not graphed individually in FullGraph (instead, the points from each seg are concat'd and SVGd as a Closed polygon).
--But we want to be able to do this, e.g. for debugging a single segment.
instance Graphable AnnulusSectorSeg where
  graphSvg = paramPathSvg

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

class HasParamPath a where
  paramPath :: a -> ParamPath

instance HasParamPath ParallelSeg where
  paramPath = parallelSegPath

instance HasParamPath AnnulusSectorSeg where
  paramPath = annulusSectorSegPath

instance HasParamPath ParamPath where
  paramPath = id

paramPathSvg :: HasParamPath a => PlotFn -> a -> SVG
paramPathSvg plotFn = graphSvg plotFn . paramPath

data ParamPath = ParamPath Class Width Colour Dasharray PathFn R R

_paramPathPt :: HasParamPath a => PlotFn -> a -> R -> Pt
_paramPathPt plotFn p = plotFn . pathFn
  where (ParamPath _ _ _ _ pathFn _ _) = paramPath p

type Class = AttributeValue       --used for labelling, not for applying style
type Width = Float
type Colour = S.AttributeValue
type Dasharray = S.AttributeValue

type PathFn = R -> Pt --maybe the original line, or maybe transformed by a PlotFn

instance Graphable ParamPath where
  graphSvg plotFn pp@(ParamPath cl w co d _ _ _) = SVG $ Path cl w co d SplitZeros $ map snd $ paramPathPts plotFn pp

paramPathPts :: HasParamPath a => PlotFn -> a -> [(R, Pt)]
paramPathPts plotFn x = removeRedundantPoints $ pathFnPts (plotFn . pathFn) s e
  where (ParamPath _ _ _ _ pathFn s e) = paramPath x

pathFnPts :: PathFn -> R -> R -> [(R, Pt)]
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
                                                else sp * sqrt other
                        | otherwise = p + signum (other - p) * 0.0005
          where
            sp = sqrt p
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

refinePts :: PathFn -> R -> R -> [(R, Pt)]
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

midR :: R -> R -> R
midR 0 e = e / 2
midR s 0 = s / 2
midR s e = sqrt s * sqrt e

isCloseTo :: Pt -> Pt -> Bool
x `isCloseTo` y = isSmall (realPart z) && isSmall (imagPart z)
  where
    z = x - y
    isSmall d = abs d < 0.05

isOutside, isWayOut :: Pt -> Bool
isOutside = isOut 10
isWayOut  = isOut 1000

isOut :: R -> Pt -> Bool
isOut lim p = isOff (realPart p) || isOff (imagPart p)
  where isOff r | isNaN r      = True
                | isInfinite r = True
                | abs r > lim  = True
                | otherwise    = False

maxNonInfiniteFloat :: forall a. RealFloat a => a
maxNonInfiniteFloat = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e
    a = undefined :: a

minPositiveFloat :: forall a. RealFloat a => a
minPositiveFloat = encodeFloat 1 $ fst (floatRange a) - floatDigits a
  where a = undefined :: a

big, tiny :: R
big  = sqrt $ sqrt maxNonInfiniteFloat
tiny = sqrt $ sqrt minPositiveFloat

rot90, flipHoriz, flipVert :: Pt -> Pt
rot90 = negate . iTimes --(*) (0 :+ (-1))
flipHoriz (x :+ y) = negate x :+ y
flipVert  (x :+ y) = x :+ negate y

removeRedundantPoints :: [(R, Pt)] -> [(R, Pt)]
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

_putParamPath :: HasParamPath a => PlotFn -> a -> IO ()
_putParamPath plotFn x = do
  putStrLn "       R,    Path,        ,    Plot,        "
  mapM_ putPts $ paramPathPts plotFn p
  where
  p@(ParamPath _ _ _ _ pathFn _ _) = paramPath x
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

class IsSvg a where
  toSvg :: a -> Svg
  trans :: PlotFn -> a -> a
  pointCount :: a -> Int

data SVG = forall a. IsSvg a => SVG a

instance IsSvg SVG where
  toSvg (SVG x) = toSvg x
  trans f (SVG x) = SVG (trans f x)
  pointCount (SVG x) = pointCount x

instance IsSvg [SVG] where
  toSvg = foldMap toSvg
  trans f = map (trans f)
  pointCount = sum . map pointCount

data Line = Line PtOpts Pt Pt

instance IsSvg Line where
  toSvg (Line b s e) = S.line ! SA.x1 (ptX b s)
                              ! SA.y1 (ptY b s)
                              ! SA.x2 (ptX b e)
                              ! SA.y2 (ptY b e)
  trans f (Line b x y) = Line b (f x) (f y)
  pointCount _ = 2

data Path = Path Class Width Colour Dasharray PtOpts [Pt] --open path, smooth curve.

instance IsSvg Path where
  toSvg (Path _ _ _ _ _ []) = mempty
  toSvg (Path cl w co d b pts) = S.polyline ! SA.points (ptsVal b pts)
                                            ! SA.class_ cl
                                            ! SA.strokeWidth (toValue w)
                                            ! SA.stroke co
                                            ! SA.strokeDasharray d
                                            ! SA.fill "none"
                                            !? (isHorzVert pts, SA.shapeRendering "crispedges")
    where isHorzVert [x1:+y1,x2:+y2] | x1 == x2 || y1 == y2  =  True
          isHorzVert _                                       =  False
  trans f (Path cl w co d b pts) = Path cl w co d b $ map f pts
  pointCount (Path _ _ _ _ _ pts) = length pts

type Saturation = Int  --saturation percent 0 = black, 100 = White

colour :: Saturation -> H.AttributeValue
colour x = "hsl(0, 1%, " <> toValue x <> "%)"

data Closed = Closed Class Saturation PtOpts [Pt]

instance IsSvg Closed where
  toSvg (Closed _ _ _ []) = mempty
  toSvg (Closed c col b pts) = S.polygon ! SA.points (ptsVal b pts) ! SA.class_ c ! SA.fill (colour col)
  trans f (Closed c col b pts) = Closed c col b $ map f pts
  pointCount (Closed _ _ _ pts) = length pts

data Group = Group Class [SVG]

instance IsSvg Group where
  toSvg (Group c xs) = S.g ! SA.class_ c $ traverse_ toSvg xs
  trans f (Group c xs) = Group c $ map (trans f) xs
  pointCount (Group _ xs) = sum $ map pointCount xs

ptsVal :: PtOpts -> [Pt] -> AttributeValue
ptsVal _ [] = undefined
ptsVal b (p:px) = pt b p <> foldMap ((" " <>) . pt b) px

pt, ptX, ptY :: PtOpts -> Pt -> AttributeValue
ptX b = dblVal . bump b . realPart
ptY b = dblVal . bump b . imagPart
pt b p = ptX b p <> "," <> ptY b p

data PtOpts = SplitZeros | CentreZeros

bump :: PtOpts -> R -> R
bump CentreZeros x | abs x < 0.0001   = x
bump _           x | isNegativeZero x = x - 0.02
                   | x < 0            = x - 0.02
                   | otherwise        = x + 0.02

dblVal :: R -> AttributeValue
dblVal x = toValue $ showFFloat (Just 3) x ""

------------------------------------
-- Utils
------------------------------------

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

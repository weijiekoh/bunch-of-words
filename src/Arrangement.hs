module Arrangement where

import Geometry
import Data.List

-- arrange a list of Rects
arrange :: Double -> Double -> [Rect] -> [RectPos]
arrange _ _ [] = []
{-arrange w h (x:xs) = centralise w h x ++ arrangeRest w h xs-}
arrange w h (x:xs) = centralised ++ vertFindSpaces w h centralised
    where
        centralised = centralise w h x

-- Do a top-to-bottom scan for empty spaces and represent them as RectPoses
vertFindSpaces :: Double -> Double -> [RectPos] -> [RectPos]
vertFindSpaces w h [] = [RectPos (Rect w h) (Point 0 0)] -- empty plane
vertFindSpaces w h rectPoses = vertFindSpaces' 0 w h rectPoses
    where
        -- Uses a pointer
        vertFindSpaces' :: Double -> Double -> Double -> [RectPos] -> [RectPos]
        {-vertFindSpaces' yPtr w h rectPoses = [RectPos (Rect-}
        vertFindSpaces' yPtr w h rectPoses = []

-- Given a current Y-position and a list of RectPoses, and the left and right 
-- edges of the plane, determine the X-coords of gaps in the plane at the Y-pos. Includes the 
findHorizGaps :: Double -> Double -> Double -> [RectPos] -> [(Double, Double)]
{-findHorizGaps _ _ _ [] = []-}
{-findHorizGaps left right ptr rects = -- filter out rects that do not overlap ptr-}
    {-where-}

-- Given a Y-position and a list of RectPoses, return RectPoses which 
-- TODO - use a HOF
filterY :: Double -> [RectPos] -> [RectPos]
filterY _ [] = []
filterY ptr (x:xs)
    | topEdge > ptr = rejected
    | bottomEdge < ptr = rejected
    | topEdge == ptr = accepted
    | bottomEdge == ptr = accepted
    | topEdge < ptr && bottomEdge > ptr = accepted
        where
            topEdge = topY x
            bottomEdge = bottomY x
            rejected = filterY ptr xs
            accepted = [x] ++ rejected

-- Given a current location, the Y-val of the bottom, and a list of RectPoses, 
-- find the Y-val of the top or bottom edge nearest to the current location
findNextTopOrBottomEdge :: Double -> Double -> [RectPos] -> Double
findNextTopOrBottomEdge _ bottom [] = bottom
findNextTopOrBottomEdge ptr bottom (x:xs) 
    | topEdge >= ptr = topEdge
    | topEdge < ptr && bottomEdge >= ptr = bottomEdge
    | bottomEdge < ptr = findNextTopOrBottomEdge ptr bottom xs
        where
            topEdge = topY x
            bottomEdge = bottomY x

-- place a Rect in the middle, given the width and height
centralise :: Double -> Double -> Rect -> [RectPos]
centralise w h x = [RectPos x (Point (w - (w/2) - (width x)/2) (h - (h/2) - (height x)/2))]

{-arrangeRest _ _ [] = []-}
{-arrangeRest w h (x:xs) = [RectPos x (Point (width x) (height x))] ++ arrangeRest w h xs-}
arrangeRest _ _ _ = []

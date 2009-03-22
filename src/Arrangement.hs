module Arrangement where

import Geometry
import Data.List

-- arrange a list of Rects
arrange :: Double -> Double -> [Rect] -> [RectPos]
arrange _ _ [] = []
{-arrange w h (x:xs) = centralise w h x ++ arrangeRest w h xs-}
arrange w h (x:xs) = centralised ++ findVertSpaces w h centralised
    where
        centralised = centralise w h x

-- Do a top-to-bottom scan for empty spaces and represent them as RectPoses
findVertSpaces :: Double -> Double -> [RectPos] -> [RectPos]
findVertSpaces w h [] = [RectPos (Rect w h) (Point 0 0)] -- empty plane
findVertSpaces w h rectPoses = findVertSpaces' w h rectPoses 0
    where
        -- Uses a pointer
        findVertSpaces' :: Double -> Double -> [RectPos] -> Double -> [RectPos]
        {-findVertSpaces' yPtr w h rectPoses = [RectPos (Rect-}
        findVertSpaces' w h rectPoses yPtr
            | h == yPtr = []
            | otherwise = (spaces (yPtr+1)) ++ findVertSpaces' w h rectPoses (findNextTopOrBottomEdge yPtr h rectPoses)
                where 
                    spaces yPtr = map (findSpace yPtr h rectPoses) (findHorizGaps 0 w yPtr rectPoses)

findSpace :: Double -> Double -> [RectPos] -> (Double, Double) -> RectPos
findSpace yPtr bottom rectPoses (x, y) = findVertSpace x y yPtr bottom rectPoses

-- Find the 
findVertSpace :: Double -> Double -> Double -> Double -> [RectPos] -> RectPos
{-findVertSpace _ _ _ [] = RectPos -}
findVertSpace left right yPos bottom rects = RectPos (Rect (right - left) height) (Point left yPos)
    where 
        height = (nextHorizEdge left right yPos bottom rects) - yPos

-- Locates the next (bottom-down) y-position of the next RectPos in the way of 
-- an imaginary vertically downward-moving horizontal line
nextHorizEdge :: Double -> Double -> Double -> Double -> [RectPos] -> Double
nextHorizEdge _ _ _ bottom [] = bottom
nextHorizEdge left right yPos bottom rects
    | min == [] = bottom
    | otherwise = minimum min
        where 
            min = (map topY (filter (belowAndInPath left right yPos) rects))

belowAndInPath :: Double -> Double -> Double -> RectPos -> Bool
belowAndInPath left right yPos r = (topY r) > yPos && inDownwardVertPath left right r

inDownwardVertPath :: Double -> Double -> RectPos -> Bool
inDownwardVertPath left right rect
    | leftEdge <= left && rightEdge >= right = True
    | leftEdge > left && leftEdge < right = True
    | rightEdge > left && rightEdge < right = True
    | otherwise = False
    where
        leftEdge = leftX rect
        rightEdge = rightX rect

topsAndBottoms :: [RectPos] -> [Double]
topsAndBottoms [] = []
topsAndBottoms (x:xs) = [topY x, bottomY x] ++ topsAndBottoms xs

-- Given a current Y-position and a list of RectPoses, and the left and right 
-- edges of the plane, determine the X-coords of gaps in the plane at the Y-pos.
findHorizGaps :: Double -> Double -> Double -> [RectPos] -> [(Double, Double)]
findHorizGaps left right yPos rects = pairs $ [left] ++ sides (filterY yPos rects) ++ [right]
    where
        pairs :: [a] -> [(a, a)]
        pairs [] = []
        pairs (x:y:xs) = [(x, y)] ++ pairs xs
        -- List the left and right sides of the RectPoses
        sides :: [RectPos] -> [Double]
        sides [] = []
        sides (x:xs) = [leftX x, rightX x] ++ sides xs

-- Given a Y-position and a list of RectPoses, return RectPoses which overlap the Y-position
-- TODO - use a HOF
filterY :: Double -> [RectPos] -> [RectPos]
filterY _ [] = []
filterY ptr (x:xs)
    | topEdge > ptr = rejected
    | bottomEdge < ptr = rejected
    {-| topEdge == ptr = accepted-}
    {-| bottomEdge == ptr = accepted-}
    | topEdge == ptr = rejected
    | bottomEdge == ptr = rejected
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
    | ptr == bottom = bottom
    | (topEdge == ptr || bottomEdge == ptr) = findNextTopOrBottomEdge (ptr+1) bottom (x:xs)
    | topEdge > ptr = topEdge
    | topEdge < ptr && bottomEdge > ptr = bottomEdge
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

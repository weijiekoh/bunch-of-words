module Arrangement where

import Geometry
import Data.List
import Data.Maybe

-- arrange a list of Rects
{-arrange :: Double -> Double -> [Rect] -> [RectPos]-}
{-arrange _ _ [] = []-}
{-[>arrange w h (x:xs) = centralise w h x ++ arrangeRest w h xs<]-}
{-arrange w h (x:xs) = centralised ++ findVertSpaces w h centralised-}
    {-where-}
        {-centralised = centralise w h x-}

-- Do a top-to-bottom scan for empty spaces and represent them as RectPoses
findVertSpaces :: Double -> Double -> [RectPos] -> Integer -> [RectPos]
findVertSpaces w h [] _ = [RectPos (Rect w h) (Point 0 0)] -- empty plane
findVertSpaces w h rectPoses c = findVertSpaces' w h rectPoses [] 0 c

findVertSpaces' :: Double -> Double -> [RectPos] -> [RectPos] -> Double -> Integer -> [RectPos]
findVertSpaces' w h rectPoses spaces yPtr c
    | c == 0 = []
    | h == yPtr = []
    | otherwise =  foundSpaces ++ findVertSpaces' w h rectPoses (foundSpaces ++ spaces) nextEdge (c-1)
        where 
            foundSpaces = filter hasArea (spacesFromGaps $ yPtr+1)
            spacesFromGaps yPtr = map (findSpace yPtr h) (findHorizGaps 0 w yPtr combined)

            findSpace :: Double -> Double -> (Double, Double) -> RectPos -- wrapper for findVertSpace
            findSpace yPtr bottom (a, b) = findVertSpace a b yPtr bottom combined

            combined = spaces ++ rectPoses
            nextEdge = findNextTopOrBottomEdge (yPtr+1) h rectPoses

-- Find the vertical space of width (right - left) that is empty from yPos down
findVertSpace :: Double -> Double -> Double -> Double -> [RectPos] -> RectPos
findVertSpace left right yPos bottom rects = RectPos (Rect (right-left) height) (Point left yPos)
    where 
        height = (nextHorizEdge left right yPos bottom (rects)) - yPos + 1

findHorizSpace :: Double -> Double -> Double -> Double -> [RectPos] -> RectPos
findHorizSpace top bottom xPos edge rects = RectPos (Rect width (bottom-top)) (Point xPos top)
    where 
        width = (nextVertEdge top bottom xPos edge rects) - xPos

-- Locates the next (bottom-down) y-position of the next RectPos in the way of 
-- an imaginary vertically downward-moving horizontal line
nextHorizEdge :: Double -> Double -> Double -> Double -> [RectPos] -> Double
nextHorizEdge = nextEdge topY belowAndInPath

nextVertEdge :: Double -> Double -> Double -> Double -> [RectPos] -> Double
nextVertEdge = nextEdge leftX rightAndInPath

nextEdge :: (RectPos -> Double) -> (Double -> Double -> Double -> RectPos -> Bool) 
                -> Double -> Double -> Double -> Double -> [RectPos] -> Double
nextEdge startRectEdge directionAndInPath side1 side2 ptr edge rects
    | min == [] = edge
    | otherwise = minimum min
        where 
            min = (map startRectEdge (filter (directionAndInPath side1 side2 ptr) rects))

-- Check if a RectPos is in the path of an imaginary horizontal/vertical line 
-- moving vertically down/right
nextAndInPath inPath startRectEdge side1 side2 ptr r 
    = (startRectEdge r) > ptr && inPath side1 side2 r

belowAndInPath :: Double -> Double -> Double -> RectPos -> Bool
{-belowAndInPath left right yPos r = (topY r) > yPos && inDownwardVertPath left right r-}
belowAndInPath = nextAndInPath inDownwardVertPath topY

rightAndInPath :: Double -> Double -> Double -> RectPos -> Bool
rightAndInPath = nextAndInPath inRightwardHorizPath leftX

inPath :: (RectPos -> Double) -> (RectPos -> Double) -> Double -> Double -> RectPos -> Bool
inPath side1 side2 left right rect
    | aEdge <= left && bEdge >= right = True
    | aEdge > left && aEdge < right = True
    | bEdge > left && bEdge < right = True
    | otherwise = False
    where
        aEdge = side1 rect
        bEdge = side2 rect

inDownwardVertPath :: Double -> Double -> RectPos -> Bool
inDownwardVertPath = inPath leftX rightX

inRightwardHorizPath :: Double -> Double -> RectPos -> Bool
inRightwardHorizPath = inPath topY bottomY

-- Utility function to get a list of horizontal/vertical sides
listSides :: (RectPos -> Double) -> (RectPos -> Double) -> [RectPos] -> [Double]
listSides _ _ [] = []
listSides side1 side2 (x:xs) = [side1 x, side2 x] ++ listSides side1 side2 xs

topsAndBottoms :: [RectPos] -> [Double]
topsAndBottoms = listSides topY bottomY

leftsAndRights :: [RectPos] -> [Double]
leftsAndRights = listSides leftX rightX

-- Given a current X/Y-position and a list of RectPoses, and the top/left and bottom/right 
-- edges of the plane, determine the Y/X-coords of gaps in the plane at the X/Y-pos.
findHorizGaps :: Double -> Double -> Double -> [RectPos] -> [(Double, Double)]
findHorizGaps = findGaps filterY leftX rightX

findVertGaps :: Double -> Double -> Double -> [RectPos] -> [(Double, Double)]
findVertGaps = findGaps filterX topY bottomY

findGaps :: (Double -> [RectPos] -> [RectPos]) -> (RectPos -> Double) -> (RectPos -> Double) 
                -> Double -> Double -> Double -> [RectPos] -> [(Double, Double)]

findGaps fil edge1 edge2 
    side1 side2 ptr rects 
    =  filter notEqual (pairs $  ([side1] ++ sides (fil ptr (sortBy compareRectPos rects)) ++ [side2]))
        where
            compareRectPos x y = compare (edge1 x) (edge1 y)
            notEqual :: (Eq a) => (a, a) -> Bool
            notEqual (x, y) = x /= y
            pairs :: [a] -> [(a, a)]
            pairs [] = []
            pairs (x:y:xs) = [(x, y)] ++ pairs xs
            -- List the top/left and bottom/right sides of the RectPoses
            sides :: [RectPos] -> [Double]
            sides [] = []
            sides (x:xs) = [edge1 x, edge2 x] ++ sides xs

-- Given a Y-position and a list of RectPoses, return RectPoses which overlap the Y-position
-- TODO - use a HOF
filterY :: Double -> [RectPos] -> [RectPos]
filterY ptr = filter (acceptEdge topY bottomY ptr)

filterX :: Double -> [RectPos] -> [RectPos]
filterX ptr = filter (acceptEdge leftX rightX ptr)

acceptEdge :: (RectPos -> Double) -> (RectPos -> Double) -> Double -> RectPos -> Bool
acceptEdge edge1 edge2 ptr rect
    | e1 < ptr && e2 > ptr = True
    | otherwise = False
        where
            e1 = edge1 rect
            e2 = edge2 rect

-- Given a current location, the X/Y-val of the right/bottom, and a list of RectPoses, 
-- find the X/Y-val of the left/top or right/bottom edge nearest to the current location
findNextTopOrBottomEdge :: Double -> Double -> [RectPos] -> Double
findNextTopOrBottomEdge = findNextEdge topY bottomY

findNextLeftOrRightEdge :: Double -> Double -> [RectPos] -> Double
findNextLeftOrRightEdge = findNextEdge leftX rightX

findNextEdge :: (RectPos -> Double) -> (RectPos -> Double) -> Double -> Double -> [RectPos] -> Double
findNextEdge _ _ _ edge [] = edge
findNextEdge side1 side2 ptr bottom rects
    | isJust found = fromJust found
    | isNothing found = bottom
    where
        found = find (> ptr) sides
        r = head sides
        sides = sort $ listSides side1 side2 rects
    {-| ptr == bottom = bottom-}
    {-| (s1 == ptr || s2 == ptr) = findNextEdge side1 side2 (ptr+1) bottom (x:xs)-}
    {-| s1 > ptr = s1-}
    {-| s1 < ptr && s2 > ptr = s2-}
    {-| s2 < ptr = findNextEdge side1 side2 ptr bottom xs-}
        {-where-}
            {-s1 = side1 x-}
            {-s2 = side2 x-}

-- place a Rect in the middle, given the width and height
centralise :: Double -> Double -> Rect -> [RectPos]
centralise w h x = [RectPos x (Point (w - (w/2) - (width x)/2) (h - (h/2) - (height x)/2))]

{-arrangeRest _ _ [] = []-}
{-arrangeRest w h (x:xs) = [RectPos x (Point (width x) (height x))] ++ arrangeRest w h xs-}
arrangeRest _ _ _ = []

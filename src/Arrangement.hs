module Arrangement where

import Geometry
import Data.List
import Data.Maybe

-- arrange a list of Rects
arrange :: Double -> Double -> [Rect] -> [RectPos]
arrange _ _ [] = []
arrange w h rects = existing ++ rest
    where
        {-existing = centralise w h (head sorted)-}
        existing = [RectPos (head sorted) (Point 0 0)]
        rest = arrangeRest w h existing (tail sorted)
        {-sorted = sortBy (\x y -> compare (area y) (area x)) rects-}
        sorted = sortBy compareRect rects

-- Carry out the arrangement assuming that one or more RectPoses have been 
-- placed (in the existing list)
{-arrangeRest :: Double -> Double -> [RectPos] -> [Rect] -> [RectPos]-}
arrangeRest _ _ existing [] = existing
arrangeRest w h existing (x:xs)
    | isNothing bestFit = arrange w h (scale spaces existing (x:xs))
    {-| isNothing bestFit = []-}
    | otherwise = [lumped] ++ arrangeRest w h (existing ++ [lumped]) xs
        where
            lumped = lumpToCentre w h (fromJust bestFit) x
            bestFit = findBestFit x (filter (\ y -> x `fitsIn` (rect y)) spaces)
            spaces = findVertSpaces w h existing ++ findHorizSpaces w h existing

lumpToCentre :: Double -> Double -> RectPos -> Rect -> RectPos
lumpToCentre _ _ pos rect = RectPos (rect) (coord pos)

-- Given some existing RectPoses and some unplaced Rects, none of which can 
-- fit into any RectPos, return a list of Rects that will definitely fit
scale :: [RectPos] -> [RectPos] -> [Rect] -> [Rect]
scale _ _ [] = []
scale _ [] unscaled = unscaled
scale spaces existing unscaled = scale' spaces existing (sortBy compareRect unscaled)

scale' :: [RectPos] -> [RectPos] -> [Rect] -> [Rect]
scale' spaces existing (x:xs) = map scaleRect ((map rect existing) ++ x:xs)
    where 
        bestFitting = fromJust $ findBestFit x spaces
        {-bestFitting = maximumBy compareFitScore (map rect spaces)-}
        {-compareFitScore y = compare (fitScore y) (fitScore x)-}
        {-acceptableUnscaleds = filter (\ r -> (area r) -}
                                            {-/ (area x) -}
                                            {->= 0.8) unscaled-}
        -- now, based on the dimensions of x and bestFitting, compose scaleRect
        -- scaleRect scales a given rectangle to dimensions determined by how x
        -- fits into bestFitting
        scaleRect r = Rect (width r * scaleRatio) (height r * scaleRatio)
            where
                scaleRatio
                    | (width x) - (width $ rect bestFitting) >= (height x) - (height $ rect bestFitting)
                        = (height $ rect bestFitting) / (height x)
                    | otherwise = (width $ rect bestFitting) / (width x)

-- place a Rect in the middle, given the width and height
centralise :: Double -> Double -> Rect -> [RectPos]
centralise w h x = [RectPos x (Point (w - (w/2) - (width x)/2) 
                                     (h - (h/2) - (height x)/2)
                              )]

-- Check if a Rect a fits in another Rect b
fitsIn :: Rect -> Rect -> Bool
a `fitsIn` b = not $ (width a) > (width b) || (height a) > (height b)

-- Locate a Rect that fits a given Rect the best
findBestFit :: Rect -> [RectPos] -> Maybe RectPos
findBestFit _ [] = Nothing
findBestFit r rects = Just $ maximumBy compareFitScore rects
    where
        compareFitScore x y = compare (fitScore (rect y) r) (fitScore (rect x) r)

-- The larger the fitScore, the "better" one Rect a fits into another, b
fitScore :: Rect -> Rect -> Double
fitScore a b -- b is larger than / equal to a
    | widthA == widthB && heightA == heightB = 3
    | widthA == widthB = 2
    | heightA == heightB = 2
    -- 
    | otherwise = ((widthB - widthA) / (heightB - heightA)) / 
                    ((area b) / (area a))
        where
            heightA = height a
            heightB = height b
            widthA = width a
            widthB = width b
----------------------------

-- General function for determining the empty spaces between RectPoses in a list
findSpaces _ _ _ w edge [] = [RectPos (Rect w edge) (Point 0 0)] -- empty plane
findSpaces findGapsF findSpaceF findEdgeF w edge rectPoses = findSpaces' findGapsF findSpaceF findEdgeF w edge rectPoses [] 0

findSpaces' findGapsF findSpaceF findEdgeF w edge rectPoses spaces ptr 
    | edge == ptr = []
    | otherwise =  foundSpaces ++ findSpaces' findGapsF findSpaceF findEdgeF w edge rectPoses (foundSpaces ++ spaces) nextEdge
        where 
            foundSpaces = filter hasArea (spacesFromGaps $ ptr)
            spacesFromGaps ptr = map (findSpace ptr edge) (findGapsF 0 w (ptr+1) combined)

            findSpace :: Double -> Double -> (Double, Double) -> RectPos -- wrapper for findVertSpace
            findSpace ptr bottom (a, b) = findSpaceF a b ptr bottom combined

            combined = spaces ++ rectPoses
            nextEdge = findEdgeF ptr edge rectPoses

-- Do a top-to-bottom scan for empty spaces and represent them as RectPoses
findVertSpaces :: Double -> Double -> [RectPos] -> [RectPos]
findVertSpaces = findSpaces findHorizGaps findVertSpace findNextTopOrBottomEdge

-- Do a left-to-right scan for empty spaces and represent them as RectPoses
findHorizSpaces :: Double -> Double -> [RectPos] -> [RectPos]
findHorizSpaces = findSpaces findVertGaps findHorizSpace findNextLeftOrRightEdge


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

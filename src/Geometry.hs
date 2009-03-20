module Geometry where

data Point = Point { x :: Double, y :: Double } deriving Show

data Rect = Rect { width :: Double, height :: Double} deriving Show

data RectPos = RectPos { rect :: Rect, coord :: Point } deriving Show

-- Compare by area
compareRect :: Rect -> Rect -> Ordering
compareRect x y = compare (area x) (area y)

-- Area
area :: Rect -> Double
area x = (width x) * (height x)

-- The Y-coordinate of the top edge of a rectangle
topY :: RectPos -> Double
topY = y . coord

-- The Y-coordinate of the bottom edge of a rectangle
bottomY :: RectPos -> Double
bottomY rp = topY rp + height (rect rp)

-- Compare two RectPos types and return the one with the higher (smaller) topY
compareTopY :: RectPos -> RectPos -> Ordering
compareTopY a b = compare (topY a) (topY b)

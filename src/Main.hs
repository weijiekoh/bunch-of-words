import Data.List (sortBy)

import Display
import Geometry
import Arrangement

rects = [Rect 150 50, Rect 100 50, Rect 200 300, Rect 40 70]


rectPoses = [
            RectPos (Rect 150 50) (Point 40 20)
            , RectPos (Rect 150 50) (Point 290 20)
            , RectPos (Rect 40 80) (Point 90 120)
            , RectPos (Rect 20 33) (Point 300 200)
             ]

main = do
    {-displayWindow $ arrange 800 600 rects-}
    displayWindow rectPoses
    {-print $ filterY 21 rectPoses-}
    {-print $ findHorizGaps 0 800 200 rectPoses-}
    {-print $ topsAndBottoms rectPoses-}
    {-print $ inDownwardVertPath 0 40 (RectPos (Rect 150 50) (Point 40 20))-}

    {-print $ belowAndInPath 0 41 0 (rectPoses !! 0)-}
    {-print $ nextHorizEdge 0 41 0 rectPoses-}
    {-print $ findVertSpace 0 41 0 rectPoses-}

    {-print $ findVertSpaces 800 600 rectPoses-}
    {-displayWindow $ findVertSpaces 800 600 rectPoses-}

    {-print $ findHorizGaps 0 800 70 rectPoses-}
    {-print $ findSpace 60 600 rectPoses (0, 40)-}
    {-print $ findNextTopOrBottomEdge 0 600 rectPoses-}

    displayOneByOne $ findVertSpaces 800 600 rectPoses

    {-displayOneByOne [-}
                    {-RectPos {rect = Rect {width = 800.0, height = 20.0}, coord = Point {x = 0.0, y = 0.0}}-}
                    {-, RectPos {rect = Rect {width = 40.0, height = 580.0}, coord = Point {x = 0.0, y = 20.0}}-}
                    {-, RectPos {rect = Rect {width = 40.0, height = 530.0}, coord = Point {x = 0.0, y = 70.0}}-}
                    {-,RectPos {rect = Rect {width = 610.0, height = 580.0}, coord = Point {x = 190.0, y = 20.0}}-}
                    {-,RectPos {rect = Rect {width = 610.0, height = 530.0}, coord = Point {x = 190.0, y = 70.0}}-}
                    {-]-}

displayOneByOne (x:xs) = do
    displayWindow [x]
    displayOneByOne xs


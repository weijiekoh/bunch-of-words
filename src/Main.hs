import Data.List (sortBy)

import Display
import Geometry
import Arrangement

rects = [Rect 150 50, Rect 100 50, Rect 200 300, Rect 40 70]

rectPoses = [
            RectPos (Rect 150 50) (Point 40 20),
            RectPos (Rect 40 80) (Point 90 120),
            RectPos (Rect 20 33) (Point 300 200)
             ]

main = do
    {-displayWindow $ arrange 800 600 rects-}
    {-displayWindow rectPoses-}
    print $ filterY 21 rectPoses

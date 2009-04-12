import Data.List
import Numeric

import Display
import Geometry
import Arrangement


rects = [
        Rect 300 300
        , Rect 300 300
        , Rect 300 300
        , Rect 300 300
        , Rect 300 300
        ]
rects1= [Rect 150 150, Rect 100 100, Rect 300 300, Rect 40 40]
rects2= [Rect 10 150, Rect 100 100, Rect 300 300, Rect 40 40]
rects3 = [Rect 50 50, Rect 100 50, Rect 200 300, Rect 40 70]

rectPoses = [
            RectPos (Rect 150 250) (Point 140 20)
            , RectPos (Rect 100 200) (Point 400 300)
             ]

main = do
    --weird
    {-print $ fitScore (Rect 10 10) (Rect 100 50)-}
    {-print $ map (fitScore (Rect 10 10)) rects-}
    {-print $ findBestFit (Rect 10 10) rects2-}
    {-print $ (Rect 10 10) `fitsIn` (Rect 9 20)-}
    {-displayWindow $ arrange 800 600 [minimumBy compareRect rects]-}
    displayWindow $ arrange 800 600 rects
    {-displayWindow $ arrange 800 600 rects3-}
    {-displayWindow $ filter (\ x -> (rect x) `fitsIn` (Rect 100 100)) rectPoses-}
    {-displayWindow rectPoses-}
    {-print $ filterY 21 rectPoses-}
    {-print $ filterX 21 rectPoses-}
    {-displayWindow $ filterX 301 rectPoses-}

    {-print $ findHorizGaps 0 800 200 rectPoses-}

    {-print $ findHorizGaps 0 800 1 [RectPos (Rect 100 100) (Point 200 0), RectPos (Rect 100 100) (Point 0 0)]-}

    {-print $ findVertGaps 0 800 91 rectPoses-}
    {-print $ topsAndBottoms rectPoses-}
    {-print $ inDownwardVertPath 0 40 (RectPos (Rect 150 50) (Point 40 20))-}

    {-print $ belowAndInPath 0 41 0 (rectPoses !! 0)-}
    {-print $ nextHorizEdge 0 41 0 600 rectPoses-}
    {-print $ nextVertEdge 0 41 0 800 rectPoses-}
    {-print $ findVertSpace 0 41 0 800 rectPoses-}
    {-print $ findHorizSpace 0 41 0 800 rectPoses-}

    {-print $ findVertSpaces 800 600 rectPoses-}

    {-print $ findVertSpaces 800 600 [RectPos (Rect 800 600) (Point 0 0)]-}

    {-displayWindow $ findVertSpaces 800 600 rectPoses-}
    {-displayWindow $ findHorizSpaces 600 800 rectPoses-}

    {-let a = [   RectPos (Rect 100 50) (Point 0 0), -}
                {-RectPos (Rect 100 50) (Point 200 0)]-}
    {-print $ findHorizGaps 0 800 1 a-}

    {-displayWindow $ findVertSpaces 800 600 [RectPos (Rect 100 100) (Point 250 50)]-}

    {-displayWindow $ findVertSpaces 800 600 [RectPos (Rect 100 100) (Point 250 50),-}
                                            {-RectPos (Rect 100 100) (Point 250 200),-}
                                            {-RectPos (Rect 100 100) (Point 250 350)] -}

    {-displayWindow $ findVertSpaces 800 600 [RectPos (Rect 100 100) (Point 250 50),-}
                                            {-RectPos (Rect 100 100) (Point 350 250),-}
                                            {-RectPos (Rect 100 100) (Point 450 350)] -}

    {-displayWindow $ findVertSpaces 800 600 [RectPos (Rect 100 100) (Point 250 50),-}
                                            {-RectPos (Rect 100 100) (Point 400 150),-}
                                            {-RectPos (Rect 100 100) (Point 550 350)]-}

    {-let b = [-}
                {-RectPos (Rect 800 20) (Point 0 0),-}
                {-RectPos (Rect 100 640) (Point 0 20),-}
                {-RectPos (Rect 650 640) (Point 150 20)-}
            {-]-}
    {-let c = [RectPos (Rect 50 50) (Point 100 20)]-}
    {-displayWindow $ findVertSpaces' 800 600 c b 70-}

    {-displayWindow $ findSpacesAtYPos 800 600 [RectPos (Rect 300 300) (Point 250 150)] 450-}
    {-print  $ findVertSpaces 800 600 [RectPos (Rect 300 300) (Point 250 150)]-}

    {-displayWindow $ findVertSpaces 800 600 [RectPos {rect = Rect {width = 800.0, height = 150.0}, coord = Point {x = 0.0, y = 1.0}},-}
                {-RectPos {rect = Rect {width = 250.0, height = 450.0}, coord = Point{x = 0.0, y = 151.0}},-}
                {-RectPos (Rect 300 300) (Point 250 150),-}
                {-[>RectPos {rect = Rect {width = 800.0, height = 150.0}, coord = Point {x = 0.0, y = 451.0}},<]-}
                {-RectPos {rect = Rect {width = 250.0, height = 450.0}, coord = Point {x = 550.0, y = 151.0}}-}
                    {-]-}


    {-print $ findSpace 60 600 rectPoses (0, 40)-}
    {-print $ findNextTopOrBottomEdge 0 600 rectPoses-}
    {-print $ findNextLeftOrRightEdge 700 800 rectPoses-}

    {-print  $ findVertSpaces 800 600 rectPoses-}
    {-displayWindow $ findVertSpaces 800 600 rectPoses-}

    {-displayOneByOne $ findVertSpaces 800 600 rectPoses-}
    {-displayOneByOne $ findHorizSpaces 800 600 rectPoses-}

displayOneByOne (x:xs) = do
    displayWindow [x]
    displayOneByOne xs


import Test.HUnit
import Geometry
import Arrangement

rectPoses = [
            RectPos (Rect 150 50) (Point 40 20)
            , RectPos (Rect 150 50) (Point 290 20)
            , RectPos (Rect 40 80) (Point 90 120)
            , RectPos (Rect 20 33) (Point 300 200)
             ]


tests = TestList [  TestLabel "findNextLeftOrRightEdge" test1,
                    TestLabel "findNextLeftOrRightEdge" test2,
                    TestLabel "findNextLeftOrRightEdge" test3
                    ]
main = do
    runTestTT tests

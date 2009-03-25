module Display where

import Graphics.UI.Gtk hiding (fill)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events

import Geometry

wWidth :: Int
wHeight :: Int
wWidth = 800
wHeight = 600

displayWindow :: [RectPos] -> IO ()
displayWindow rects = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Bunch Of Words",
                 windowDefaultWidth := wWidth+6, windowDefaultHeight := wHeight+6,
                 containerBorderWidth := 0 ]
     windowSetPosition window WinPosCenter

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas
     {-widgetModifyBg canvas StateNormal (Color 65535 65535 65535)-}

     widgetShowAll window 
     drawin <- widgetGetDrawWindow canvas
     onExpose canvas (\x -> do renderWithDrawable drawin (drawRectangles rects)
                               return (eventSent x))
    
     onDestroy window mainQuit
     mainGUI

oneRect :: [RectPos] -> Render ()
oneRect [] = stroke

oneRect (r:rs) = do 
    let w = width (rect r)
    let h = height (rect r)
    let xPos = x (coord r)
    let yPos = y (coord r)

    setSourceRGBA 0.3 0.3 1 0.5
    setLineWidth 1
    rectangle xPos yPos w h
    stroke

    setSourceRGBA 0.3 0.3 1 0.5
    rectangle xPos yPos w h
    fill

    oneRect rs

drawRectangles :: [RectPos] -> Render ()
drawRectangles rects = do
    setSourceRGB 0.3 0.3 0.3
    setLineWidth 0
    rectangle 1 1 (fromIntegral wWidth) (fromIntegral wHeight)
    stroke

    oneRect rects

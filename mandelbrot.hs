import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Events
import Control.Concurrent.MVar
import Data.Array.MArray
import Data.Word
import Control.Monad

red = Color 65535 0 0
white = Color 65535 65535 65535

width = 525 :: Int
height = 300 :: Int

main :: IO ()
main = do
    initGUI
    win <- windowNew
    windowSetTitle win "The Mandelbrot Set"
    win `onDestroy` mainQuit

    mandelbrot <- makeMandelbrotPixbuf
 
    canvas <- drawingAreaNew
    canvas `onSizeRequest` return (Requisition width height)
    canvas `onExpose` drawCanvas canvas mandelbrot
 
    containerAdd win canvas
    widgetShowAll win
    mainGUI

makeMandelbrotPixbuf :: IO Pixbuf
makeMandelbrotPixbuf = do
    pb <- pixbufNew ColorspaceRgb False 8 width height
    pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
    rowStride <- pixbufGetRowstride pb
    forM_ mandelbrotSet (\row ->
        forM_ row (\(r, c, v) -> do
            writeArray pbData (c*3 + r*rowStride) v
            writeArray pbData (c*3 + r*rowStride+1) v
            writeArray pbData (c*3 + r*rowStride+2) v
            ))
    return pb

mandelbrotSet :: [[(Int, Int, Word8)]]
mandelbrotSet =
    [[(row, col, mandelbrot (col, row))
        | col <- [0..(width-1)]]
            | row <- [0..(height-1)]]

mandelbrot :: (Int, Int) -> Word8
mandelbrot (x, y) =
    let p' = p (scaleX x, scaleY y)
        iterations = take 1000 $ iterate p' (0, 0)
        (left, right) = span (\(x,y) -> x*x + y*y <= 2*2) iterations
        in if right == []
              then 0
              else fromIntegral $ max 0 $ 256 - 8*(length left)

scaleX :: Int -> Double
scaleX x = (fromIntegral x)/(fromIntegral width)*3.5 - 2.5

scaleY :: Int -> Double
scaleY y = -((fromIntegral y)/(fromIntegral height)*2 - 1)

p :: (Double, Double) -> (Double, Double) -> (Double, Double)
p (x0, y0) (x, y) = (x*x - y*y + x0, 2*x*y + y0)

drawCanvas :: DrawingArea -> Pixbuf -> event -> IO Bool
drawCanvas canvas pb _evt = do
    dw <- widgetGetDrawWindow canvas
    drawWindowClear dw
    gc <- gcNew dw
    drawPixbuf dw gc pb 0 0 0 0 width height RgbDitherNone 0 0
    return True -- everything is OK

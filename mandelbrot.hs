import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Events
import Control.Concurrent.MVar
import Data.Array.MArray
import Data.Word
import Control.Monad
import Control.Concurrent

red = Color 65535 0 0
white = Color 65535 65535 65535

windowWidth = 525 :: Int
windowHeight = 300 :: Int

main :: IO ()
main = do
    initGUI
    win <- windowNew
    windowSetTitle win "The Mandelbrot Set"
    win `onDestroy` mainQuit

    mandelPb <- newMVar =<< pixbufNew ColorspaceRgb False 8 windowWidth windowHeight
 
    canvas <- drawingAreaNew
    canvas `onSizeRequest` return (Requisition windowWidth windowHeight)
    canvas `onExpose` drawCanvasHandler canvas mandelPb

    forkIO $ makeMandelbrotPixbuf canvas mandelPb
 
    containerAdd win canvas
    widgetShowAll win
    mainGUI

makeMandelbrotPixbuf :: DrawingArea -> MVar Pixbuf -> IO ()
makeMandelbrotPixbuf canvas pbVar = do
    pb <- readMVar pbVar
    pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
    rowStride <- pixbufGetRowstride pb
    putStrLn "calculating..."
    forM_ [0..(windowWidth-1)] (\c -> do
        forM_ [0..(windowHeight-1)] (\r ->
            let v = mandelbrot (c, r) in do
                writeArray pbData (c*3 + r*rowStride) v
                writeArray pbData (c*3 + r*rowStride+1) v
                writeArray pbData (c*3 + r*rowStride+2) v
            )
        postGUIAsync $ do {drawCanvas canvas pbVar c 0 1 windowHeight; return ()})
    putStrLn "done"
    return ()

mandelbrot :: (Int, Int) -> Word8
mandelbrot (x, y) =
    let p' = p (scaleX x, scaleY y)
        iterations = {-# SCC "iterations" #-} take 1000 $ iterate p' (0, 0)
        (left, right) = {-# SCC "span" #-} span (\(x,y) -> x*x + y*y <= 4) iterations
        in if right == []
              then 0
              else fromIntegral $ max 0 $ 256 - 8*(length left)

scaleX :: Int -> Double
scaleX x = (fromIntegral x)/(fromIntegral windowWidth)*3.5 - 2.5

scaleY :: Int -> Double
scaleY y = -((fromIntegral y)/(fromIntegral windowHeight)*2 - 1)

p :: (Double, Double) -> (Double, Double) -> (Double, Double)
p (x0, y0) (x, y) = (x*x - y*y + x0, 2*x*y + y0)

drawCanvasHandler :: DrawingArea -> MVar Pixbuf -> event -> IO Bool
drawCanvasHandler canvas pbVar _evt = drawCanvas canvas pbVar 0 0 windowWidth windowHeight

drawCanvas :: DrawingArea -> MVar Pixbuf -> Int -> Int -> Int -> Int -> IO Bool
drawCanvas canvas pbVar x y width height = do 
    pb <- readMVar pbVar
    dw <- widgetGetDrawWindow canvas
    gc <- gcNew dw
    drawPixbuf dw gc pb x y x y width height RgbDitherNone 0 0
    return True -- everything is OK

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Array.MArray
import Data.Word
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC

windowWidth = 1050 :: Int
windowHeight = 600 :: Int

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

    makeMandelbrotPixbuf canvas mandelPb

    containerAdd win canvas
    widgetShowAll win
    mainGUI

makeMandelbrotPixbuf :: DrawingArea -> MVar Pixbuf -> IO ()
makeMandelbrotPixbuf canvas pbVar = do
    ch <- (newChan :: IO (Chan Int))
    replicateM 2 $ forkIO $ worker canvas pbVar ch
    forM_ [0..(windowHeight-1)] (\r -> do
            writeChan ch r)
    return ()

worker :: DrawingArea -> MVar Pixbuf -> Chan Int -> IO ()
worker canvas pbVar ch = do
    pb <- readMVar pbVar
    pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
    rowStride <- pixbufGetRowstride pb
    forever $ do
        r <- readChan ch
        forM_ [0..(windowWidth-1)] (\c -> do
            let v = mandelbrot (c, r) in do
                writeArray pbData (c*3 + r*rowStride) v
                writeArray pbData (c*3 + r*rowStride+1) v
                writeArray pbData (c*3 + r*rowStride+2) v
            )
        postGUIAsync $ do {drawCanvas canvas pbVar 0 r windowWidth 1; return ()}


drawCanvasHandler :: DrawingArea -> MVar Pixbuf -> event -> IO Bool
drawCanvasHandler canvas pbVar _evt = drawCanvas canvas pbVar 0 0 windowWidth windowHeight

drawCanvas :: DrawingArea -> MVar Pixbuf -> Int -> Int -> Int -> Int -> IO Bool
drawCanvas canvas pbVar x y width height = do
    pb <- readMVar pbVar
    dw <- widgetGetDrawWindow canvas
    gc <- gcNew dw
    drawPixbuf dw gc pb x y x y width height RgbDitherNone 0 0
    return True

mandelbrot :: (Int, Int) -> Word8
mandelbrot (x, y) =
    let coord = (scaleX x, scaleY y)
        p' = p coord
        iterations = take 1000 $ iterate p' (0, 0)
        (left, right) = span (\(x,y) -> x*x + y*y <= 4) iterations
        in if knownZero coord || right == []
              then 0
              else fromIntegral $ max 0 $ 256 - 8*(length left)

        where knownZero (x, y) =
                  let q = (x - 0.25)^2 + y^2
                      in q*(q + (x - 0.25)) < y^2 / 4 ||
                          (x+1)^2 + y^2 < 1/16

              scaleX :: Int -> Double
              scaleX x = (fromIntegral x)/(fromIntegral windowWidth)*3.5 - 2.5

              scaleY :: Int -> Double
              scaleY y = -((fromIntegral y)/(fromIntegral windowHeight)*2 - 1)

              p :: (Double, Double) -> (Double, Double) -> (Double, Double)
              p (x0, y0) (x, y) = (x*x - y*y + x0, 2*x*y + y0)

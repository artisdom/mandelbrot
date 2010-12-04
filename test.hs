import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Events
import Control.Concurrent.MVar

main :: IO ()
main = do
    initGUI
    window <- windowNew
    drawingArea <- drawingAreaNew
    pixmapVar <- (newEmptyMVar :: IO (MVar Pixmap))
    set window [containerChild := drawingArea,
                windowDefaultWidth := 500,
                windowDefaultHeight := 500]
    onExposeRect drawingArea (exposeHandler drawingArea pixmapVar)
    onConfigure drawingArea (configureHandler pixmapVar)
    widgetShowAll window
    onDestroy window mainQuit
    mainGUI

configureHandler :: MVar Pixmap -> Event -> IO Bool
configureHandler pixmapVar event = do
    pixmap <- pixmapNew (Nothing :: Maybe Pixmap) 500 500 (Just 24)
    empty <- isEmptyMVar pixmapVar
    if empty
        then putMVar pixmapVar pixmap
        else do swapMVar pixmapVar pixmap
                return ()
    pixmapGc <- gcNew pixmap
    drawRectangle pixmap pixmapGc True 50 60 200 100
    return True

exposeHandler :: DrawingArea -> MVar Pixmap -> Rectangle -> IO ()
exposeHandler drawingArea pixmapVar (Rectangle x y width height) = do
    pixmap <- readMVar pixmapVar
    drawWindow <- widgetGetDrawWindow drawingArea
    style <- widgetGetStyle drawingArea
    state <- widgetGetState drawingArea
    fg <- styleGetBackground style state
    gc <- gcNew drawWindow
    --gcSetValues gc (GCValues {foreground = fg})
    pixmap <- readMVar pixmapVar
    drawDrawable drawWindow
                 gc
                 pixmap
                 x y
                 x y
                 width height

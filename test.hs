import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Events
import Control.Concurrent.MVar

main :: IO ()
main = do
    initGUI
    win <- windowNew
    windowSetTitle win "My Title"
    win `onDestroy` mainQuit
 
    canvas <- drawingAreaNew
    canvas `onSizeRequest` return (Requisition 500 500)
    canvas `onExpose` drawCanvas canvas
 
    containerAdd win canvas
    widgetShowAll win
    mainGUI

drawCanvas :: DrawingArea -> event -> IO Bool
drawCanvas canvas _evt = do
    dw <- widgetGetDrawWindow canvas
    drawWindowClear dw
    gc <- gcNew dw
    drawLine dw gc (10,10) (100,100)
    return True -- everything is OK

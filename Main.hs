module Main where

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo
import Control.Concurrent
import Control.Monad.Trans
import Data.Time
import Data.IORef

import TV
import DrawProgrammes
-----------------------
chans = ["bbc1", "bbc3", "bbc2", "dave"]
-----------------------

main = doGUI 
  
doGUI = do
  initGUI

  Just xml <- xmlNew "TV-epg.glade"
  splash <- xmlGetWidget xml castToWindow "splash"

  widgetShowAll splash
  timeoutAddFull (yield >> return True)
                 priorityDefaultIdle 50
  forkIO (loadDataAndShowMainWindow splash xml chans)
  mainGUI

loadDataAndShowMainWindow splash xml channels = do
  window <- xmlGetWidget xml castToWindow "mainwindow"
  drawingarea <- xmlGetWidget xml castToDrawingArea "drawingarea1"
  onDestroy window mainQuit

  (channels, programmes') <- doXMLTV $ blebXMLTV channels

  now <- fmap zonedTimeToLocalTime getZonedTime
  programmes <- return $ filter (\p -> (stop p) > now) programmes'

  totalTime <- return $ diffUTCTime (utcStop $ maximum programmes)
                                    (utcStart $ minimum programmes)
  
  widgetHide splash
  widgetShowAll window --must show before making a drawable
              
  dw <- widgetGetDrawWindow drawingarea
  pixmap <- pixmapNew (Just dw)
                      (floor channelWidth * (length channels))
                      (floor totalTime) 
                      Nothing
  pixref <- newIORef pixmap
  drawProgrammes pixmap channels programmes

  drawingarea `on` sizeRequest $ return (Requisition (floor (channelWidth * fromIntegral (length channels))) 1000)
  drawingarea `on` exposeEvent $ updateCanvas pixref
  return ()

updateCanvas :: IORef Pixmap -> EventM EExpose Bool
updateCanvas pixref = do
  win <- eventWindow
  liftIO $ do print "updating..."
              drawWindowClear win
              gc <- gcNew win
              pixmap <- readIORef pixref
              (w,h) <- drawableGetSize pixmap
              print w
              print h
              drawDrawable win gc pixmap 0 0 0 0 (-1) (-1)
  return True


module Main where

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo
import Control.Concurrent

import Data.Time


import TV
-----------------------
programmeToPangoMarkup :: TVProgramme -> Markup
programmeToPangoMarkup p = "<b>" ++ (title p) ++ "</b>\n"
--                           ++ "<i>" ++ show (subtitle p) ++ "</i>\n"
                           ++ "(" ++ show (start p) ++ " - "
                           ++ show(programmeLengthMinutes p)
                           ++ "m)\n"
                           ++ "<u>" ++ show (channel p) ++ "</u>\n"
                           ++ (description p)


-----------------------

main = do 

  channels <- return ["bbc1", "bbc3"]

  initGUI
  Just xml <- xmlNew "TV-epg.glade"
  splash <- xmlGetWidget xml castToWindow "splash"

  widgetShowAll splash

  timeoutAddFull (yield >> return True)
                 priorityDefaultIdle 50

  forkIO (loadDataAndShowMainWindow splash xml channels)
                                    
  mainGUI

loremIpsum = "Lorem <b>ipsum</b> \n\ndolor <i>sit</i> amet, consectetur adipisicing elit,\
        \ sed do eiusmod tempor incididunt ut labore et dolore magna\
        \ aliqua. Ut enim ad minim ve\nniam, quis nostrud exercitation\
        \ ullamco laboris nisi ut aliquip ex ea commodo consequat.\
        \ Duis aute irure dolor in reprehenderit in voluptate\
        \ velit esse cillum dolore eu fugiat nulla pariatur.\
        \ Excepteur sint occaecat cupidatat non proident, sunt in culpa\
        \ qui officia deserunt mollit anim id est laborum."


loadDataAndShowMainWindow splash xml channels = do
  window <- xmlGetWidget xml castToWindow "mainwindow"
  onDestroy window mainQuit

  (channels, programmes) <- doXMLTV $ blebXMLTV channels

  widgetHide splash
  widgetShowAll window

  ctx <- cairoCreateContext Nothing
  lay <- layoutEmpty ctx 
  layoutSetMarkup lay loremIpsum

  drawing <- xmlGetWidget xml castToDrawingArea "drawingarea1"
  drawing `on` sizeRequest $ return (Requisition 40 40)
  drawing `on` exposeEvent $ updateCanvas lay programmes
  return ()

fromJust (Just a) = a

updateCanvas :: PangoLayout -> [TVProgramme] -> EventM EExpose Bool
updateCanvas text programmes = do
  win <- eventWindow
  liftIO $ do
  (width',height') <- drawableGetSize win
  let width  = realToFrac width'
      height = realToFrac height'
  
--  layoutSetWidth text (Just width)
  -- Draw using the cairo api
  programmeInfoBubble win 10 10 100 (minimum programmes)
  return True

heightPerMinute = 3 :: Double

programmeInfoBubble :: (DrawableClass d) => d -> Double -> Double -> Double -> TVProgramme -> IO ()
programmeInfoBubble win x y w p = do
  radius <- return 10
  markup <- return $ programmeToPangoMarkup p
  len <- return $ fromIntegral $ programmeLengthMinutes p
  h <- return $ (len * heightPerMinute)
  ctx <- cairoCreateContext Nothing
  lay <- layoutEmpty ctx
  layoutSetMarkup lay markup
  layoutSetWidth lay (Just (w - 2*radius))
  renderWithDrawable win $ do
    gradientRoundedRect x y w h radius  0 0 0 1  0 0 1 1  0 0 0.6 1
    moveTo (x+radius) (y+radius)
    showLayout lay


gradientRoundedRect x y w h ra sr sg sb sa r g b a r' g' b' a' = do
  save
  setSourceRGBA sr sg sb sa
  withLinearPattern ((x+w)/2) y ((x+w)/2) (y+h) $ \pattern -> do
    patternAddColorStopRGBA pattern 0 r g b a
    patternAddColorStopRGBA pattern 1 r' g' b' a'
    roundedRect x y w h ra pattern
  restore
    
roundedRect x y w h r fillPattern = do
  moveTo (x+r) y
  lineTo (x+w-r) y
  arc (x+w-r) (y+r) r (-pi/2) 0
  lineTo (x+w) (y+h-r)
  arc (x+w-r) (y+h-r) r 0 (pi/2)
  lineTo (x+r) (y+h)
  arc (x+r) (y+h-r) r (pi/2) (-pi)
  lineTo x (y+r)
  arc (x+r) (y+r) r (-pi) (-pi/2)
  save
  setSource fillPattern
  fillPreserve
  restore
  stroke
  return ()
  
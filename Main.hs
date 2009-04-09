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
programmeToPangoMarkup p = "<b>" ++ (em $ (title p)) ++ "</b>\n"
--                           ++ "<i>" ++ em $ show (subtitle p) ++ "</i>\n"
                           ++ "(" ++ (em $ show (start p)) ++ " - "
                           ++ (em $ show(programmeLengthMinutes p))
                           ++ "m)\n"
                           ++ "<u>" ++ (em $ show (channel p)) ++ "</u>\n"
                           ++ (em $ (description p))
                              where
                                em = escapeMarkup


timeBetweenProgrammesMinutes p p' = floor $ (/ 60) $ (diffUTCTime 
                           (utcStart p)
                           (utcStart p'))
                           


-----------------------

main = do 

  channels <- return ["bbc1", "bbc3", "bbc2", "dave"]

  initGUI
  Just xml <- xmlNew "TV-epg.glade"
  splash <- xmlGetWidget xml castToWindow "splash"

  widgetShowAll splash

  timeoutAddFull (yield >> return True)
                 priorityDefaultIdle 50

  forkIO (loadDataAndShowMainWindow splash xml channels)
                                    
  mainGUI


heightPerMinute = 3 :: Double
channelWidth = 200 :: Double
padding = 3 :: Double

loadDataAndShowMainWindow splash xml channels = do
  print "loading data and showing main window"
  window <- xmlGetWidget xml castToWindow "mainwindow"
  onDestroy window mainQuit

  hscale <- xmlGetWidget xml castToHScale "timescale"

  (channels, programmes') <- doXMLTV $ blebXMLTV channels

  now' <- getZonedTime
  now <- return $ zonedTimeToLocalTime now'
  programmes <- return $ filter (\p -> (stop p) > now) programmes'

  afterRangeChangeValue hscale $ scaleValueChanged xml channels programmes

  earliest <- return $ utcStart $ minimum programmes
  latest <- return $ utcStop $ maximum programmes

  range <- return $ realToFrac $ diffUTCTime latest earliest
  print "Range is"
  print range

  rangeSetRange hscale 0 range

--  hscale `on` afterRangeChangeValue $ scaleValueChanged xml channels programmes

  widgetHide splash
  widgetShowAll window

  drawing <- xmlGetWidget xml castToDrawingArea "drawingarea1"
  drawing `on` sizeRequest $ return (Requisition (floor (channelWidth * fromIntegral (length channels))) 100)
  drawing `on` exposeEvent $ updateCanvas xml channels programmes
  return ()

fromJust (Just a) = a


scaleValueChanged xml channels programmes scroll value  = do
  redraw xml channels programmes
  return True

updateCanvas :: GladeXML -> [Channel] -> [TVProgramme] -> EventM EExpose Bool
updateCanvas xml cs ps = do
  liftIO (redraw xml cs ps)
  return True

redraw :: GladeXML -> [Channel] -> [TVProgramme] -> IO ()
redraw xml channels programmes = do
  drawarea <- xmlGetWidget xml castToDrawingArea "drawingarea1"
  win <- widgetGetDrawWindow drawarea
  liftIO $ do
  (width',height') <- drawableGetSize win
  let width  = realToFrac width'
      height = realToFrac height'

  hscale <- xmlGetWidget xml castToHScale "timescale"
  val <- rangeGetValue hscale
  cutoff <- return $ addUTCTime (realToFrac val) (utcStart (minimum programmes))

  

  doChannels win channels [p|p<-programmes, (utcStart p) > cutoff]
--  programmeInfoBubble win 10 10 100 (minimum programmes)


doChannels :: (DrawableClass d) => d -> [Channel] -> [TVProgramme] -> IO ()
doChannels win cs programmes = doChannels' win cs programmes (minimum programmes) 0

doChannels' :: (DrawableClass d) => d -> [Channel] -> [TVProgramme] -> TVProgramme -> Double -> IO ()
doChannels' _ [] _ _ _ = return ()
doChannels' win (c:cs) programmes earliest n = do
  putStrLn ("doing channel: " ++ chanId c)
  doProgrammes win ps earliest n
  doChannels' win cs programmes earliest (n+1)
    where
      ps = filter (\p -> (channel p) == (chanId c)) programmes
                
doProgrammes :: DrawableClass d => d -> [TVProgramme] -> TVProgramme -> Double -> IO ()
doProgrammes _ [] _ _ = return ()
doProgrammes win (p:ps) earliest col = do
  x <- return $ col * channelWidth
  y <- return $ (fromIntegral $ timeBetweenProgrammesMinutes p earliest) * heightPerMinute
  programmeInfoBubble win x y channelWidth p
  doProgrammes win ps earliest col

programmeInfoBubble :: (DrawableClass d) => d -> Double -> Double -> Double -> TVProgramme -> IO ()
programmeInfoBubble win x y w p = do
  radius <- return 5
  markup <- return $ programmeToPangoMarkup p
  len <- return $ fromIntegral $ programmeLengthMinutes p
  h <- return $ (len * heightPerMinute)
  ctx <- cairoCreateContext Nothing
  lay <- layoutEmpty ctx
  layoutSetMarkup lay markup
  layoutSetWidth lay (Just (w - 2*radius))
  renderWithDrawable win $ do
    save
    gradientRoundedRect (x+padding) (y+padding) (w-padding) (h-padding) 
                        radius  0 0 0 1  0 0.6 0.8 1  0 0.2 0.8 1
    moveTo (x+radius) (y+radius)
    showLayout lay
    restore


gradientRoundedRect x y w h ra sr sg sb sa r g b a r' g' b' a' = do
  setSourceRGBA sr sg sb sa
  withLinearPattern ((x+w)/2) y ((x+w)/2) (y+h) $ \pattern -> do
    patternAddColorStopRGBA pattern 0 r g b a
    patternAddColorStopRGBA pattern 1 r' g' b' a'
    roundedRect x y w h ra pattern

--draws a roundedRect and sets the clipping region to the rectangle
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
  strokePreserve
  clip
  return ()
  
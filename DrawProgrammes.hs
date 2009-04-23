module DrawProgrammes
    (
     programmeInfoBubble,
     showNextTwoHours,
     nextTwoHoursSizeRequest,
     padding
    )
where

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Data.Time
import Data.IORef
import Control.Monad

import TV
import GlobalState

padding = 10 :: Double

programmeToPangoMarkup cs p = titleToMarkup p ++ "\n"
                              ++ timingAndChannelToMarkup p ++ "\n\n"
                              ++ subtitleToMarkup p
                              ++ descriptionToMarkup p
    where
      titleToMarkup p = markSpan [FontSize SizeGiant] $ em $ title p
      timingAndChannelToMarkup p = markSpan [FontWeight WeightBold,
                                             FontSize SizeLarge]
                                     (chan p ++ ": " 
                                      ++ tim p ++ " " ++ len p ++ "m")
          where
            chan p = em $ channelName cs p
            tim p = em $ niceTime (start p)
            len p = em $ show $ programmeLengthMinutes p
      subtitleToMarkup p = sttm (subtitle p)
          where
            sttm (Just st) = "<i>" ++ st ++ "</i>" ++ "\n"
            sttm Nothing = ""
      descriptionToMarkup p = em $ description p
      em = escapeMarkup


twoHoursHeight = 30 :: Double
twoHoursPadding = 3 :: Double

nextTwoHoursSizeRequest ref = do
  cs <- liftM stChans $ readIORef ref
  let height = twoHoursHeight * fromIntegral (length cs)
  return $ Requisition 0 (round height)

showNextTwoHours :: (WidgetClass d) => d -> [Channel] -> [TVProgramme] -> IO()
showNextTwoHours widget cs ps = do
  localTime <- liftM zonedTimeToLocalTime getZonedTime
  timezone <- getCurrentTimeZone
  let currentTime = localTimeToUTC utc localTime
      twoHoursTime = addUTCTime 7200 currentTime
      ps' = filter (\p -> (utcStop p > currentTime)
                         && (utcStart p < twoHoursTime)) ps
  
  showChannels widget cs ps' 100 0 currentTime

showChannels _ [] _  _ _ _ = return ()
showChannels widget (c:cs) ps' xoffset yoffset currentTime = do
  showChannel widget c ps' xoffset yoffset currentTime
  showChannels widget cs ps' xoffset (yoffset+twoHoursHeight) currentTime

showChannel widget c ps xoffset yoffset currentTime = do
  style <- widgetGetStyle widget

  top <- styleGetDark style StateSelected
  bottom <- styleGetLight style StateSelected
  source <- styleGetText style StateSelected
 
  gradientRoundedRectWithMarkup widget 
                                    0 yoffset xoffset twoHoursHeight 
                                    0 twoHoursPadding 
                                    source top bottom 
                                    (chanName c)

  mapM (showProgramme widget xoffset yoffset currentTime) channelProgrammes
    where
      channelProgrammes = filter (\p -> (channel p) == (chanId c)) ps

showProgramme widget xoffset yoffset currentTime p = do
  win <- widgetGetDrawWindow widget
  (w,_) <- drawableGetSize win
  let totalWidth = (fromIntegral w) - xoffset
      widthPerMinute = totalWidth/120 --  width divided by two hours
      width = widthPerMinute * fromIntegral (programmeLengthMinutes p)
      timeOffset = fromRational $ toRational (diffUTCTime (utcStart p) currentTime) / 60
      leftOffset = timeOffset * widthPerMinute + xoffset


  if leftOffset >= xoffset
     then programmeTitleBubble widget p leftOffset yoffset width twoHoursHeight
     else do
       let width' = width + (leftOffset - xoffset)
           leftOffset' = xoffset
       programmeTitleBubble widget p leftOffset' yoffset width' twoHoursHeight

programmeTitleBubble :: (WidgetClass d) => d -> TVProgramme -> Double -> Double -> Double -> Double -> IO ()
programmeTitleBubble widget p x y w h = do
  win <- widgetGetDrawWindow widget
  style <- widgetGetStyle widget
  let radius = 5
  let markup = "<span size=\"x-small\"><b>" ++ (escapeMarkup $ title p) ++ "</b>: " ++ niceHour (start p) ++ "</span>"

  top <- styleGetDark style StateSelected
  bottom <- styleGetLight style StateSelected
  source <- styleGetText style StateSelected

  gradientRoundedRectWithMarkup widget x y w h radius twoHoursPadding source top bottom markup

programmeInfoBubble :: (WidgetClass d) => d -> [Channel] -> TVProgramme -> IO ()
programmeInfoBubble widget cs p = do
  win <- widgetGetDrawWindow widget
  style <- widgetGetStyle widget
  radius <- return 10
  markup <- return $ programmeToPangoMarkup cs p

  (w', h') <- drawableGetSize win
  w <- (return $ fromIntegral w') :: IO Double
  h <- (return $ fromIntegral h') :: IO Double
  (x, y) <- return (0,0)

  top <- styleGetDark style StateSelected
  bottom <- styleGetLight style StateSelected
  source <- styleGetText style StateSelected --todo: respond to hover?

  gradientRoundedRectWithMarkup widget x y w h radius padding source top bottom markup


gradientRoundedRectWithMarkup widget x y w h radius padding source top bottom markup = do
  ctx <- widgetCreatePangoContext widget
  drawable <- widgetGetDrawWindow widget
  lay <- layoutEmpty ctx
  layoutSetMarkup lay markup
  layoutSetWidth lay (Just (w - 2*radius - 2*padding))
  renderWithDrawable drawable $ do
    save
    gradientRoundedRect (x+padding) (y+padding) (w-2*padding) (h-2*padding) 
                        radius source top bottom
    moveTo (x+padding+radius) (y+padding+radius)
    showLayout lay
    restore


gradientRoundedRect x y w h ra source top bottom = do
  applyColorRGB setSourceRGB source
  withLinearPattern ((x+w)/2) y ((x+w)/2) (y+h) $ \pattern -> do
    applyColorRGB (patternAddColorStopRGB pattern 0) top
    applyColorRGB (patternAddColorStopRGB pattern 1) bottom
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
  

applyColorRGB f c = f (red c) (green c) (blue c)

red :: Color -> Double
red (Color r _ _ ) = (fromIntegral r) / 65535

green :: Color -> Double
green (Color _ g _ ) = (fromIntegral g) / 65535

blue :: Color -> Double
blue (Color _ _ b ) = (fromIntegral b) / 65535
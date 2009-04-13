module DrawProgrammes
    (
     programmeInfoBubble,
     padding
    )
where

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk

import TV

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

--  ctx <- cairoCreateContext Nothing
  ctx <- widgetCreatePangoContext widget
  lay <- layoutEmpty ctx
  layoutSetMarkup lay markup
  layoutSetWidth lay (Just (w - 2*radius - 2*padding))
  renderWithDrawable win $ do
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
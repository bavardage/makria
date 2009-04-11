{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module TV 
    (Channel(..),
     TVProgramme(..),
     doXMLTV,
     blebXMLTV,
     programmeLength,
     programmeLengthMinutes,
     timeBetweenProgrammesMinutes,
     utcStart, utcStop,
     channelName,
     ) where

import Network.HTTP
import Network.URI
import Text.XML.HXT.Arrow
import Data.Tree.NTree.TypeDefs hiding (getChildren)
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy.Char8 as B 
import Data.Time
import Data.List



type Title = String
type Description = String
type ChannelID = String

data Channel = Channel {chanId :: ChannelID,
                        chanName :: String
                       } deriving Show

data TVProgramme = TVProgramme { start :: LocalTime,
                             stop :: LocalTime,
                             channel :: ChannelID,
                             title :: Title,
                             subtitle :: Maybe Title,
                             description :: Description
                           }

instance Show TVProgramme where
    show programme = ",,,,,,,,,,,,,,,,,,,,,,,,,,," ++ "\n" ++ 
                     title programme ++ " (" ++  (show $ start programme) ++ ")\n"
                     ++ show (subtitle programme) ++ " " ++ channel programme ++ "\n"
                     ++ "_______________" ++ "\n"
                            ++ description programme ++ "\n\n"

instance Eq TVProgramme where
    (==) p q = (start p) == (start q)

instance Ord TVProgramme where
    compare p q = compare (start p) (start q)

utcStart p = localTimeToUTC utc $ start p
utcStop p = localTimeToUTC utc $ stop p

programmeLength :: TVProgramme -> NominalDiffTime
programmeLength p = diffUTCTime (utcStop p) (utcStart p)

programmeLengthMinutes = floor .  (/ 60) . programmeLength

timeBetweenProgrammesMinutes p p' = floor $ (/ 60) $ (diffUTCTime 
                           (utcStart p)
                           (utcStart p'))
channelName :: [Channel] -> TVProgramme -> String
channelName [] _ = "unknown"
channelName (c:cs) p | chanId c == channel p = chanName c
                     | otherwise = channelName cs p

{--------
 Get the url of the feed for given channels using the bleb service
-}-------

blebXMLTV :: [String] -> String
blebXMLTV channels = intercalate ""
                    ["http://bleb.org/tv/data/listings?days=",
                     "0..6",
                     "&format=XMLTV&channels=",
                     channels',
                     "&file=gzip"
                     ]
    where
      channels' = intercalate "," channels

{--------
 Get and process xmltv data
-}-------

type Request_BS = Request B.ByteString

retrieveListingData :: String -> IO B.ByteString
retrieveListingData url = do
  case parseURI url of
    Nothing -> ioError . userError $ "Invalid URL"
    Just uri -> get uri

get uri = do
  req <- return (defaultGETRequest_ uri :: Request_BS)
  eresp <- simpleHTTP req
  case eresp of
    Left _ -> ioError . userError $ "Failed to get " ++ show uri
    Right res -> return $ rspBody res

doXMLTV url = do
  gzipped <- retrieveListingData url
  ungzipped <- return $ decompress gzipped
  undoctyped <- return $ dedoctype (B.unpack ungzipped)
  xml <- return $ readString [(a_validate, "0")] undoctyped
  channels <- runX (xml >>> processChannels)
  programmes <- runX (xml >>> processProgrammes)
  return (channels, programmes)

dedoctype text = unlines $ dedoctype' $ lines text
                 where
                   dedoctype' (x:y:xs) = x:xs

getField field = getChildren >>> hasName field >>> text 
maybeGetField field = withDefault (getField field >>> arr Just) Nothing
text = getChildren >>> getText

processChannels =
    isElem >>>
    getChildren >>> --tv
    getChildren >>> --channel
    hasName "channel" >>>
    proc x -> do
      id' <- getAttrValue "id" -< x
      name <- getField "display-name" -< x
      returnA -< Channel {chanId=id',
                          chanName=name
                         }

processProgrammes = 
    isElem >>>
    getChildren >>>
    getChildren >>>
    hasName "programme" >>>
    proc x -> do
      start'' <- getAttrValue "start" -< x
      stop'' <- getAttrValue "stop" -< x
      start' <- returnA -< toTime start''
      stop' <- returnA -< toTime stop''
      channel' <- getAttrValue "channel" -< x
      title' <- getField "title" -< x
      desc <- getField "desc" -< x
      subtitle' <- maybeGetField "sub-title" -< x
      returnA -< TVProgramme {start=start',
                              stop=stop',
                              channel=channel',
                              title=title',
                              subtitle=subtitle',
                              description=desc
                              }

toTime :: String -> LocalTime
toTime t = LocalTime day tod
    where
      day = fromGregorian (read (time !! 0)) (read (time !! 1)) (read (time !! 2))
      tod = TimeOfDay {todHour = read (time !! 3),
                       todMin = read (time !! 4),
                       todSec = (fromIntegral $ read (time !! 5)) 
                      }
      time = splitTime t [4,2,2,2,2,2]
      splitTime t [] = [t]
      splitTime t (x:xs) = fst y : splitTime (snd y) xs where y = splitAt x t

import Network.HTTP
import Network.URI
import Text.XML.HXT.Arrow
import Data.Tree.NTree.TypeDefs hiding (getChildren)
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy.Char8 as B 
--import qualified Data.ByteString.Lazy as B


import Data.List

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

type Title = String
type Description = String
type Channel = String
data TVProgramme = TVProgramme { start :: String,
                             stop :: String,
                             channel :: Channel,
                             title :: Title,
                             subtitle :: Maybe Title,
                             description :: Description
                           }

instance Show TVProgramme where
    show programme = title programme ++ "(" ++  start programme ++ ")\n"
                     ++ show (subtitle programme) ++ channel programme ++ "\n"
                     ++ "------------" ++ "\n"
                            ++ description programme

xmltvURL :: [String] -> String
xmltvURL channels = intercalate ""
                    ["http://bleb.org/tv/data/listings?days=",
                     "0..6",
                     "&format=XMLTV&channels=",
                     channels',
                     "&file=gzip"
                     ]
    where
      channels' = intercalate "," channels

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

doXMLTV channels = do
  gzipped <- retrieveListingData $ xmltvURL channels
  ungzipped <- return $ decompress gzipped
  undoctyped <- return $ dedoctype (B.unpack ungzipped)
  xml <- return $ readString [(a_validate, "0")] undoctyped
  channels <- runX (xml >>> processChannels)
  programmes <- runX (xml >>> processProgrammes)
  print programmes

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
    getChildren >>> --display-name, icon
    hasName "display-name" >>>
    getChildren >>>
    getText

processProgrammes = 
    isElem >>>
    getChildren >>>
    getChildren >>>
    hasName "programme" >>>
    proc x -> do
      start' <- getAttrValue "start" -< x
      stop' <- getAttrValue "stop" -< x
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
    


-- retrieveListingData :: String -> IO String
-- retrieveListingData url = do
--   case parseURI url of
--     Nothing  -> ioError . userError $ "Invalid URL"
--     Just uri -> get uri

-- get uri = do
--   eresp <- simpleHTTP (Request uri GET [] "") 
--   case eresp of
--     Left _    -> ioError . userError $ "Failed to get " ++ show uri
--     Right res -> return $ rspBody res 

-- type Channel = String --change this to include URI and such
-- data TVProgram = TVProgram { time :: Int,
--                              channel :: Channel,
--                              title :: String,
--                              description :: String
--                            } deriving Show

-- doXMLTV = do
--   gzipped <- retrieveListingData xmltvurl
--   asbytes <- return (pack gzipped)
--   ungzipped <- return (decompress asbytes)
--   return $ readString [(a_validate,"0")] (unpack ungzipped)


-- doChannel channel day = do
--   rss <- retrieveListingData (getRssURL channel day)
--   xml <- return $ readString [(a_validate,"0")] rss
--   results <- runX (xml >>> processChannel channel)
--   print results

-- getField field = getChildren >>> hasName field >>> text
-- text = getChildren >>> getText

-- processChannel channel = 
--      isElem >>>
--      getChildren >>>
--      getChildren >>>
--      hasName "channel" >>>
--      getChildren >>>
--      hasName "item" >>>
--      proc x -> do 
--        description' <- getField "description" -< x
--        timetitle' <- getField "title" -< x
--        (time'',title'') <- returnA -< break (==':') timetitle'
--        time' <- returnA -< init time''
--        title' <- returnA -< tail $ tail title''
--        returnA -< TVProgram { time = read time',
--                               channel = channel,
--                               title = title',
--                               description = description'
--                             }
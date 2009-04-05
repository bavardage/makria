import Network.HTTP
import Network.URI
import Text.XML.HXT.Arrow
import Data.Tree.NTree.TypeDefs hiding (getChildren)

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

xmltvurl = "http://bleb.org/tv/data/listings?days=0..6&format=XMLTV&channels=bbc1,dave&file=gzip"

getRssURL :: String -> Int -> String
getRssURL channel day = "http://bleb.org/tv/data/rss.php?ch=" ++ channel ++ "&day=" ++ show day

bbc1today = getRssURL "bbc1" 0

retrieveListingData url = do
  case parseURI url of
    Nothing  -> ioError . userError $ "Invalid URL"
    Just uri -> get uri

get uri = do
  eresp <- simpleHTTP (Request uri GET [] "")
  case eresp of
    Left _    -> ioError . userError $ "Failed to get " ++ show uri
    Right res -> return $ rspBody res 

type Channel = String --change this to include URI and such
data TVProgram = TVProgram { time :: Int,
                             channel :: Channel,
                             title :: String,
                             description :: String
                           } deriving Show

doChannel channel day = do
  rss <- retrieveListingData (getRssURL channel day)
  xml <- return $ readString [(a_validate,"0")] rss
  results <- runX (xml >>> processChannel channel)
  print results

getField field = getChildren >>> hasName field >>> text
text = getChildren >>> getText

processChannel channel = 
     isElem >>>
     getChildren >>>
     getChildren >>>
     hasName "channel" >>>
     getChildren >>>
     hasName "item" >>>
     proc x -> do 
       description' <- getField "description" -< x
       timetitle' <- getField "title" -< x
       (time'',title'') <- returnA -< break (==':') timetitle'
       time' <- returnA -< init time''
       title' <- returnA -< tail $ tail title''
       returnA -< TVProgram { time = read time',
                              channel = channel,
                              title = title',
                              description = description'
                            }
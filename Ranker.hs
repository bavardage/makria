
module Ranker
    (Ranker,
     keywords,
     normalised,
     rankMap,
     cachedRank,
     rank,
     writeOut,
     readIn,
     newKeywordRanker,
    )
where

import Prelude hiding (lookup)
import Normaliser
import TV
import qualified Data.Map as Map
import Data.IORef

class Ranker a where
    cachedRank :: a -> TVProgramme -> IO Float
    cachedRank kr p = return $ rank kr p
    rank :: a -> TVProgramme -> Float
    writeOut :: a -> String -> IO ()
    readIn :: String -> IO (a)


{- Keyword Ranker --------------------------------------}
type WeightedKeyword = (String, Float)

data KeywordRanker = KeywordRanker {keywords :: [WeightedKeyword],
                                    normalised :: [WeightedKeyword],
                                    rankMap :: IORef (Map.Map TVProgramme Float)} 
                     

newKeywordRanker :: [WeightedKeyword] -> IO KeywordRanker 
newKeywordRanker kws = do
  ref <- newIORef Map.empty
  return KeywordRanker {keywords = kws,
                        normalised = (map (\(kw,r) -> (normaliseWord kw,r)) kws),
                        rankMap = ref}


instance Ranker KeywordRanker where
    writeOut kr filename = writeFile filename $ show $ keywords kr

    readIn f = readFile f >>= \x -> newKeywordRanker (read x)

    rank kr p = sum $ map (rankKR $ normalised kr) [description p,
                                                  title p]

    cachedRank kr p = do
      rankmap <- readIORef (rankMap kr)
      result <- return $ (Map.lookup p) rankmap
      case result of
        Just r -> return r
        Nothing -> do
          r <- return $ rank kr p
          writeIORef (rankMap kr) (Map.insert p r rankmap)
          return r

rankKR kws str= rankKR' kws (normaliseText str)
    where
      rankKR' kws words = sum $ map (valueOfWord kws) words

valueOfWord [] _ = 0
valueOfWord ((kw,r):kws) w | kw == w = r
                           | otherwise = valueOfWord kws w
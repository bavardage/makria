
module Ranker
where

import Normaliser
import TV

class Ranker a where
    rank :: a -> TVProgramme -> Float
    writeOut :: a -> String -> IO ()
    readIn :: String -> IO (a)


{- Keyword Ranker --------------------------------------}
type WeightedKeyword = (String, Float)

data KeywordRanker = KeywordRanker {keywords :: [WeightedKeyword]} 
                     deriving Show

instance Ranker KeywordRanker where
    writeOut (KeywordRanker(kws)) filename = writeFile filename (show kws)
    readIn f = readFile f >>= \x -> return (KeywordRanker(read x))
    rank (KeywordRanker(kws)) p = sum $ map (rankKR kws) [description p,
                                                           title p]

rankKR kws str= rankKR' (map (\(kw,r) -> (normaliseWord kw,r)) kws) (normaliseText str)
    where
      rankKR' kws words = sum $ map (valueOfWord kws) words

valueOfWord [] _ = 0
valueOfWord ((kw,r):kws) w | kw == w = r
                           | otherwise = valueOfWord kws w
module Normaliser

where

import Data.Char
import Stemmer

commonWords = ["the", "and", "of", "a", "in"]

normaliseWord = stem.(filter isAsciiLower).(map toLower)

normaliseText = (map normaliseWord)
                .(filter (not.(`elem` commonWords)))
                .words
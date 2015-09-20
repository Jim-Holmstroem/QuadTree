{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module Cities
( getCities
, City(..)
, position
) where

import Data.List
import qualified Data.Set as S
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


data City = City !Text.Text !Double !Double deriving (Show)


position (City _ longitude latitude) = (longitude, latitude)


readDouble :: Text.Text -> Double
readDouble raw = read $ Text.unpack $ case Text.splitOn "." raw of
    [integer] -> integer
    [integer, fraction] -> flip Text.append (Text.cons '.' fraction) $ case Text.uncons integer of
        Just ('-', "") -> "-0"
        Just ('-', xs) -> Text.append "-" xs
        Just (  x, xs) -> Text.append (Text.singleton x) xs
        Nothing -> "0"


city :: Text.Text -> City
city line = City city (readDouble latitude) (readDouble longitude)
    where [_, city, _, _, _, latitude, longitude] = Text.splitOn "," line


nubOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOn f = nubOn' S.empty
    where nubOn' _ [] = []
          nubOn' s (x:xs) | S.member (f x) s = nubOn' s xs
                          | otherwise        = x : nubOn' (S.insert (f x) s) xs


getCities :: IO [City]
getCities = do
    raw <- Text.readFile "data/worldcitiespop.utf8.txt"
    return $ positionallyUnique $ (map city) $ body $ Text.lines $ raw
        where body = Prelude.tail
              positionallyUnique = nubOn position

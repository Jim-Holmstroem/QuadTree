{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

import System.Environment

import qualified Cities


(a, b) <+> (c, d) = (a + c, b + d)
(a, b) </> c      = (a / c, b / c)


sumPosition [(longitude, latitude)] = (longitude, latitude)
sumPosition ((longitude, latitude):xs) = (longitude, latitude) <+> sumPosition xs


meanPosition xs = (sumPosition xs) </> (fromIntegral $ length xs)


main :: IO ()
main = do
    (count_:_)  <- getArgs
    let count = read count_ :: Int

    cities <- Cities.getCities

    print $ head cities
    print $ meanPosition $ map Cities.position $ take count cities

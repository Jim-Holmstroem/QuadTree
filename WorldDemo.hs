{-# LANGUAGE OverloadedStrings #-}

import Control.Monad

import qualified Data.List
import qualified Data.Text as Text

import qualified Cities as C
import qualified QuadTree as QT


getWorldQuadTree :: IO (QT.QuadTree C.City)
getWorldQuadTree = do
    cities <- C.getCities
    let convert city@(C.City name latitude longitude) = (QT.Point latitude longitude, city)
    let convertedCities = map convert cities

    return $ QT.quadTree longitudeLatitudeRange convertedCities
        where longitudeLatitudeRange = QT.Domain (QT.Point (-90.1) (-180.1)) (QT.Point 90.1 180.1)


showBigNumber :: Int -> String
showBigNumber number = Data.List.intercalate "'" $ reverse $ chunks 3 $ reverse $ show number
    where chunks n xs = takeWhile (not.null) $ Data.List.unfoldr (Just . splitAt n) xs


getPosition :: IO (Double, Double)
getPosition = readLn


query :: (Show a) => QT.QuadTree a -> IO ()
query qt = do
    putStrLn "Get all locations 0.1 degrees and closer to position (Ex. \"(-10,10\") (no spaces))"
    (latitude, longitude) <- getPosition

    (mapM_ (print . snd)) $ QT.near 0.1 (QT.Point latitude longitude) qt


main :: IO ()
main = do
    putStrLn "Generating QuadTree (This can take up to a few minutes)"
    wqt <- getWorldQuadTree
    putStrLn $ "Generated QuadTree with " ++ showBigNumber (QT.count wqt) ++ " places."
    putStrLn "===================="
    putStrLn "Example Query 0.1 degrees close to Stockholm @(59.3294, 18.0686):"
    (mapM_ print) $ QT.near 0.1 (QT.Point 59.3294 18.0686) wqt

    forever $ query wqt

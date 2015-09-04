import qualified QuadTree as QT
import qualified Cities

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified System.Environment as Env


city2unitQuad :: Cities.City -> (QT.Point, Text.Text)
city2unitQuad (Cities.City name latitude longitude) = (QT.Point (latitude/latitudeRange) (longitude/longitudeRange), name)
    where latitudeRange = 90
          longitudeRange = 180


nth :: Int -> [a] -> [a]
nth n [] = []
nth n as = head as : nth n next
    where next = drop n as

main = do
    cities <- Cities.getCities
    (arg0:_) <- Env.getArgs
    let count = (read arg0) :: Int

    let selectedCities = map city2unitQuad cities
    print $ show $ QT.meanDepth $ QT.quadTree QT.unitDomain $ take count $ selectedCities
    --QT.renderUnitPNG ("output." ++ show count ++ ".png") $ QT.quadTree QT.unitDomain $ take count $ selectedCities

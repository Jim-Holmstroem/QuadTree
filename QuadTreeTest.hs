import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import qualified QuadTree as QT

newtype SUnitInterval = SUnit { getSUnit :: Double }
    deriving (Show)

-- The range is until I'm sure that there isn't any precision problems with large ranges (This needs to be checked as well ofc, but later)
instance Arbitrary SUnitInterval where
    arbitrary = fmap SUnit (choose (-1, 1))
    shrink (SUnit x) = [ SUnit y | y <- shrink x, -1 <= y && y <= 1 ]

instance Arbitrary QT.Point where
    --arbitrary = do
    --  SUnit x <- arbitrary
    --  SUnit y <- arbitrary
    --  return $ QT.Point x y

    arbitrary = QT.Point <$> position <*> position
        where position = getSUnit <$> arbitrary

instance Arbitrary QT.Domain where
    arbitrary = QT.Domain <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (QT.QuadTree a) where
    -- use the constructor quadTree to both make the arbitrary simpler and the generated QuadTree a is a valid one (whould be cool if it could generate without using the constructor implementation)
    arbitrary = QT.quadTree <$> arbitrary <*> arbitrary


insideSUnit :: QT.Point -> Bool
insideSUnit (QT.Point x y) = (-1 <= x && x <= 1) && (-1 <= y && y <= 1)

insideSDomain :: QT.Domain -> Bool
insideSDomain (QT.Domain a b) = (insideSUnit a) && (insideSUnit b)

import Data.List
import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import QuadTree


check :: (Testable prop) => prop -> IO ()
check = quickCheckWith stdArgs { maxSuccess = 8192 }


newtype UnitInterval = Unit Double
    deriving (Show)

instance Arbitrary UnitInterval where
    arbitrary = fmap Unit (choose (0, 1))
    shrink (Unit x) = [ Unit y | y <- shrink x, 0 <= y && y <= 1 ]

newtype SUnitInterval = SUnit { getSUnit :: Double }
    deriving (Show)

-- The range is until I'm sure that there isn't any precision problems with large ranges (This needs to be checked as well ofc, but later)
instance Arbitrary SUnitInterval where
    arbitrary = fmap SUnit (choose (-1, 1))
    shrink (SUnit x) = [ SUnit y | y <- shrink x, -1 <= y && y <= 1 ]

instance Arbitrary Point where
    --arbitrary = do
    --  SUnit x <- arbitrary
    --  SUnit y <- arbitrary
    --  return $ Point x y

    arbitrary = Point <$> position <*> position
        where position = getSUnit <$> arbitrary

instance Arbitrary Domain where
    arbitrary = Domain <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (QuadTree a) where
    -- use the constructor quadTree to both make the arbitrary simpler and the generated QuadTree a is a valid one (whould be cool if it could generate without using the constructor implementation)
    arbitrary = quadTree unitDomain <$> arbitrary
    -- TODO unitDomain should be arbitrary but needs to be larger then all possible arbitrary points (the last arbitrary)
    -- this works by only generating the points and fixating the domain


toList :: QuadTree a -> [(Point, a)]
toList (QuadTree _ ur ul ll lr) = concatMap toList [ur, ul, ll, lr]
toList (QuadLeaf _ p v) = [(p, v)]
toList (QuadEmpty _) = []


(~==) :: (Eq a) => [a] -> [a] -> Bool
x ~== y = null (x \\ y)


prop_pointInsideSUnit :: Point -> Bool
prop_pointInsideSUnit (Point x y) = (-1 <= x && x <= 1) && (-1 <= y && y <= 1)


prop_domainInsideSDomain :: Domain -> Bool
prop_domainInsideSDomain (Domain a b) = (prop_pointInsideSUnit a) && (prop_pointInsideSUnit b)


prop_countNonNegative :: QuadTree () -> Bool
prop_countNonNegative t = (count t >= 0)


prop_depthNonNegative :: QuadTree () -> Bool
prop_depthNonNegative t = (depth t >= 0)


prop_meanDepthNonNegative :: QuadTree () -> Bool
prop_meanDepthNonNegative t = (meanDepth t >= 0)


prop_nearPseudoIdempotent :: UnitInterval -> Point -> QuadTree Int -> Bool
prop_nearPseudoIdempotent (Unit qr) qp t = near qr qp t == (near qr qp $ quadTree (domain t) $ near qr qp t)


prop_nearQuadTreeIdempotent :: UnitInterval -> Point -> QuadTree Int -> Bool
prop_nearQuadTreeIdempotent (Unit qr) qp t = (nearQuadTree qr qp $ nearQuadTree qr qp t) == nearQuadTree qr qp t


prop_nearIsNear :: UnitInterval -> Point -> QuadTree Int -> Bool
prop_nearIsNear (Unit qr) qp@(Point qx qy) t = all (\(p@(Point x y), v)->(sqrt $ (qx-x)**2 + (qy-y)**2) <= qr) $ near qr qp t


-- FIXME breakout unitDomain to be arbitrary as well, however the query is only valid if all the points is inside of the domain  (needs to use bounding box as domain or something). Related to instance Arbitrary (QuadTree a).
prop_ifAndOnlyIfNear :: UnitInterval -> Point -> [(Point, Int)] -> Bool
prop_ifAndOnlyIfNear (Unit qr) qp@(Point qx qy) ps = (near qr qp $ quadTree unitDomain ps) ~== (filter (\(p@(Point x y), v)->(sqrt $ (qx-x)**2 + (qy-y)**2) <= qr) ps)

prop_nearQuadTreeAndNearEquivalence :: UnitInterval -> Point -> QuadTree Int -> Bool
prop_nearQuadTreeAndNearEquivalence (Unit qr) qp t = (near qr qp t) ~== (toList $ nearQuadTree qr qp t)


main = do
    check prop_pointInsideSUnit
    check prop_domainInsideSDomain
    check prop_countNonNegative
    check prop_depthNonNegative
    check prop_meanDepthNonNegative
    check prop_nearPseudoIdempotent
    check prop_nearQuadTreeIdempotent
    check prop_nearIsNear
    check prop_ifAndOnlyIfNear

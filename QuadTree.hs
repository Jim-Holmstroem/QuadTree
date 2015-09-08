module QuadTree
( Point(..)
, Domain(..)
, Renderable(..)
, QuadTree(..)
, quadTree
, domain
, nearQuadTree
, near
, unitDomain
, randomQuadTree
, randomPoints
, renderUnitPNG
, count
, depth
, meanDepth
, volume
) where

import Control.Monad (replicateM)
import Control.Applicative ((<$>), (<*>))
import System.Random

import qualified Graphics.Rendering.Cairo as C

-- Think about the outer interface of this library, should it be independent types like tuples or constructed by for example QuadLeaf
-- Is QuadTree.Point exceptible as outer interface?


-- *----B
-- |    |
-- |    |
-- |    |
-- A----*
--
-- A < B
--
-- a-d-g
-- |2|1|
-- b-e-h
-- |3|4|
-- c-f-i


data Point = Point Double Double
    deriving (Show, Eq)
data Domain = Domain Point Point
    deriving (Show, Eq)


data QuadTree v =
    QuadTree Domain (QuadTree v) (QuadTree v) (QuadTree v) (QuadTree v)
    | QuadLeaf Domain Point v
    | QuadEmpty Domain
    deriving (Show, Eq)


subdivide :: Domain -> (Domain, Domain, Domain, Domain)
subdivide (Domain (Point xA yA) (Point xB yB)) = (Domain e g, Domain b d, Domain c e, Domain f h)
    where [_, b, c, d, e, f, g, h, _] = Point <$> [xA, ((xA+xB)/2), xB] <*> [yB, ((yA+yB)/2), yA]


-- most be nonoverlapping range (i.e. cannot have wA <= w <= wB) because that could result in a point being
-- placed into multiple splits
inside1D :: Double -> Double -> Double -> Bool
inside1D w wA wB = wA <= w && w < wB


inside :: Point -> Domain -> Bool
(Point x y) `inside` (Domain (Point xA yA) (Point xB yB)) = (inside1D x xA xB) && (inside1D y yA yB)


distance :: Point -> Point -> Double
distance (Point x y) (Point x' y') = sqrt $ (x'-x)**2 + (y'-y)**2


distanceToDomain :: Point -> Domain -> Double
distanceToDomain p@(Point x y) domain@(Domain (Point xA yA) (Point xB yB))
    | p `inside` domain = 0.0
    | xB <= x && yB <= y = distance p (Point xB yB)  -- closest to (xB, yB)
    | xA >= x && yB <= y = distance p (Point xA yB)  -- closest to (xA, yB)
    | xA >= x && yA >= y = distance p (Point xA yA)  -- closest to (xA, yA)
    | xB <= x && yA >= y = distance p (Point xB yA)  -- closest to (xB, yA)
    | yB <= y = y - yB  -- closest to yB = y
    | xA >= x = xA - x  -- closest to xA = x
    | yA >= y = yA - y  -- closest to yA = y
    | xB <= x = x - xB  -- closest to xB = x
    | otherwise = error $ "This should not occure all possible case are already covered: d(" ++ show p ++ ", " ++ show domain ++ ")"


randomQuadTree :: Domain -> Int -> IO (QuadTree Point)
randomQuadTree domain count = do
    points <- randomPoints domain count
    return $ quadTree domain $ zip points points


-- The subquadtree consisting of the things with a domain within the radius r from p
nearQuadTree :: Double -> Point -> QuadTree a -> QuadTree a
nearQuadTree queryR queryPoint (QuadTree domain ur ul ll lr)
    | distanceToDomain queryPoint domain < queryR = QuadTree domain (nearQuadTree queryR queryPoint ur) (nearQuadTree queryR queryPoint ul) (nearQuadTree queryR queryPoint ll) (nearQuadTree queryR queryPoint lr)
    | otherwise = QuadEmpty domain
nearQuadTree queryR queryPoint leaf@(QuadLeaf domain point _)
    | distance queryPoint point < queryR = leaf
    | otherwise = QuadEmpty domain
nearQuadTree r p empty@(QuadEmpty domain) = empty


near :: Double -> Point -> QuadTree a -> [(Point, a)]
near queryR queryPoint (QuadTree domain ur ul ll lr)
    | distanceToDomain queryPoint domain < queryR = concatMap (near queryR queryPoint) [ur, ul, ll, lr]
    | otherwise = []
near queryR queryPoint (QuadLeaf domain point value)
    | distance queryPoint point < queryR = [(point, value)]
    | otherwise = []
near r p (QuadEmpty _) = []


quadTree :: Domain -> [(Point, a)] -> QuadTree a
quadTree domain pvs
    | all (\(p, _)->p `inside` domain) pvs = quadTree' domain pvs
    | otherwise = error $ "Trying to construct a quadTree with points outside of the domain \"" ++ show domain ++ "\": " ++ (show $ map fst $ filter (\(p, _)->not (p `inside` domain)) pvs)


--quadTree constructor without validation check
quadTree' :: Domain -> [(Point, a)] -> QuadTree a
quadTree' domain [] = QuadEmpty domain
quadTree' domain [(point, value)] = QuadLeaf domain point value
quadTree' domain pvs = QuadTree domain (quadTree' domain_ur pvs_ur) (quadTree' domain_ul pvs_ul) (quadTree' domain_ll pvs_ll) (quadTree' domain_lr pvs_lr)
    where (domain_ur, domain_ul, domain_ll, domain_lr) = subdivide domain
          [pvs_ur, pvs_ul, pvs_ll, pvs_lr] = map ($ pvs) $ map filterInside [domain_ur, domain_ul, domain_ll, domain_lr]
          filterInside d = filter ((`inside` d) . fst)


depth :: QuadTree a -> Int
depth (QuadEmpty _) = 0
depth (QuadLeaf _ _ _) = 1
depth (QuadTree _ ur ul ll lr) = (1+) $ foldl1 max $ map depth [ur, ul, ll, lr]


meanDepth :: QuadTree a -> Double
meanDepth (QuadEmpty _) = 0.0
meanDepth (QuadLeaf _ _ _) = 1.0
meanDepth (QuadTree _ ur ul ll lr) = (1.0+) $ mean $ map meanDepth $ nonEmpty [ur, ul, ll, lr]
    where mean [] = 0
          mean xs = (sum $ xs) / (fromIntegral $ length xs)
          nonEmpty [] = []
          nonEmpty ((QuadEmpty x):xs) = nonEmpty xs  -- hard to define meanDepth of QuadEmpty so that the mean gets unaffected by all 0 depths
          nonEmpty (x:xs) = x:nonEmpty xs


count :: QuadTree a -> Int
count (QuadEmpty _) = 0
count (QuadLeaf _ _ _) = 1
count (QuadTree _ ur ul ll lr) = foldl1 (+) $ map count [ur, ul, ll, lr]


volume :: QuadTree a -> Double
volume (QuadEmpty _) = 0.0
volume (QuadLeaf (Domain (Point xA yA) (Point xB yB)) _ _) = (abs (xB-xA))*(abs (yB-xA))
volume (QuadTree _ ur ul ll lr) = foldl1 (+) $ map volume [ur, ul, ll, lr]


class Renderable a where
    render :: a -> C.Render ()

instance Renderable Domain where
    render (Domain (Point xa ya) (Point xb yb)) = do
        C.save

        C.setSourceRGBA 0 0 0 0.8
        C.rectangle xa ya (xb-xa) (yb-ya)
        C.stroke

        C.restore

instance Renderable Point where
    render (Point x y) = do
        C.save
        width <- C.getLineWidth

        C.setSourceRGBA 0.5 0.5 0.5 0.5
        C.arc x y (5*width) 0 (2*pi)
        C.fill

        C.restore

instance Renderable (QuadTree a) where
    render (QuadTree domain@(Domain pa@(Point xa ya) pb@(Point xb yb)) ur ul ll lr) = do
        C.save
        width <- C.getLineWidth

        C.setSourceRGBA 0 0 0 1

        let c@(Point xc yc) = center domain
        C.moveTo xc yc
        C.lineTo xc ya
        C.stroke
        C.moveTo xc yc
        C.lineTo xc yb
        C.stroke
        C.moveTo xc yc
        C.lineTo xa yc
        C.stroke
        C.moveTo xc yc
        C.lineTo xb yc
        C.stroke

        render [ur, ul, ll, lr]

        C.restore

    render (QuadLeaf domain@(Domain (Point xa ya) (Point xb yb)) point _) = do
        C.save
        width <- C.getLineWidth

        C.setSourceRGBA 0 1 0 0.05
        C.rectangle xa ya (xb-xa) (yb-ya)
        C.fill

        render point

        C.restore

    render (QuadEmpty (Domain (Point xa ya) (Point xb yb))) = do
        C.save

        C.setSourceRGBA 1 0 0 0.05
        C.rectangle xa ya (xb-xa) (yb-ya)
        C.fill

        C.restore

instance (Renderable a) => Renderable ([a]) where
    render [] = return ()
    render (x:xs) = render x >> render xs


domain :: QuadTree a -> Domain
domain (QuadTree domain _ _ _ _) = domain
domain (QuadLeaf domain _ _) = domain
domain (QuadEmpty domain) = domain


center :: Domain -> Point
center (Domain (Point xa ya) (Point xb yb)) = Point ((xa+xb)/2) ((ya+yb)/2)


randomU01Sequence :: Int -> IO [Double]
randomU01Sequence count = replicateM count (randomIO :: IO Double)


randomPoints :: Domain -> Int -> IO [Point]
randomPoints (Domain (Point xa ya) (Point xb yb)) count = do
    xs <- randomU01Sequence count
    ys <- randomU01Sequence count
    return $ zipWith Point (fillW xa xb xs) (fillW ya yb ys)
        where fillW wa wb = map ((+wa).((wb-wa)*))


renderingWidth :: Int
renderingWidth = 2*2048
renderingHeight :: Int
renderingHeight = renderingWidth


testCircle :: C.Render ()
testCircle = do
    C.save

    C.setSourceRGBA 0 0 0 0.8
    C.arc 0 0 0.25 0 (2*pi)
    C.stroke
    C.arc 0 0 0.5 0 (2*pi)
    C.stroke
    C.arc 0 0 0.75 0 (2*pi)
    C.stroke
    C.arc 0 0 1 0 (2*pi)
    C.stroke

    C.restore


unitBlank :: C.Render ()
unitBlank = do
    C.save

    C.setSourceRGBA 1 1 1 1
    C.rectangle (-1) (-1) 2 2
    C.fill

    C.restore


unitRendering :: Double -> Double -> C.Render () -> C.Render ()
unitRendering width height rendering = do
    C.save

    C.scale (width/2) (height/2)
    C.translate 1 1
    --C.rotate (-pi/2)
    C.setLineWidth (1/((width+height)/2))

    unitBlank

    rendering

    C.restore


renderToPNG filename rendering surface = do
    C.renderWith surface rendering
    C.surfaceWriteToPNG surface filename


unitDomain = Domain (Point (-1) (-1)) (Point 1 1)


completeQuadTree :: Int -> Domain -> QuadTree Int
completeQuadTree 0 outer_domain = QuadEmpty outer_domain
completeQuadTree n outer_domain = QuadTree outer_domain (completeQuadTree (n-1) domain_ur) (completeQuadTree (n-1) domain_ul) (completeQuadTree (n-1) domain_ll) (completeQuadTree (n-1) domain_lr)
    where (domain_ur, domain_ul, domain_ll, domain_lr) = subdivide outer_domain


renderUnitPNG :: (Renderable a) => FilePath -> a -> IO ()
renderUnitPNG filename rendering = C.withImageSurface C.FormatARGB32 renderingWidth renderingHeight (renderToPNG filename $ unitRendering (fromIntegral renderingWidth) (fromIntegral renderingHeight) $ render rendering)

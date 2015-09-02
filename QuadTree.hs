module QuadTree
( Point(..)
, Domain(..)
, quadTree
, unitDomain
, randomPoints
, renderQuad
, count
, depth
, meanDepth
, volume
) where

import Control.Monad (replicateM)
import Control.Applicative ((<$>), (<*>))
import System.Random

import qualified Graphics.Rendering.Cairo as C

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
    deriving (Show)
data Domain = Domain Point Point
    deriving (Show)


data QuadTree v =
    QuadTree Domain (QuadTree v) (QuadTree v) (QuadTree v) (QuadTree v)
    | QuadLeaf Domain Point v
    | QuadEmpty Domain
    deriving Show


subdivide :: Domain -> (Domain, Domain, Domain, Domain)
subdivide (Domain (Point xA yA) (Point xB yB)) = (Domain e g, Domain b d, Domain c e, Domain f h)
    where [_, b, c, d, e, f, g, h, _] = Point <$> [xA, ((xA+xB)/2), xB] <*> [yB, ((yA+yB)/2), yA]


inside :: Point -> Domain -> Bool
(Point x y) `inside` (Domain (Point xA yA) (Point xB yB)) = (inside1D x xA xB) && (inside1D y yA yB)
    where inside1D w wA wB = wA <= w && w < wB


quadTree :: Domain -> [(Point, a)] -> QuadTree a
quadTree outer_domain [] = QuadEmpty outer_domain
quadTree outer_domain [(point, value)] = QuadLeaf outer_domain point value
quadTree outer_domain pvs = QuadTree outer_domain (quadTree domain_ur pvs_ur) (quadTree domain_ul pvs_ul) (quadTree domain_ll pvs_ll) (quadTree domain_lr pvs_lr)
    where (domain_ur, domain_ul, domain_ll, domain_lr) = subdivide outer_domain
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
        C.arc x y (3*width) 0 (2*pi)
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
randomU01Sequence length = replicateM length (randomIO :: IO Double)


randomPoints :: Domain -> Int -> IO [Point]
randomPoints (Domain (Point xa ya) (Point xb yb)) length = do
    xs <- randomU01Sequence length
    ys <- randomU01Sequence length
    return $ zipWith Point (fillW xa xb xs) (fillW ya yb ys)
        where fillW wa wb = map ((+wa).((wb-wa)*))


renderingWidth :: Int
renderingWidth = 2048
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


levelTree :: Int -> Domain -> QuadTree Int
levelTree 0 outer_domain = QuadEmpty outer_domain
levelTree n outer_domain = QuadTree outer_domain (levelTree (n-1) domain_ur) (levelTree (n-1) domain_ul) (levelTree (n-1) domain_ll) (levelTree (n-1) domain_lr)
    where (domain_ur, domain_ul, domain_ll, domain_lr) = subdivide outer_domain


renderQuad filename quad = C.withImageSurface C.FormatARGB32 renderingWidth renderingHeight (renderToPNG filename $ unitRendering (fromIntegral renderingWidth) (fromIntegral renderingHeight) $ render quad)

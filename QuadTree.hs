module QuadTree
( Point
, Domain
, quadTree
) where

import Control.Monad (replicateM)
import Control.Applicative ((<$>), (<*>))
import System.Random

import Graphics.Rendering.Cairo


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


class Renderable a where
    render :: a -> Render ()


instance Renderable Domain where
    render (Domain (Point xa ya) (Point xb yb)) = do
        save

        setSourceRGBA 0 0 0 0.8
        rectangle xa ya (xb-xa) (yb-ya)
        stroke

        restore


instance Renderable Point where
    render (Point x y) = do
        save
        width <- getLineWidth

        setSourceRGBA 0.5 0.5 0.5 0.5
        arc x y (3*width) 0 (2*pi)
        fill

        restore


instance Renderable (QuadTree a) where
    render (QuadTree domain@(Domain pa@(Point xa ya) pb@(Point xb yb)) ur ul ll lr) = do
        save
        width <- getLineWidth

        setSourceRGBA 0 0 0 1

        let c@(Point xc yc) = center domain
        moveTo xc yc
        lineTo xc ya
        stroke
        moveTo xc yc
        lineTo xc yb
        stroke
        moveTo xc yc
        lineTo xa yc
        stroke
        moveTo xc yc
        lineTo xb yc
        stroke

        render [ur, ul, ll, lr]

        restore

    render (QuadLeaf domain@(Domain (Point xa ya) (Point xb yb)) point _) = do
        save
        width <- getLineWidth

        setSourceRGBA 0 1 0 0.05
        rectangle xa ya (xb-xa) (yb-ya)
        fill

        render point

        restore

    render (QuadEmpty (Domain (Point xa ya) (Point xb yb))) = do
        save

        setSourceRGBA 1 0 0 0.05
        rectangle xa ya (xb-xa) (yb-ya)
        fill

        restore


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


testCircle :: Render ()
testCircle = do
    save
    setSourceRGBA 0 0 0 0.8
    arc 0 0 0.25 0 (2*pi)
    stroke
    arc 0 0 0.5 0 (2*pi)
    stroke
    arc 0 0 0.75 0 (2*pi)
    stroke
    arc 0 0 1 0 (2*pi)
    stroke
    restore


unitBlank :: Render ()
unitBlank = do
    save

    setSourceRGBA 1 1 1 1
    rectangle (-1) (-1) 2 2
    fill

    restore


unitRendering :: Double -> Double -> Render () -> Render ()
unitRendering width height rendering = do
    save

    scale (width/2) (height/2)
    translate 1 1
    setLineWidth (1/((width+height)/2))

    unitBlank

    rendering

    restore


renderToPNG rendering surface = do
    renderWith surface rendering
    surfaceWriteToPNG surface "output.png"


unitDomain = Domain (Point (-1) (-1)) (Point 1 1)


levelTree :: Int -> Domain -> QuadTree Int
levelTree 0 outer_domain = QuadEmpty outer_domain
levelTree n outer_domain = QuadTree outer_domain (levelTree (n-1) domain_ur) (levelTree (n-1) domain_ul) (levelTree (n-1) domain_ll) (levelTree (n-1) domain_lr)
    where (domain_ur, domain_ul, domain_ll, domain_lr) = subdivide outer_domain

main = do
    --let rendering = render $ QuadTree (Domain (Point (-1) (-1)) (Point 1 1)) (QuadTree (Domain (Point 0 0) (Point 1 1)) (QuadLeaf (Point 0.75 0.75) 5) QuadEmpty QuadEmpty QuadEmpty) QuadEmpty QuadEmpty QuadEmpty

    points <- randomPoints unitDomain (1*1024)

    let pointValues = zip points $ repeat (0::Int)
    let quad = quadTree unitDomain pointValues

    --let rendering = render quad
--    let tree = levelTree 4 unitDomain
    let rendering = render quad
--    let rendering = render points

--    let (a,b,c,d) = subdivide (Domain (Point (-1) (-1)) (Point 1 1))
--    mapM_ print [a,b,c,d]


    withImageSurface FormatARGB32 renderingWidth renderingHeight (renderToPNG $ unitRendering (fromIntegral renderingWidth) (fromIntegral renderingHeight) rendering)

import qualified System.Environment as Env

import qualified Graphics.Rendering.Cairo as C

import qualified QuadTree as QT


data Query a = Query Double QT.Point (QT.QuadTree a)
    deriving (Show)


instance QT.Renderable (Query a) where
    render (Query r p@(QT.Point x y) tree) = do
        C.save
        QT.render tree

        C.setSourceRGBA 0 0 0 0.9
        C.arc x y r 0 (2*pi)
        C.stroke

        C.restore


data Result a = Result Double QT.Point [(QT.Point, a)]
    deriving (Show)

instance QT.Renderable (Result a) where
    render (Result r p@(QT.Point x y) ps) = do
    C.save
    QT.render $ map fst ps

    C.setSourceRGBA 0 0 0 0.9
    C.arc x y r 0 (2*pi)
    C.stroke

    C.restore


main :: IO ()
main = do
    (count':x':y':r':_) <- Env.getArgs
    let count = (read count') :: Int
    let p = QT.Point (read x') (read y')
    let r = (read r') :: Double

    quadtree <- QT.randomQuadTree QT.unitDomain count
    let nearquadtree = QT.nearQuadTree r p quadtree
    let nearresult = QT.near r p quadtree

    QT.renderUnitPNG ("output/random." ++ show count ++ ".png") quadtree
    QT.renderUnitPNG ("output/random." ++ show count ++ ".debug.png") $ Query r p nearquadtree
    QT.renderUnitPNG ("output/random." ++ show count ++ ".result.png") $ Result r p nearresult

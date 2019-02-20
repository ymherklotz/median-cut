module Main where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.List            (transpose)
import qualified Data.Text            as T
import           PFM

data Direction = Horizontal | Vertical
               deriving (Show, Eq)

data Cut = Cut Direction Int
         deriving (Show, Eq)

clamp :: PFMColour -> PPMColour
clamp (PFMColour ri gi bi) = PPMColour (f ri) (f gi) (f bi)
  where
    v s = s * 255.0
    f s = if v s > 255.0 then 255 else fromInteger (floor (v s))
clamp _ = undefined

clampImage :: PFMImage -> PPMImage
clampImage (PFMImage w h c) = PPMImage w h $ fmap clamp <$> c

fixIntensity :: Int -> Int -> PFMColour -> Double
fixIntensity sizeY y (PFMColour r g b) =
    sin (fromIntegral y / fromIntegral sizeY * pi) * (f r + f g + f b) / 3
    where f = realToFrac
fixIntensity _ _ _ = error "Mono not supported"

findSplit :: [Double] -> Int
findSplit d = (+ 1) . floor . (/ sum d) . sum $ zipWith (*) [0 ..] d

cfmap :: Functor f => (t -> a -> b) -> t -> f a -> f b
cfmap f a b = f a <$> b

findSplitLine' :: [[Double]] -> Int
findSplitLine' = findSplit . fmap sum

findSplitLine :: [[Double]] -> Cut
findSplitLine d | length d > length (head d) = Cut Horizontal $ findSplitLine' d
                | otherwise                  = Cut Vertical . findSplitLine' $ transpose d

drawCut' :: PFMColour -> Cut -> (Int, Int) -> PFMColour -> PFMColour
drawCut' c (Cut Vertical n) (_, x) c' | x == n    = c
                                      | otherwise = c'
drawCut' c (Cut Horizontal n) (y, _) c' | y == n    = c
                                        | otherwise = c'

drawCut :: PFMColour -> Cut -> [[PFMColour]] -> [[PFMColour]]
drawCut c cut colour = (zipWith . zipWith)
    (drawCut' c cut)
    [ [ (y, x) | x <- [0 .. length $ head colour] ] | y <- [0 .. length colour] ]
    colour

applyGamma :: Float -> PFMImage -> PFMImage
applyGamma g (PFMImage w h c) = PFMImage w h $ fmap gc <$> c
    where gc (PFMColour r gr b) = PFMColour (gamma g r) (gamma g gr) (gamma g b)

main :: IO ()
main = do
    im <- B.readFile "data/grace_latlong.pfm"
    let grace            = revColour $ parse im
    let height           = pfmHeight grace - 1
    let fixedIntensities = zipWith (cfmap $ fixIntensity height) [0 ..] $ pfmColour grace
    let cut              = findSplitLine fixedIntensities
    let newColour        = drawCut (PFMColour 0 1 0) cut $ pfmColour grace
    print $ length newColour
    print cut
    BL.writeFile "data/final.ppm" . encodePPM . clampImage . applyGamma 2.2 $ PFMImage
        (pfmWidth grace)
        (pfmHeight grace)
        newColour

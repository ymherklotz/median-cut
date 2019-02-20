module Main where

import qualified Data.ByteString as B
import           Data.List       (transpose)
import qualified Data.Text       as T
import           PFM

data Direction = Horizontal | Vertical
               deriving (Show, Eq)

data Cut = Cut Direction Int
         deriving (Show, Eq)

clamp :: PFMColour -> PPMColour
clamp (PFMColour ri gi bi) =
  PPMColour (f ri) (f gi) (f bi)
  where
    v s = s * 255.0
    f s = if v s > 255.0 then 255 else fromInteger (floor (v s))
clamp _ = undefined

clampImage :: PFMImage -> PPMImage
clampImage (PFMImage w h c) =
  PPMImage w h $ fmap clamp <$> c

fixIntensity :: Int -> Int -> PFMColour -> Double
fixIntensity sizeY y (PFMColour r g b) =
    sin (fromIntegral y / fromIntegral sizeY * pi) * (f r + f g + f b) / 3
    where
        f = realToFrac
fixIntensity _ _ _ = error "Mono not supported"

findSplit :: [Double] -> Int
findSplit d =
    (+1) . floor . (/sum d) . sum $ zipWith (*) [0..] d

cfmap :: Functor f => (t -> a -> b) -> t -> f a -> f b
cfmap f a b = f a <$> b

findSplitLine' :: [[Double]] -> Int
findSplitLine' = findSplit . fmap sum

findSplitLine :: [[Double]] -> Cut
findSplitLine d
    | length d > length (head d) = Cut Horizontal $ findSplitLine' d
    | otherwise = Cut Vertical . findSplitLine' $ transpose d

drawCut' :: PFMColour -> Cut -> (Int, Int) -> PFMColour -> PFMColour
drawCut' c (Cut Vertical n) (x, _) c'
    | x == n || x == n - 1 = c
    | otherwise = c'
drawCut' c (Cut Horizontal n) (_, y) c'
    | y == n || y == n - 1 = c
    | otherwise = c'

drawCut :: PFMColour -> Cut -> [[PFMColour]] -> [[PFMColour]]
drawCut c cut colour =
    

main :: IO ()
main = do
    im <- B.readFile "data/grace_latlong.pfm"
    let grace = revColour $ parse im
    let height = pfmHeight grace - 1
    let fixedIntensities = zipWith (cfmap $ fixIntensity height) [0..] $ pfmColour grace
    print . findSplitLine $ fixedIntensities

module Main where

import           Data.Bifunctor       (Bifunctor, bimap)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.List            (foldl', transpose)
import qualified Data.Text            as T
import           Debug.Trace
import           PFM

data Direction = Horizontal | Vertical
               deriving (Show, Eq)

data Cut = Cut Direction Int
         deriving (Show, Eq)

bbimap :: Bifunctor p => (a -> d) -> p a a -> p d d
bbimap a = bimap a a

clamp :: PFMColour -> PPMColour
clamp (PFMColour ri gi bi) = PPMColour (f ri) (f gi) (f bi)
  where
    v s = s * 255
    f s = if v s > 255 then 255 else fromInteger (floor (v s))
clamp _ = undefined

clampImage :: PFMImage -> PPMImage
clampImage (PFMImage w h c) = PPMImage w h $ fmap clamp <$> c

fixIntensity :: Int -> Int -> PFMColour -> Double
fixIntensity sizeY y (PFMColour r g b) =
    sin ((fromIntegral y / fromIntegral sizeY) * pi) * (f r + f g + f b) / 3
    where f = realToFrac
fixIntensity _ _ _ = error "Mono not supported"

findSplit_ :: [Double] -> Int
findSplit_ d = findSplit'_ d [] 0
  where
    findSplit'_ (x : ds) e i | sum ds < sum e = i
                             | otherwise      = findSplit'_ ds (x : e) $ i + 1
    findSplit'_ _ _ i = i

findSplit :: [Double] -> Int
findSplit = round . findSplit'

findSplit' :: [Double] -> Double
findSplit' d = (/ sum d) . sum $ zipWith (*) [1 ..] d

findEnergy :: [Double] -> (Double, Double)
findEnergy d = bbimap sum $ splitAt (findSplit d) d

cfmap :: Functor f => (t -> a -> b) -> t -> f a -> f b
cfmap f a b = f a <$> b

findSplitLine' :: [[Double]] -> Int
findSplitLine' = findSplit_ . fmap sum

findSplitLine :: [[Double]] -> Cut
findSplitLine d | length d > length (head d) = Cut Horizontal $ findSplitLine' d
                | otherwise                  = Cut Vertical . findSplitLine' $ transpose d

energies :: [[Double]] -> ((Double, Double), [Double])
energies d | length d > length (head d) = (bs . splitAt (findSplitLine' d) $ fmap sum d, fmap sum d)
           | otherwise = (bs . splitAt (findSplitLine' d') $ fmap sum d', fmap sum d')
  where
    bs = bbimap sum
    d' = transpose d

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

split :: Cut -> [[a]] -> ([[a]], [[a]])
split (Cut Horizontal n) l = splitAt n l
split (Cut Vertical   n) l = bbimap transpose $ splitAt n l' where l' = transpose l

combine :: Cut -> ([[a]], [[a]]) -> [[a]]
combine (Cut Horizontal _) (a, b) = a ++ b
combine (Cut Vertical   _) (a, b) = zipWith (++) a b

fIntens :: [[PFMColour]] -> [[Double]]
fIntens d = zipWith (cfmap . fixIntensity $ length d - 1) [0 ..] d

findCentroid :: [[Double]] -> (Int, Int)
findCentroid d = (y, x)
    where
        y = findSplitLine' d
        x = findSplitLine' $ transpose d

drawCentroid' :: PFMColour -> Int -> (Int, Int) -> (Int, Int) -> PFMColour -> PFMColour
drawCentroid' ci s (xi, yi) (x, y) c
    | 2 * abs (x - xi) < s && 2 * abs (y - yi) < s = ci
    | otherwise = c

drawCentroid :: PFMColour -> [[Double]] -> [[PFMColour]] -> [[PFMColour]]
drawCentroid ic d c = (zipWith . zipWith) (drawCentroid' ic 5 pt)
    [ [ (y, x) | x <- [0 .. length $ head c] ] | y <- [0 .. length c] ]
    c
    where
        pt = findCentroid d

drawCentroidBlack' :: PFMColour -> Int -> (Int, Int) -> (Int, Int) -> PFMColour -> PFMColour
drawCentroidBlack' ci s (xi, yi) (x, y) _
    | 2 * abs (x - xi) < s && 2 * abs (y - yi) < s = ci
    | otherwise = PFMColour 0 0 0

drawCentroidBlack :: PFMColour -> [[Double]] -> [[PFMColour]] -> [[PFMColour]]
drawCentroidBlack ic d c = (zipWith . zipWith) (drawCentroidBlack' ic 5 pt)
    [ [ (y, x) | x <- [0 .. length $ head c] ] | y <- [0 .. length c] ]
    c
    where
        pt = findCentroid d

add :: PFMColour -> PFMColour -> PFMColour
add (PFMColour r1 g1 b1) (PFMColour r2 g2 b2) =
    PFMColour (r1+r2) (g1+g2) (b1+b2)
add _ _ = error "Mono not supported"

totalRadiance :: [[PFMColour]] -> PFMColour
totalRadiance c = f $ f <$> c
     where
         f = foldl' add (PFMColour 0 0 0)

recSplit :: Int -> [[Double]] -> [[PFMColour]] -> [[PFMColour]]
recSplit 0 d c = drawCentroid (PFMColour 0 0 1) d c
recSplit n d c = drawCut (PFMColour 1 1 1) cut a
  where
    cut  = findSplitLine d
    a    = combine cut . apply nrec $ split cut c
    (d1, d2) = split cut d
    nrec = bbimap (recSplit (n - 1)) (d1, d2)
    apply (f, g) (a', c') = (f a', g c')

norm :: PFMColour -> Double
norm (PFMColour r g b) = (f r + f g + f b) / 3
    where f = realToFrac

recSplitRadiance :: Int -> [[Double]] -> [[PFMColour]] -> [[PFMColour]]
recSplitRadiance 0 d c = drawCentroidBlack (totalRadiance c) d c
recSplitRadiance n d c = a
  where
    cut  = findSplitLine d
    a    = combine cut . apply nrec $ split cut c
    (d1, d2) = split cut d
    nrec = bbimap (recSplitRadiance (n - 1)) (d1, d2)
    apply (f, g) (a', c') = (f a', g c')

main :: IO ()
main = do
    im <- B.readFile "data/grace_latlong.pfm"
    let grace = revColour $ parse im
    mapM_
        (\i ->
            let newColour = recSplit i (fIntens $ pfmColour grace) $ pfmColour grace
                img = PFMImage (pfmWidth grace) (pfmHeight grace) newColour
            in  do
                    putStrLn $ "data/mc_" ++ show i ++ ".ppm"
                    BL.writeFile ("data/mc_" ++ show i ++ ".ppm")
                        . encodePPM
                        . clampImage
                        . applyGamma 2.2
                        $ img
                    putStrLn $ "data/mc_" ++ show i ++ ".pfm"
                    BL.writeFile ("data/mc_" ++ show i ++ ".pfm")
                        . encode
                        . revColour
                        $ img
        )
        [1 .. 10]
    mapM_
        (\i ->
            let newColour = recSplitRadiance i (fIntens $ pfmColour grace) $ pfmColour grace
                img = PFMImage (pfmWidth grace) (pfmHeight grace) newColour
            in  do
                    putStrLn $ "data/mc_rad_" ++ show i ++ ".ppm"
                    BL.writeFile ("data/mc_rad_" ++ show i ++ ".ppm")
                        . encodePPM
                        . clampImage
                        . applyGamma 2.2
                        $ img
                    putStrLn $ "data/mc_rad_" ++ show i ++ ".pfm"
                    BL.writeFile ("data/mc_rad_" ++ show i ++ ".pfm")
                        . encode
                        . revColour
                        $ img
        )
        [6]

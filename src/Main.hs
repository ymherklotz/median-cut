module Main where

import qualified Data.ByteString as B
import qualified Data.Text       as T
import           PFM

fixIntensity :: Int -> Int -> PFMColour -> Double
fixIntensity sizeY y (PFMColour r g b) =
    sin (fromIntegral y / fromIntegral sizeY * pi) * (f r + f g + f b) / 3
    where
        f = realToFrac
fixIntensity _ _ _ = error "Mono not supported"

findSplit :: [Double] -> Int
findSplit = 

main :: IO ()
main = do
    im <- B.readFile "data/grace_latlong.pfm"
    let grace = revColour $ parse im
    let height = pfmHeight grace - 1
    let fixedIntensities = [fixIntensity height y <$> c | c <- pfmColour grace, y <- [0..height]]
    

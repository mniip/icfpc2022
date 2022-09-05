import Codec.Picture
import Codec.Picture.Types
import Control.Lens
import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.List ((\\), nub, group, groupBy, sort, sortBy, minimumBy)
import Data.Function (on)
import Control.Monad.State
import Data.Text.IO qualified as T
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment
import Text.Read

import ICFPC.Graph
import ICFPC.ISL qualified as I
import ICFPC.Render
import ICFPC.Cost
import ICFPC.Pairs

main :: IO ()
main = do
  getArgs >>= \case
    [input] -> readImage input >>= \case
      Left err -> error err
      Right (ImageRGBA8 image') -> do
          grid <- readGrid `fmap` getContents
          let swaps = keepImproving image' grid
              grid' = foldl swapInGrid grid swaps
              n = length $ concat grid
          putStr $ printSwaps (optimizeSwaps swaps n)
          putStr $ printColors (recolor image' grid')
          -- savePngImage "grid.png" (ImageRGBA8 $ drawGrid 10 grid)
      Right _ -> error "Invalid pixel format"
    _ -> error "Usage: swapper <input.png>"

-- Block Id and color
type Grid = [[(Int, PixelRGBA8)]]

readGrid :: String -> Grid
readGrid str = [[ unfolded !! (x*len - y - 1) | x <- [1 .. len]] | y <- [0 .. len-1]]
    where topix [a, b, c, d] = PixelRGBA8 a b c d
          unfolded = zip [0..] (map (topix . read) $ lines str)
          len2 = length unfolded
          len = round . sqrt $ fromIntegral len2

exampleGrid :: Grid
exampleGrid = [[(2, wh), (5, bl), (8, bl)], [(1, wh), (4, wh), (7, bl)], [(0, bl), (3, bl), (6, wh)]]
    where wh = PixelRGBA8 255 255 255 255
          bl = PixelRGBA8 0 0 0 255

drawGrid :: Int -> Grid -> Image PixelRGBA8
drawGrid size grid = generateImage atXY width height
    where hsqrs = length grid
          wsqrs = length $ head grid
          height = size * hsqrs
          width = size * wsqrs
          atXY x y = let x' = x `div` size
                         y' = y `div` size
                     in snd ((grid !! y') !! x')

dist (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2)
      = sqrt $ (fromIntegral r1 - fromIntegral r2)^2 + (fromIntegral g1 - fromIntegral g2)^2 +
               (fromIntegral b1 - fromIntegral b2)^2 + (fromIntegral a1 - fromIntegral a2)^2

distToCell :: Image PixelRGBA8 -> PixelRGBA8 -> Int -> (Int, Int) -> Int
distToCell image color size (x0, y0) = round . (0.005*) $
                                       sum [dist color (pixelAt image (x0*size + x) (y0*size + y)) | x <- [0..size-1], y <- [0..size-1]]

bestColor :: Image PixelRGBA8 -> Int -> (Int, Int) -> PixelRGBA8
bestColor image size (x0, y0) = median [pixelAt image (x0*size + x) (y0*size + y) | x <- [0..size-1], y <- [0..size-1]]

-- Try recoloring each cell of the grid to match the image.
recolor :: Image PixelRGBA8 -> Grid -> [(Int, PixelRGBA8)]
recolor image grid = map snd . filter (\x -> fst x < 0) $ map try [0..(len*len-1)]
    where width = imageWidth image
          len = length grid
          size = width `div` len
          cost = len*len*5
          try i = let (coord, color) = inGrid grid i
                      color' = bestColor image size coord
                  in (cost + distToCell image color' size coord - distToCell image color size coord, (i, color'))

printColors :: [(Int, PixelRGBA8)] -> String
printColors = unlines . map printColor
    where printColor (i, PixelRGBA8 r g b a) = "color [" ++ show i ++ "] [" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ show a ++ "]"

bakedDists :: Image PixelRGBA8 -> Int -> Grid -> M.Map (Int, (Int, Int)) Int
bakedDists image size grid = M.fromList $ do
    let len = length grid
    x <- [0..len-1]
    y <- [0..len-1]
    (i, color) <- concat grid
    return ((i, (x, y)), distToCell image color size (x, y))

-- Find coordinates and color of block i in the grid
inGrid :: Grid -> Int -> ((Int, Int), PixelRGBA8)
inGrid grid i = if null locs then error "Heh" else snd $ head locs
    where len = length grid
          locs = filter (\(ind, _) -> ind == i) . concat $
                 zipWith (\y xs -> zipWith (\x (ind, c) -> (ind, ((x, y), c))) [0..] xs) [0 .. len - 1] grid

type Swap = (Int, Int)

-- Given indices of blocks, swap them in the grid
swapInGrid :: Grid -> Swap -> Grid
swapInGrid grid (i1, i2) = map (map switch) grid
    where flat = concat grid
          first = head $ filter (\(i, _) -> i == i1) flat
          second = head $ filter (\(i, _) -> i == i2) flat
          switch c | c == first = second
                   | c == second = first
                   | otherwise = c

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

-- Transpositions of different color.
saneTransp :: Grid -> [Swap]
saneTransp grid = concat [ [(x, y) | x <- c1, y <- c2] | (c1, c2) <- pairs classes]
    where classes = map (map fst) . groupBy ((==) `on` snd) . sortBy (compare `on` snd) $ concat grid
          len = length classes

-- Takes the image, block/image square distance matrix, grid, allowed transpositions and returns best score, transposition and a new grid.
improveGrid :: Image PixelRGBA8 -> M.Map (Int, (Int, Int)) Int -> Grid -> [(Int, Swap)] -> (Int, Swap, Grid)
improveGrid image baked grid allowed = minimumBy (compare `on` (\(s, _, _) -> s)) grids
                              {- if null suggestions
                                 then (1000000, (0, 0), grid)
                                 else head suggestions -}
    where len = length grid
          size = width `div` len -- we assume images are square
          width = imageWidth image
          height = imageHeight image
          grids = map (\(s, t) -> (s, t, swapInGrid grid t)) allowed

judgeSwaps :: M.Map (Int, (Int, Int)) Int -> Grid -> [Swap] -> [(Int, Swap)]
judgeSwaps baked grid = map (\t -> (gridDist t, t))
    where len = length grid
          cost = len*len*3
          gridDist (i1, i2) = let (crds1, _) = inGrid grid i1
                                  (crds2, _) = inGrid grid i2
                              in baked M.! (i1, crds2) + baked M.! (i2, crds1) - baked M.! (i1, crds1) - baked M.! (i2, crds2) + cost

rejudgeSwaps :: M.Map (Int, (Int, Int)) Int -> Grid -> [(Int, Swap)] -> Swap -> [(Int, Swap)]
rejudgeSwaps baked grid swaps t = map replace swaps
    where len = length grid
          cost = len*len*3
          adjacent (a, b) (c, d) = a == c || a == d || b == c || b == d
          replace (sc, s) | adjacent s t = (gridDist s, s)
                          | otherwise = (sc, s)
          gridDist (i1, i2) = let (crds1, _) = inGrid grid i1
                                  (crds2, _) = inGrid grid i2
                              in baked M.! (i1, crds2) + baked M.! (i2, crds1) - baked M.! (i1, crds1) - baked M.! (i2, crds2) + cost

keepImproving :: Image PixelRGBA8 -> Grid -> [Swap]
keepImproving image grid = go grid (judgeSwaps baked grid $ saneTransp grid) 
    where oldDist = compareImages image (drawGrid size grid)
          width = imageWidth image
          size = width `div` (length grid)
          baked = bakedDists image size grid
          go g allowed = let (s, t, g') = improveGrid image baked g allowed
                             allowed' = rejudgeSwaps baked g' (filter (\(_, t') -> t /= t') allowed) t
                         in if s < 0 then t : go g' allowed' else []

printSwaps :: [Swap] -> String
printSwaps swaps = unlines $ map printSwap swaps
    where printSwap (a, b) = "swap [" ++ show a ++ "] [" ++ show b ++ "]"

applyPerms :: [Swap] -> Int -> Int
applyPerms perms i = foldl switch i perms
    where switch x (a, b) | a == x = b
                          | b == x = a
                          | otherwise = x

orbit :: [Swap] -> Int -> [Int]
orbit perms i = reverse $ go [] i
    where go orb x = let x' = applyPerms perms x
                     in if x' `elem` orb then orb else go (x' : orb) x'

orbits :: [Swap] -> Int -> [[Int]]
orbits perms n = go 0 [0..n-1]
    where go x free = let orb = orbit perms x
                          free' = free \\ orb
                      in if null free then [] else orb : go (head free') free'

toTrans :: [[Int]] -> [Swap]
toTrans = concatMap cyc
    where cyc ls = reverse $ prs ls
          prs [] = []
          prs [x] = []
          prs (x:y:xs) = (x, y) : prs (y:xs)

optimizeSwaps :: [Swap] -> Int -> [(Int, Int)]
optimizeSwaps swaps n = toTrans $ orbits swaps n

median :: [PixelRGBA8] -> PixelRGBA8
median [x] = x
median xs = fromTuple $ go 100 (sumDist average) average
  where
    toTuple :: PixelRGBA8 -> (Int, Int, Int, Int)
    toTuple (PixelRGBA8 r g b a) = (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)
    fromTuple (r, g, b, a) = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
    xs' = map toTuple xs
    sumDist x = sum $ dist x <$> xs'
    dist (r1, g1, b1, a1) (r2, g2, b2, a2) = sqrt . fromIntegral $ (r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2 + (a1 - a2)^2 :: Float
    (r1, g1, b1, a1) .+. (r2, g2, b2, a2) = (r1+r2, g1+g2, b1+b2, a1+a2)
    len = length xs
    average = let (r, g, b, a) = foldr1 (.+.) xs' in (r `div` len, g `div` len, b `div` len, a `div` len)
    dirs = [(r, g, b, a) | r <- [0, -1, 1], g <- [0, -1, 1], b <- [0, -1, 1], a <- [0, -1, 1],
                           r^2 + g^2 + b^2 + a^2 <= 1, r^2 + g^2 + b^2 + a^2 /= 0]
    inside (r, g, b, a) = 0 <= r && r <= 255 && 0 <= g && g <= 255 && 0 <= b && b <= 255 && 0 <= a && a <= 255
    go step oldDist pix =
      let nbrs = filter inside $ map (\(r, g, b, a) -> pix .+. (step*r, step*g, step*b, step*a)) dirs
          dists = map (\n -> (sumDist n, n)) nbrs
          (best, bestPix) = minimumBy (compare `on` fst) dists
      in if best < oldDist
         then go step best bestPix
         else if step > 1 then go ((step+1) `div` 2) oldDist pix else pix

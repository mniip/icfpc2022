import Codec.Picture
import Codec.Picture.Types
import Control.Lens
import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.List (group, sort, sortBy, minimumBy)
import Data.Function (on)
import Control.Monad.State
import Data.Text.IO qualified as T
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
    [sStep, input] | Just step <- readMaybe sStep, step >= 0 -> readImage input >>= \case
      Left err -> error err
      Right (ImageRGBA8 image') -> if step == 0 then T.putStrLn . I.printProgram . toISL $ stepSearch image'
                                                else let Just (g, _) = drawWithStep step image'
                                                     in T.putStrLn . I.printProgram $ toISL g
      Right _ -> error "Invalid pixel format"
    _ -> error "Usage: pixel <N> <input.png>"

-- May fail
stepSearch :: Image PixelRGBA8 -> Graph
stepSearch image = fst $ minimumBy (compare `on` snd) $ mapMaybe (\s -> drawWithStep s image) [10, 20..((min width height) - 10)]
    where width = imageWidth image
          height = imageHeight image

-- Colors with occurencies, outputs popular colors first.
colorStats :: Image PixelRGBA8 -> [(Int, PixelRGBA8)]
colorStats image = reverse . sortBy (compare `on` fst) . map (\g -> (length g, head g)) . group $
                   sort [pixelAt image x y | x <- [0 .. width-1], y <- [0 .. height-1]]
    where width = imageWidth image
          height = imageHeight image

-- boundingLine [T, T, T, F, F, T, F, T, T] == (3, 6)
--                        ^  ^  ^  ^
boundingLine :: [Bool] -> (Int, Int)
boundingLine ls = let left = length $ takeWhile id ls
                      right = length . takeWhile id $ reverse ls
                  in (left, length ls - right - 1)

-- Find bounding box with a given background color.
boundingBoxChroma :: Image PixelRGBA8 -> PixelRGBA8 -> ((Int, Int), (Int, Int))
boundingBoxChroma image color = ((left, down), (right, up))
    where width = imageWidth image
          height = imageHeight image
          horLine y = [pixelAt image x y | x <- [0 .. width-1]]
          verLine x = [pixelAt image x y | y <- [0 .. height-1]]
          align = foldl1 (\(a, b) (c, d) -> (min a c, max b d))
          (left, right) = align $ map (boundingLine . map (== color) . horLine) [0 .. height-1]
          (down, up) = align $ map (boundingLine . map (== color) . verLine) [0 .. width-1]

-- Outputs the best bounding box and the respective background color.
boundingBox :: Image PixelRGBA8 -> (((Int, Int), (Int, Int)), PixelRGBA8)
boundingBox image = minimumBy (compare `on` area) boxes
    where popularColors = map snd . take 10 $ colorStats image
          boxes = map (\c -> (boundingBoxChroma image c, c)) popularColors
          area (((l, d), (r, u)), _) = (r-l)*(u-d)

-- "Pixelate" the image with a given step, return the command graph and new score.
drawWithStep :: Int -> Image PixelRGBA8 -> Maybe (Graph, Int)
drawWithStep step image' = do
  let
    image = vflippedImage image'
    width = imageWidth image
    height = imageHeight image
    (((left, down), (right, up)), background) = boundingBox image

    tryCost g = case runRender (XY width height) $ runCostT (runTrace g (XY width height)) of
                  (((Nothing, _), Sum cost), image'') -> Just $ (g, cost + compareImages image' image'')
                  (((Just e, _), _), _) -> Nothing

    fresh = _2 <+= 1
    node n = modify $ _1 %~ addNode n
    stepX0 = let s = width `mod` step in if s == 0 then step else s
    stepY0 = let s = height `mod` step in if s == 0 then step else s
    graph = fst $ execState (start 0) (emptyGraph, 0)

    cutDownMargin m
      | down == 0 = return m
      | otherwise = do
          r <- fresh
          m' <- fresh
          node $ YCut m down r m'
          return m'
    cutLeftMargin m
      | left == 0 = return m
      | otherwise = do
          r <- fresh
          m' <- fresh
          node $ XCut m left r m'
          return m'
    -- Start drawing rectangles
    start m = do
        m1 <- fresh
        node $ Color m (packPixel background) m1 -- Fill the background color in case it's not white
        m2 <- cutDownMargin m1
        m3 <- cutLeftMargin m2
        goRow stepX0 step left down [] m3
    -- Draw a rectangle with width stepX
    goRow stepX stepY x y rs m
      | y > up = do
        m' <- fresh
        node $ Color m (packPixel background) m'
      | x + stepX > right = do
        m' <- fresh
        let rgba = median $ [pixelAt image x' y' | y' <- [y .. min (height - 1) (y + stepY - 1)],
                                                   x' <- [x .. min (width - 1) (x + stepX - 1)]]
        node $ Color m (packPixel rgba) m'
        joinRow stepY y rs m'
      | otherwise = do
        m' <- fresh
        let rgba = median $ [pixelAt image x' y' | y' <- [y .. min (height - 1) (y + stepY - 1)],
                                                   x' <- [x .. min (width - 1) (x + stepX - 1)]]
        node $ Color m (packPixel rgba) m'
        r' <- fresh
        m'' <- fresh
        node $ XCut m' (x + stepX) r' m''
        goRow step stepY (x + stepX) y (r':rs) m''
    -- Merge rectangles in the row
    joinRow stepY y (r:rs) m
      | y + stepY >= height = pure ()
      | otherwise = do
        m' <- fresh
        node $ Merge r m m'
        joinRow stepY y rs m'
    -- Finish drawing a row of rectangles
    joinRow stepY y [] m = do
      r <- fresh
      m' <- fresh
      node $ YCut m (y + stepY) r m'
      goRow stepX0 step left (y + stepY) [] m'

  tryCost graph

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

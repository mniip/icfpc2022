import Codec.Picture
import Control.Lens
import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
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

-- "Pixelate" the image with a given step, return the command graph and new score.
drawWithStep :: Int -> Image PixelRGBA8 -> Maybe (Graph, Int)
drawWithStep step image' = do
  let
    image = vflippedImage image'
    width = imageWidth image
    height = imageHeight image

    tryCost g = case runRender (XY width height) $ runCostT (runTrace g (XY width height)) of
                  (((Nothing, _), Sum cost), image'') -> Just $ (g, cost + compareImages image' image'')
                  (((Just e, _), _), _) -> Nothing

    fresh = _2 <+= 1
    node n = modify $ _1 %~ addNode n
    graph = fst $ execState (goRow 0 0 [] 0) (emptyGraph, 0)

    goRow x y rs m
      | x + step >= width = do
        m' <- fresh
        let PixelRGBA8 r g b a = median $ [pixelAt image x' y' | y' <- [y .. min (height - 1) (y + step - 1)],
                                                                 x' <- [x .. min (width - 1) (x + step - 1)]]
        node $ Color m r g b a m'
        joinRow y rs m'
      | otherwise = do
        m' <- fresh
        let PixelRGBA8 r g b a = median $ [pixelAt image x' y' | y' <- [y .. min (height - 1) (y + step - 1)],
                                                                 x' <- [x .. min (width - 1) (x + step - 1)]]
        node $ Color m r g b a m'
        r' <- fresh
        m'' <- fresh
        node $ XCut m' (x + step) r' m''
        goRow (x + step) y (r':rs) m''
    joinRow y (r:rs) m
      | y + step >= height = pure ()
      | otherwise = do
        m' <- fresh
        node $ Merge r m m'
        joinRow y rs m'
    joinRow y [] m = do
      r <- fresh
      m' <- fresh
      node $ YCut m (y + step) r m'
      goRow 0 (y + step) [] m'

  tryCost graph

median :: [PixelRGBA8] -> PixelRGBA8
median [x] = x
median xs = fromTuple $ go (sumDist average) average
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
    index ls a = fst . head . filter (\b -> snd b == a) $ zip [0..] ls
    go oldDist pix =
      let nbrs = map (pix .+.) dirs
          dists = map sumDist nbrs
          best = minimum dists
          i = index dists best
          bestPix = nbrs !! i
      in if best < oldDist then go best bestPix else pix

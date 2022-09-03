import Codec.Picture
import Control.Lens
import Control.Monad.State
import Data.Text.IO qualified as T
import System.Environment
import Text.Read

import ICFPC.Graph
import ICFPC.ISL qualified as I
import ICFPC.Render

main :: IO ()
main = do
  getArgs >>= \case
    [sStep, input] | Just step <- readMaybe sStep, step >= 1 -> readImage input >>= \case
      Left err -> error err
      Right (ImageRGBA8 image') -> do
        let
          image = vflippedImage image'
          width = imageWidth image
          height = imageHeight image

          fresh = _2 <+= 1
          node n = modify $ _1 %~ addNode n
          graph = fst $ execState (goRow 0 0 [] 0) (emptyGraph, 0)

          goRow x y rs m
            | x + step >= width = do
              m' <- fresh
              let PixelRGBA8 r g b a = median $ [pixelAt image x' y' | y' <- [y .. min (height - 1) (y + step - 1)], x' <- [x .. min (width - 1) (x + step - 1)]]
              node $ Color m r g b a m'
              joinRow y rs m'
            | otherwise = do
              m' <- fresh
              let PixelRGBA8 r g b a = median $ [pixelAt image x' y' | y' <- [y .. min (height - 1) (y + step - 1)], x' <- [x .. min (width - 1) (x + step - 1)]]
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

        T.putStrLn $ I.printProgram $ toISL graph
      Right _ -> error "Invalid pixel format"
    _ -> error "Usage: pixel <N> <input.png>"

median :: [PixelRGBA8] -> PixelRGBA8
median xs = go (PixelRGBA8 0 0 0 0)
  where
    sumDist x = sum $ dist x <$> xs
    dist (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2)
      = sqrt $ (fromIntegral r1 - fromIntegral r2)^2 + (fromIntegral g1 - fromIntegral g2)^2 + (fromIntegral b1 - fromIntegral b2)^2 + (fromIntegral a1 - fromIntegral a2)^2 :: Float
    go c0@(PixelRGBA8 r g b a)
      | r < 255 && dr < d0 = go cr
      | g < 255 && dg < d0 = go cg
      | b < 255 && db < d0 = go cb
      | a < 255 && da < d0 = go ca
      | otherwise = c0
      where
        cr = PixelRGBA8 (r + 1) g b a
        cg = PixelRGBA8 r (g + 1) b a
        cb = PixelRGBA8 r g (b + 1) a
        ca = PixelRGBA8 r g b (a + 1)
        d0 = sumDist c0
        dr = sumDist cr
        dg = sumDist cg
        db = sumDist cb
        da = sumDist ca

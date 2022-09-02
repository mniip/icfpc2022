module ICFPC.Cost where

import Codec.Picture
import Control.Monad.Trans.Writer hiding (tell)
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader

import ICFPC.Pairs
import ICFPC.Tracer

newtype CostT m a = CostT { runCostT :: m (a, Sum Int) }
  deriving (Functor, Applicative, Monad, MonadWriter (Sum Int), MonadReader r)
    via (WriterT (Sum Int) m)
  deriving (MonadTrans)
    via (WriterT (Sum Int))

addCost :: MonadReader (XY Int) m => (Int -> Int) -> CostT m ()
addCost f = ask >>= \(XY width height) -> tell $ Sum $ f $ width * height

area :: XY (MinMax Int) -> Int
area = xyArea . fmap mmLength

roundDiv :: Int -> Int -> Int
roundDiv x y = (x + (y `div` 2)) `div` y
infixl 7 `roundDiv`

instance (MonadReader (XY Int) m, MonadCommand m) => MonadCommand (CostT m) where
  onXCut xs ys = do
    addCost $ \ar -> 7 * ar `roundDiv` area (XY (outer xs) ys)
    lift $ onXCut xs ys
  onYCut xs ys = do
    addCost $ \ar -> 7 * ar `roundDiv` area (XY xs (outer ys))
    lift $ onYCut xs ys
  onPCut b@(XY xs ys) = do
    addCost $ \ar -> 10 * ar `roundDiv` area (XY (outer xs) (outer ys))
    lift $ onPCut b
  onColor b rgba = do
    addCost $ \ar -> 5 * ar `roundDiv` area b
    lift $ onColor b rgba
  onSwap b1 b2 = do
    addCost $ \ar -> 3 * ar `roundDiv` area b1
    lift $ onSwap b1 b2
  onXMerge xs ys = do
    addCost $ \ar -> 1 * ar `roundDiv` area (XY (outer xs) ys)
    lift $ onXMerge xs ys
  onYMerge xs ys = do
    addCost $ \ar -> 1 * ar `roundDiv` area (XY xs (outer ys))
    lift $ onYMerge xs ys

compareImages :: Image PixelRGBA8 -> Image PixelRGBA8 -> Int
compareImages img1 img2
  | imageWidth img2 /= width || imageHeight img2 /= height = error "Size mismatch"
  | otherwise = round $ sum
    [ dist (pixelAt img1 x y) (pixelAt img2 x y)
    | y <- [0 .. width - 1]
    , x <- [0 .. height - 1]
    ] * (0.005 :: Double)

  where
    width = imageWidth img1
    height = imageHeight img1
    dist (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2)
      = sqrt $ (fromIntegral r1 - fromIntegral r2)^2 + (fromIntegral g1 - fromIntegral g2)^2 + (fromIntegral b1 - fromIntegral b2)^2 + (fromIntegral a1 - fromIntegral a2)^2
